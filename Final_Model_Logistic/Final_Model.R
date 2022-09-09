library(caTools)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(yardstick)
library(readr)
library(randomForest)
library(caret)
df_clean <- read_csv("df_clean.csv")
df_core_noFB<-df_clean %>% select(Id, Conversion,industry,Category,with_web,P_G,occupation,State_G,
                                  Google_P,Google___of_Reviews,Google_Avg_Rating,Google_diff_week,
                                  Angi_P,Angi_s___of_Reviews,Angi_s_Avg_Rating,Angi_diff_week,
                                  HA_P,HA___of_Reviews,HA_Avg_Rating,HA_diff_week,
                                  Yelp_P,Yelp___of_Reviews,Yelp_Avg_Rating,Yelp_diff_week,Yelp___of_Reviews__that_are_not_recommended_,
                                  IG_P,IG_Follows)

df_RF_noFB<-df_core_noFB
#Removing Outliers and NA
df_RF_noFB[which.max(df_RF_noFB$Google_Avg_Rating),'Google_Avg_Rating']<-4.6
df_RF_noFB[which.max(df_RF_noFB$Google_diff_week),'Google_diff_week']<-8
df_RF_noFB[which.max(df_RF_noFB$Google_diff_week),'Google_diff_week']<-96
df_RF_noFB[which.max(df_RF_noFB$Angi_diff_week),'Angi_diff_week']<-179
df_RF_noFB[which.max(df_RF_noFB$Angi_diff_week),'Angi_diff_week']<-380
df_RF_noFB[which.max(df_RF_noFB$Angi_diff_week),'Angi_diff_week']<-860
df_RF_noFB[which.max(df_RF_noFB$Yelp_Avg_Rating),'Yelp_Avg_Rating']<-4
df_RF_noFB[which.min(df_RF_noFB$HA_diff_week),'HA_diff_week']<-0

df_RF_noFB$Conversion <- as.factor(df_RF_noFB$Conversion)


#Replace all NA into 0
df_RF_noFB[is.na(df_RF_noFB)] <- 0

#If no profile, all other fields=0
df_RF_noFB$Google_Avg_Rating<-ifelse(df_RF_noFB$Google_P==F,0,df_RF_noFB$Google_Avg_Rating)
df_RF_noFB$Google___of_Reviews<-ifelse(df_RF_noFB$Google_P==F,0,df_RF_noFB$Google___of_Reviews)
df_RF_noFB$Yelp___of_Reviews<-ifelse(df_RF_noFB$Yelp_P==F,0,df_RF_noFB$Yelp___of_Reviews)
df_RF_noFB$Yelp_Avg_Rating<-ifelse(df_RF_noFB$Yelp_P==F,0,df_RF_noFB$Yelp_Avg_Rating)

#Add category group
category_group<-function(x){
  if(x['occupation'] %in% c('Drywall Specialist','Electrician','Gutter Specialist','Handyman',
                            'Home Electronics Specialist','Inspector and Restorer','Locksmith')){
    out<-'Handyman/Electrician'
  }else if(x['occupation'] %in% c('Engineer and Technical Designer','Furniture Service Specialist',
                                  'Interior Designer','Metalworker','Real Estate Professional')){
    out<-'Other'
  }else if(x['occupation'] %in% c('Mason','Paver')){
    out<-'Paver'
  }else if(x['Category'] %in% c('Sprinkler and Irrigation System Repair and Maintenance','Artificial Turf Installation',
                                'Outdoor Landscaping and Design','Fence and Gate Installation','Tree Trimming and Removal',
                                'Fence and Gate Repairs','Sprinkler and Irrigation System Installation','Patio Remodel or Addition',
                                'Sod Installation','In-Ground Swimming Pool Construction','Swimming Pool Repair','Swimming Pool Cleaning, Maintenance, and Inspection')){
    out<-'Landscaper 1'
  }else if(x['Category'] %in% c('Full Service Lawn Care','Gardening',
                                'Lawn Mowing and Trimming','Snow Plowing','Weeding')){
    out<-'Landscaper 2'
  }else{
    out<-x['occupation']
  }
}

df_RF_noFB$category_G<-apply(df_RF_noFB,1,category_group)

df_agg_noFB<-df_RF_noFB  %>% 
  mutate(Review_sum=Google___of_Reviews+Angi_s___of_Reviews+HA___of_Reviews+Yelp___of_Reviews,
         Avg_Rating_sum=Google_Avg_Rating+Angi_s_Avg_Rating+HA_Avg_Rating+Yelp_Avg_Rating,
         diff_week_sum=Google_diff_week+Angi_diff_week+HA_diff_week+Yelp_diff_week,
         P_num=Google_P+Angi_P+HA_P+Yelp_P,
         Review_mean=Review_sum/P_num,
         Avg_Rating_mean=Avg_Rating_sum/P_num,
         diff_week_mean=diff_week_sum/P_num)

#numerical category variables
df_agg_noFB$with_web<-ifelse(df_agg_noFB$with_web==F,0,1)
State_num<-function(x){
  if(x['State_G'] =='High'){
    out<-2
  }else if(x['State_G'] =='Mid'){
    out<-1
  }else{
    out<-0
  }
}
df_agg_noFB$State_G<-apply(df_agg_noFB,1,State_num)

Profile_num<-function(x){
  if(x['P_G'] =='High'){
    out<-2
  }else if(x['P_G'] =='Mid'){
    out<-1
  }else{
    out<-0
  }
}
df_agg_noFB$P_G<-apply(df_agg_noFB,1,Profile_num)

df_L2_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,
                                   Conversion)

df_L3_noFB<-df_L2_noFB
HA_group<-function(x){
  if(x['HA_Avg_Rating'] ==0){
    out<-1
  }else if(x['HA_Avg_Rating']< 4.7){
    out<-0
  }else{
    out<-2
  }
}

df_L3_noFB$HA_Avg_Rating<-apply(df_L3_noFB,1,HA_group)

runtest6<-function(df,outlier_rm=F,w=2,plot=1){
  library(AUC)
  res_recall<-NULL
  res_acc<-NULL
  res_auc<-NULL
  res_pre<-NULL
  res_outlier_index=NULL
  matrix<-NULL
  model<-NULL
  for(i in 1:30){
    set.seed(i)
    df_split<-df
    sample <- sample.split(df_split$Conversion, SplitRatio = .75)
    train <- subset(df_split, sample == TRUE)
    test <- subset(df_split, sample == FALSE)
    
    all_F<-train[train$Conversion==F,]
    sample_F<-all_F[sample(nrow(all_F),table(train$Conversion)[2]),]
    train_resample<-rbind(sample_F,train[train$Conversion==T,])
    weight<-ifelse(train_resample$Conversion==T,w,1)
    
    # define model
    m_w<-glm(Conversion~.,data=train_resample,family="binomial",weights=weight)
    print(m_w)
    
    if(outlier_rm){
      outlier_index<-augment(m_w) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) >= 2.5) %>% select(index)
      res_outlier_index<-c(res_outlier_index,outlier_index[[1]])
      
      train_resample_new<-augment(m_w) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) < 2.5) %>% select(1:ncol(df_split))
      
      weight2<-ifelse(train_resample_new$Conversion==T,w,1)
      
      m_w<-glm(Conversion~.,data=train_resample_new,family="binomial",weights=weight2)
    }
    # test1<-test
    # test1$category_G[which(!(test$category_G %in% unique(train_resample$category_G)))] <- NA  # Replace new levels by NA
    prod.m_w<-predict(m_w,test,type="response") #numerical
    
    preb.m_w<-rep("TRUE",length(prod.m_w))
    preb.m_w[prod.m_w<0.5]<-"FALSE" #categorical
    print(table(test$Conversion,preb.m_w))
    
    if(i==plot){
      matrix<-table(test$Conversion,preb.m_w)
      result<-as.data.frame(cbind(test[,ncol(train)][[1]],preb.m_w,prod.m_w,1-prod.m_w))
      names(result)<-c('actual','pred','prob_T','prob_F')
      model<-m_w
    }
    
    recall<-table(preb.m_w,test$Conversion)[2,2]/table(test$Conversion)[2]
    res_recall<-c(res_recall,recall)
    
    pre<-table(preb.m_w,test$Conversion)[2,2]/table(preb.m_w)[2]
    res_pre<-c(res_pre,pre)
    
    acc<-1-mean(preb.m_w!=test$Conversion) #ACC
    res_acc<-c(res_acc,acc)
    
    auc<-auc(roc(predictions=prod.m_w,labels=as.factor(test$Conversion)))
    res_auc<-c(res_auc,auc)
  }
  list("recall"=summary(res_recall),"ACC"=summary(res_acc),"AUC"=summary(res_auc),"Precision"=summary(res_pre),
       "outlier_index"=table(res_outlier_index),"pr_result"=result,"Matrix"=matrix,"Model"=model)
}



result_final<-runtest6(df_L3_noFB,w=2,outlier_rm=T)
result_final



result_final[c("recall","Precision","AUC","Matrix")] #chosen
summary(df_L3_noFB)
summary(result_final$Model)


library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(df_L3_noFB[,-5]),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2,Var1,label = value),
            color = "white", size = 12)+
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 19))

df_clean %>% group_by(Conversion) %>%
  summarise(n = n(),
            HA_diff=median(HA_diff_week,na.rm=T),
            HA_raing=mean(HA_Avg_Rating,na.rm=T)) %>%
  mutate(p=round(n/nrow(df),2))

df_agg_noFB %>% group_by(Conversion) %>%
  summarise(n = n(),
            State=mean(State_G),
            Profile=mean(P_G),
            Web=mean(with_web)) %>%
  mutate(p=round(n/nrow(df),2))

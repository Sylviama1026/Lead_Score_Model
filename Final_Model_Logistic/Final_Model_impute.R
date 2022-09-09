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
df_core_noFB<-df_clean %>% select(Id, Conversion,State_G,HA_Avg_Rating,HA_diff_week,Stand_Alone_Website)

df_RF_noFB<-df_core_noFB
#Removing Outliers and NA
df_RF_noFB[which.min(df_RF_noFB$HA_diff_week),'HA_diff_week']<-0

df_RF_noFB$Conversion <- as.factor(df_RF_noFB$Conversion)
df_RF_noFB[is.na(df_RF_noFB$HA_Avg_Rating),'HA_Avg_Rating'] <- 0
summary(df_RF_noFB$HA_Avg_Rating)

#numerical category variables
df_RF_noFB$Stand_Alone_Website<-ifelse(is.na(df_RF_noFB$Stand_Alone_Website),0,1)
State_num<-function(x){
  if(x['State_G'] =='High'){
    out<-2
  }else if(x['State_G'] =='Mid'){
    out<-1
  }else{
    out<-0
  }
}
df_RF_noFB$State_G<-apply(df_RF_noFB,1,State_num)


HA_group<-function(x){
  if(x['HA_Avg_Rating'] ==0){
    out<-1
  }else if(x['HA_Avg_Rating']< 4.7){
    out<-0
  }else{
    out<-2
  }
}

df_RF_noFB$HA_Avg_Rating<-apply(df_RF_noFB,1,HA_group)

df_RF_noFB<-df_RF_noFB %>% select(Stand_Alone_Website,HA_diff_week,HA_Avg_Rating,State_G,
                                   Conversion)


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




#try imputation
summary(df_RF_noFB$HA_diff_week)
#1. mean:worse?
df_L1_noFB<-df_RF_noFB

df_L1_noFB[is.na(df_L1_noFB$HA_diff_week),'HA_diff_week'] <- 253.93
summary(df_L1_noFB)

result_final<-runtest6(df_L1_noFB,w=2,outlier_rm=T)
result_final

#2. median
df_L2_noFB<-df_RF_noFB

df_L2_noFB[is.na(df_L2_noFB$HA_diff_week),'HA_diff_week'] <- 58.71
summary(df_L2_noFB)

result_final2<-runtest6(df_L2_noFB,w=2,outlier_rm=T)
result_final2


result_final[c("recall","Precision","AUC","Matrix")] #chosen
result_final2[c("recall","Precision","AUC","Matrix")] #chosen

#3. Replace as 0
df_L3_noFB<-df_RF_noFB

df_L3_noFB[is.na(df_L3_noFB$HA_diff_week),'HA_diff_week'] <- 0
summary(df_L3_noFB)

result_final3<-runtest6(df_L3_noFB,w=2,outlier_rm=T)
result_final3


result_final[c("recall","Precision","AUC","Matrix")] 
result_final2[c("recall","Precision","AUC","Matrix")] 
result_final3[c("recall","Precision","AUC","Matrix")] #chosen

library(caTools)
library(broom)
library(dplyr)
library(ggplot2)
library(smotefamily)
library(tidyr)

df_agg_noFB <- read_csv("df_agg_noFB.csv")
df_L1_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,P_G,
                                   Conversion)

give_pred_prob<-function(df,test){
  m_b<-glm(Conversion~.,data=df,family="binomial")
  print(m_b)
  prod.m_b.train<-predict(m_b,type="response")
  prod.m_b<-predict(m_b,test,type="response") #numerical
  
  preb.m_b<-rep("TRUE",length(prod.m_b))
  preb.m_b[prod.m_b<0.5]<-"FALSE" #categorical
  
  list(prod.m_b,preb.m_b)
}


runtest5<-function(df,outlier_rm=F){
  library(AUC)
  res_recall<-NULL
  res_acc<-NULL
  res_auc<-NULL
  res_outlier_index=NULL
  for(i in 1:30){
    set.seed(i)
    df_split<-df
    sample <- sample.split(df_split$Conversion, SplitRatio = .75)
    train <- subset(df_split, sample == TRUE)
    test <- subset(df_split, sample == FALSE)
    
    res_smote<-SMOTE(train %>% select(-Conversion),as.vector(train$Conversion),K=2,dup_size=0)
    df_smote<-res_smote$data[,-dim(train)[2]]
    df_smote$Conversion <- as.factor(res_smote$data$class)
    
    m1<-glm(Conversion~.,data=df_smote,family="binomial")
    
    if(outlier_rm){
      outlier_index<-augment(m1) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) >= 2.5) %>% select(index)
      res_outlier_index<-c(res_outlier_index,outlier_index[[1]])
      
      df_smote_new<-augment(m1) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) < 2.5) %>% select(1:ncol(df))
      m1<-glm(Conversion~.,data=df_smote_new,family="binomial")
    }
    prod.m1<-predict(m1,test,type="response") #numerical
    
    preb.m1<-rep("TRUE",length(prod.m1))
    preb.m1[prod.m1<0.5]<-"FALSE" #categorical
    print(table(test$Conversion,preb.m1))
    
    recall<-table(preb.m1,test$Conversion)[2,2]/table(test$Conversion)[2]
    res_recall<-c(res_recall,recall)
    
    acc<-1-mean(preb.m1!=test$Conversion) #ACC
    res_acc<-c(res_acc,acc)
    
    auc<-auc(roc(predictions=prod.m1,labels=as.factor(test$Conversion)))
    res_auc<-c(res_auc,auc)
  }
  list(summary(res_recall),summary(res_acc),summary(res_auc),table(res_outlier_index))
}

#1. No change
#train on smote-train, test on non-smote test
result_1<-runtest5(df_L1_noFB) #depend on test-train splitting, unstable

#2. Remove outliers
result_2<-runtest5(df_L1_noFB,outlier_rm = T)

#3. Regroup non-linear features
#regroup HA_Avg_Rating
df_L2_noFB<-df_L1_noFB
HA_group<-function(x){
  if(x['HA_Avg_Rating'] ==0){
    out<-2
  }else if(x['HA_Avg_Rating']< 4.3){
    out<-0
  }else if(x['HA_Avg_Rating']< 4.8){
    out<-1
  }else if(x['HA_Avg_Rating']==5){
    out<-3
  }else{
    out<-4
  }
}

df_L2_noFB$HA_rating_G<-apply(df_L2_noFB,1,HA_group)

cn<-table(df_L2_noFB$Conversion)[2]/nrow(df_L2_noFB)
df_L2_noFB %>% group_by(HA_rating_G) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))

result_3<-runtest5(df_L2_noFB %>% select(-HA_Avg_Rating),outlier_rm = T)

#Compare
result_1[[1]]
result_2[[1]]
result_3[[1]]

#Apply and Plotting: Before vs After
#Before
df_split<-df_L1_noFB
sample <- sample.split(df_split$Conversion, SplitRatio = .75)
train <- subset(df_split, sample == TRUE)
test <- subset(df_split, sample == FALSE)
dim(train)
dim(test)

res_smote<-SMOTE(train[,-dim(train)[2]],as.vector(train$Conversion),K=5,dup_size=0)
df_L1_noFB_smote<-res_smote$data[,-dim(train)[2]]
df_L1_noFB_smote$Conversion <- as.factor(res_smote$data$class)

#odds linear:
prod.model<-give_pred_prob(df_L1_noFB_smote,test)[[1]]
mydata <- test %>% select(HA_Avg_Rating,HA_diff_week) %>%
  mutate(logit = log(prod.model/(1-prod.model))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#After
df_L2_noFB_smote<-df_L1_noFB_smote
test2<-test
df_L2_noFB_smote$HA_rating_G<-apply(df_L2_noFB_smote,1,HA_group)
test2$HA_rating_G<-apply(test2,1,HA_group)

#odds linear:
prod.model<-give_pred_prob(df_L2_noFB_smote,test2)[[1]]
mydata <- test2 %>% select(HA_rating_G,HA_diff_week) %>%
  mutate(logit = log(prod.model/(1-prod.model))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

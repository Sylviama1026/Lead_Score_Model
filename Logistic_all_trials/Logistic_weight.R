library(caTools)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)

df_agg_noFB <- read_csv("df_agg_noFB.csv")
df_L1_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,P_G,
                                   Conversion)

#same size sample, different weight: high recall, same auc
runtest6<-function(df,outlier_rm=F){
  library(AUC)
  res_recall<-NULL
  res_acc<-NULL
  res_auc<-NULL
  res_pre<-NULL
  res_outlier_index=NULL
  for(i in 1:30){
    set.seed(i)
    df_split<-df
    sample <- sample.split(df_split$Conversion, SplitRatio = .75)
    train <- subset(df_split, sample == TRUE)
    test <- subset(df_split, sample == FALSE)
    
    all_F<-train[train$Conversion==F,]
    sample_F<-all_F[sample(nrow(all_F),table(train$Conversion)[2]),]
    train_resample<-rbind(sample_F,train[train$Conversion==T,])
    weight<-ifelse(train_resample$Conversion==T,2,1)
    
    # define model
    m_w<-glm(Conversion~.,data=train_resample,family="binomial",weights=weight)
    print(m_w)
    
    if(outlier_rm){
      outlier_index<-augment(m_w) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) >= 2.5) %>% select(index)
      res_outlier_index<-c(res_outlier_index,outlier_index[[1]])
      
      train_resample_new<-augment(m_w) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) < 2.5) %>% select(1:ncol(df))
      m_w<-glm(Conversion~.,data=train_resample_new,family="binomial")
    }
    prod.m_w<-predict(m_w,test,type="response") #numerical
    
    preb.m_w<-rep("TRUE",length(prod.m_w))
    preb.m_w[prod.m_w<0.5]<-"FALSE" #categorical
    print(table(test$Conversion,preb.m_w))
    
    recall<-table(preb.m_w,test$Conversion)[2,2]/table(test$Conversion)[2]
    res_recall<-c(res_recall,recall)
    
    pre<-table(preb.m_w,test$Conversion)[2,2]/table(preb.m_w)[2]
    res_pre<-c(res_pre,pre)
    
    acc<-1-mean(preb.m_w!=test$Conversion) #ACC
    res_acc<-c(res_acc,acc)
    
    auc<-auc(roc(predictions=prod.m_w,labels=as.factor(test$Conversion)))
    res_auc<-c(res_auc,auc)
  }
  list(summary(res_recall),summary(res_acc),summary(res_auc),summary(res_pre),
       table(res_outlier_index))
}


result_w<-runtest6(df_L1_noFB)
result_w


#ROSE: both oversampling and undersampling->similar to RF result, unstable
library(ROSE)

runtest7<-function(train,test,outlier_rm=F){
  library(AUC)
  res_recall<-NULL
  res_acc<-NULL
  res_auc<-NULL
  res_outlier_index=NULL
  res_pre<-NULL
  for(i in 1:30){
    set.seed(i)
    train_resample <- ovun.sample(Conversion~., data = train, method = "both",N = 2000)$data
    
    # define model
    m_w<-glm(Conversion~.,data=train_resample,family="binomial")
    print(m_w)
    prod.m_w<-predict(m_w,test,type="response") #numerical
    
    preb.m_w<-rep("TRUE",length(prod.m_w))
    preb.m_w[prod.m_w<0.5]<-"FALSE" #categorical
    print(table(test$Conversion,preb.m_w))
    
    recall<-table(preb.m_w,test$Conversion)[2,2]/table(test$Conversion)[2]
    res_recall<-c(res_recall,recall)
    
    pre<-confusionMatrix(preb.m_w, test$Conversion, mode = "everything", positive="TRUE")$byClass['Precision']
    res_pre<-c(res_pre,pre)
    
    acc<-1-mean(preb.m_w!=test$Conversion) #ACC
    res_acc<-c(res_acc,acc)
    
    auc<-auc(roc(predictions=prod.m_w,labels=as.factor(test$Conversion)))
    res_auc<-c(res_auc,auc)
  }
  list(summary(res_recall),summary(res_acc),summary(res_auc),table(res_outlier_index))
}

i=2
set.seed(i)
df_split<-df_L1_noFB
sample <- sample.split(df_split$Conversion, SplitRatio = .75)
train <- subset(df_split, sample == TRUE)
test <- subset(df_split, sample == FALSE)

train$Conversion<-as.factor(train$Conversion)
train<-as.data.frame(train)

result_r<-runtest7(train,test)
result_r


#weight-training:
runtest8<-function(df,outlier_rm=F){
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
    
    all_F<-train[train$Conversion==F,]
    sample_F<-all_F[sample(nrow(all_F),table(train$Conversion)[2]),]
    train_resample<-rbind(sample_F,train[train$Conversion==T,])
    weight<-ifelse(train_resample$Conversion==T,2,1)
    
    # define model
    m_w<-glm(Conversion~.,data=train_resample,family="binomial",weights=weight)
    #print(m_w)
    
    if(outlier_rm){
      outlier_index<-augment(m_w) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) >= 2.5) %>% select(index)
      res_outlier_index<-c(res_outlier_index,outlier_index[[1]])
      
      train_resample_new<-augment(m_w) %>% mutate(index = 1:n()) %>% 
        filter(abs(.std.resid) < 2.5) %>% select(1:ncol(df))
      m_w<-glm(Conversion~.,data=train_resample_new,family="binomial")
    }
    prod.m_w<-predict(m_w,type="response") #numerical
    
    preb.m_w<-rep("TRUE",length(prod.m_w))
    preb.m_w[prod.m_w<0.5]<-"FALSE" #categorical
    print(table(train_resample$Conversion,preb.m_w))
    
    recall<-table(preb.m_w,train_resample$Conversion)[2,2]/table(train_resample$Conversion)[2]
    res_recall<-c(res_recall,recall)
    
    acc<-1-mean(preb.m_w!=train_resample$Conversion) #ACC
    res_acc<-c(res_acc,acc)
    
    auc<-auc(roc(predictions=prod.m_w,labels=as.factor(train_resample$Conversion)))
    res_auc<-c(res_auc,auc)
  }
  list(summary(res_recall),summary(res_acc),summary(res_auc),table(res_outlier_index))
}

result_w2<-runtest8(df_L1_noFB)
result_w2

#Compare all
#RF:diff features
result1[[1]]
result2[[1]] #highest
result3[[1]]
result4[[1]]

#Logistic-SMOTE: remove outliers, fix linearity
result_1[[1]]
result_2[[1]]
result_3[[1]]

#Logistic-ROSE
result_r[[1]]

#Logistic-weight
result_w[[1]]


#Final compare
result_w<-runtest6(df_L1_noFB)
result_w

result3<-runtest1(df_RF3_noFB)
result3
result31<-runtest0(df_RF3_noFB)
result31

result_w
result31


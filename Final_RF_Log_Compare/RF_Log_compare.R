library(caTools)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(yardstick)
library(readr)
library(randomForest)
library(caret)

df_agg_noFB <- read_csv("df_agg_noFB.csv")

runtest0<-function(df_RF,plot=1){
  library(AUC)
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Precision<-NULL
  result<-NULL
  matrix<-NULL
  for(i in 1:30){
    set.seed(i)
    df_split<-df_RF
    sample <- sample.split(df_split$Conversion, SplitRatio = .75)
    train <- subset(df_split, sample == TRUE)
    test <- subset(df_split, sample == FALSE)
    
    nRareSamples = 900 * 0.05
    rf.strata = randomForest(as.factor(Conversion)~.,data=train,strata=train$Conversion,
                             sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=dim(train)[2]-1,
                             importance=TRUE)
    #print(rf.strata)
    pred <- predict(rf.strata, newdata=test[-ncol(train)])
    prob<-predict(rf.strata, newdata=test[-ncol(train)], type="prob")
    print(table(test[,ncol(train)][[1]], pred))
    
    
    if(i==plot){
      matrix<-table(test[,ncol(train)][[1]], pred)
      result<-as.data.frame(cbind(test[,ncol(test)][[1]],pred,prob))
      names(result)<-c('actual','pred','prob_F','prob_T')
    }
    
    
    rec<-confusionMatrix(pred, as.factor(test$Conversion), mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    pre<-confusionMatrix(pred, as.factor(test$Conversion), mode = "everything", positive="TRUE")$byClass['Precision']
    Precision<-c(Precision,pre)
    
    oob<-confusionMatrix(pred, as.factor(test$Conversion), mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(predictions=prob[,2],labels=as.factor(test$Conversion)))
    Area<-c(Area,a)
  }
  list("recall"=summary(Recall),"ACC"=summary(ACC),"AUC"=summary(Area),"Precision"=summary(Precision),"pr_result"=result,
       "Matrix"=matrix)
}

runtest_weight<-function(df,w=2,plot=1){
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  Precision<-NULL
  result<-NULL
  matrix<-NULL
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
    rf.w = randomForest(as.factor(Conversion)~.,data=train_resample,
                        weights=weight,
                        importance=TRUE)
    print(rf.w)
    
    pred <- predict(rf.w, newdata=test[-ncol(train)])
    prob<-predict(rf.w, newdata=test[-ncol(train)], type="prob")
    print(table(test[,ncol(train)][[1]], pred))
    
    if(i==plot){
      matrix<-table(test[,ncol(train)][[1]], pred)
      result<-as.data.frame(cbind(test[,ncol(test)][[1]],pred,prob))
      names(result)<-c('actual','pred','prob_F','prob_T')
      result['actual']<-as.factor(ifelse(result['actual']==0,F,T))
      result['pred']<-as.factor(ifelse(result['pred']==1,F,T))
    }
    
    
    rec<-confusionMatrix(pred, as.factor(test$Conversion), mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    pre<-confusionMatrix(pred, as.factor(test$Conversion), mode = "everything", positive="TRUE")$byClass['Precision']
    Precision<-c(Precision,pre)
    
    oob<-confusionMatrix(pred, as.factor(test$Conversion), mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(predictions=prob[,2],labels=as.factor(test$Conversion)))
    Area<-c(Area,a)
  }
  list("recall"=summary(Recall),"ACC"=summary(ACC),"AUC"=summary(Area),"Precision"=summary(Precision),
       "pr_result"=result,"Matrix"=matrix)
}

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

df_L1_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,P_G,
                                   Conversion) 

df_L2_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,
                                   Conversion)

df_L4_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,category_G,
                                   Conversion)


#RF_rf.strata1
result_s1<-runtest0(df_L1_noFB)
result_s1

#RF_rf.strata2
result_s2<-runtest0(df_L2_noFB)
result_s2


#RF_weight
result_rf_w<-runtest_weight(df_L1_noFB,w=2)
result_rf_w

#log:with/without P_G
result_w<-runtest6(df_L1_noFB,w=2,outlier_rm=T)
result_w

result_w2<-runtest6(df_L2_noFB,w=2,outlier_rm=T)
result_w2

# result_w4<-runtest6(df_L4_noFB,w=2,outlier_rm=T)
# result_w4


result_s1[c("recall","Precision","AUC","Matrix")] #better
result_s2[c("recall","Precision","AUC","Matrix")]

result_rf_w[c("recall","Precision","AUC","Matrix")]
result_w[c("recall","Precision","AUC","Matrix")]

result_w2[c("recall","Precision","AUC","Matrix")] #chosen



#Plotting
result_s1$pr_result['actual']<-as.factor(ifelse(result_s1$pr_result['actual']==0,F,T))
result_s1$pr_result['pred']<-as.factor(ifelse(result_s1$pr_result['pred']==1,F,T))
pr_rf_s1<-pr_curve(data = result_s1$pr_result, actual, prob_F)


result_s2$pr_result['actual']<-as.factor(ifelse(result_s2$pr_result['actual']==0,F,T))
result_s2$pr_result['pred']<-as.factor(ifelse(result_s2$pr_result['pred']==1,F,T))
pr_rf_s2<-pr_curve(data = result_s2$pr_result, actual, prob_F)



pr_rf<-pr_curve(data = result_rf_w$pr_result, actual, prob_F)

result_w$pr_result$prob_F<-as.numeric(result_w$pr_result$prob_F)
result_w$pr_result$actual<-as.factor(result_w$pr_result$actual)
pr_log<-pr_curve(data = result_w$pr_result, actual, prob_F)

plot(pr_rf$recall,pr_rf$precision,type = "l",main="black rf_w, red logistic, green rf_s1, blue rf_s2")
lines(pr_log$recall,pr_log$precision,type = "l",col=2)
lines(pr_rf_s1$recall,pr_rf_s1$precision,type = "l",col=3)
#lines(pr_rf_s2$recall,pr_rf_s2$precision,type = "l",col=4)




set.seed(2)
df_split<-df_L2_noFB
sample <- sample.split(df_split$Conversion, SplitRatio = .75)
train <- subset(df_split, sample == TRUE)
test <- subset(df_split, sample == FALSE)
dim(train)
dim(test)

all_F<-train[train$Conversion==F,]
sample_F<-all_F[sample(nrow(all_F),table(train$Conversion)[2]),]
train_resample<-rbind(sample_F,train[train$Conversion==T,])
weight<-ifelse(train_resample$Conversion==T,2,1)

# define model & remove outliers
m_w<-glm(Conversion~.,data=train_resample,family="binomial")
summary(m_w)

# train_resample_new<-augment(m_w) %>% mutate(index = 1:n()) %>% 
#   filter(abs(.std.resid) < 2.5) %>% select(1:ncol(df_L1_noFB))
# m_w<-glm(Conversion~.,data=train_resample_new,family="binomial")

prod.m_w<-predict(m_w,test,type="response") #numerical

preb.m_w<-rep("TRUE",length(prod.m_w))
preb.m_w[prod.m_w<0.5]<-"FALSE" #categorical
print(table(test$Conversion,preb.m_w))

#odds linear:
mydata <- test %>% select(HA_Avg_Rating,HA_diff_week) %>%
  mutate(logit = log(prod.m_w/(1-prod.m_w))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")





#Regroup non-linear features
#regroup HA_Avg_Rating
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

cn<-table(df_L3_noFB$Conversion)[2]/nrow(df_L3_noFB)
df_L3_noFB %>% group_by(HA_Avg_Rating) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))

#After
test2<-test
test2$HA_Avg_Rating<-apply(test2,1,HA_group)

mydata <- test2 %>% select(HA_Avg_Rating,HA_diff_week) %>%
  mutate(logit = log(prod.m_w/(1-prod.m_w))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


result_w3<-runtest6(df_L3_noFB,w=2,outlier_rm=T)
result_w3

result_rf_w3<-runtest_weight(df_L3_noFB,w=2)
result_rf_w3


result_rf_w3[c("recall","Precision","AUC","Matrix")] #better
result_rf_w[c("recall","Precision","AUC","Matrix")]


result_w3[c("recall","Precision","AUC","Matrix")]#chosen
result_w2[c("recall","Precision","AUC","Matrix")] 


#Final Decision
result_rf_w3[c("recall","Precision","AUC","Matrix")] #88 recall,pre 10, 68 auc
result_w3[c("recall","Precision","AUC","Matrix")] #92 recall, pre 10, 71 auc, chosen?

result_s1[c("recall","Precision","AUC","Matrix")] #69 recall, pre13, 70 auc


result_w3[c("recall","Precision","AUC","Matrix")]
result_w[c("recall","Precision","AUC","Matrix")] #better
result_w2[c("recall","Precision","AUC","Matrix")] #better



result<-result_w3$pr_result
result$prob_F<-as.numeric(result$prob_F)
result$actual<-as.factor(result$actual)
# prob hist
library(plotly)
library(tidymodels)
z <- roc_curve(data = result, 'actual', 'prob_F')
z$specificity <- 1 - z$specificity
colnames(z) <- c('threshold', 'tpr', 'fpr')

fig1 <- plot_ly(x= result['prob_T'][[1]], color = result['actual'][[1]], colors = c('blue', 'red'), type = 'histogram', alpha = 0.5, nbinsx = 50) %>%
  layout(barmode = "overlay",xaxis = list(title = "Probability of being TRUE"), yaxis = list(title = "Frequency"))%>%
  add_segments(x = 0.5, xend = 0.5, y = 0, yend = 100, line = list(dash = "dash", color = 'black'),inherit = FALSE,name='Threshold')
fig1

# TPR vs FPR curve
fig2 <- plot_ly(data = z, x = ~threshold) %>%
  add_trace(y = ~fpr, mode = 'lines', name = 'False Positive Rate', type = 'scatter')%>%
  add_trace(y = ~tpr, mode = 'lines', name = 'True Positive Rate', type = 'scatter')%>%
  layout(title = 'TPR and FPR at every threshold',yaxis = list(title = "Rate"))
fig2 <- fig2 %>% layout(legend=list(title=list(text='<b> Rate </b>')))
fig2

#ROC curve
library(pROC)
pdb <- roc_curve(data = result, actual, prob_F)
pdb$specificity <- 1 - pdb$specificity
auc = roc_auc(data = result, actual, prob_F)
auc = auc$.estimate

tit = paste('ROC Curve (AUC = ',toString(round(auc,2)),')',sep = '')


fig_roc <-  plot_ly(data = pdb ,x =  ~specificity, y = ~sensitivity, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(title = tit,xaxis = list(title = "False Positive Rate"), yaxis = list(title = "True Positive Rate")) %>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'),inherit = FALSE, showlegend = FALSE)
fig_roc
detach("package:caret", unload = TRUE)
detach("package:pROC", unload = TRUE)

#adjust classifier threshold
result_new<-result
result_new$pred_new<-as.factor(ifelse(result_new['prob_F']<0.48,T,F)) #0.45(FN+2, 540), 0.48(FN same, 600)
result_new$correct_new<-ifelse(result_new['actual']==result_new['pred_new'],T,F)
#View(result_new)

table(result_new['actual'][[1]], result_new['pred_new'][[1]])
table(result['actual'][[1]], result['pred'][[1]])


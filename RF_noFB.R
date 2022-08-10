library(readr)
library(AUC)
library(randomForest)
library(dplyr)

#RF: without FB
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

df_RF_noFB$Conversion <- as.factor(df_RF_noFB$Conversion)
table(df_RF_noFB$Conversion)


#Replace all NA into 0
df_RF_noFB[is.na(df_RF_noFB)] <- 0

#If no profile, all other fields=0
df_RF_noFB$Google_Avg_Rating<-ifelse(df_RF_noFB$Google_P==F,0,df_RF_noFB$Google_Avg_Rating)
df_RF_noFB$Google___of_Reviews<-ifelse(df_RF_noFB$Google_P==F,0,df_RF_noFB$Google___of_Reviews)
df_RF_noFB$Yelp___of_Reviews<-ifelse(df_RF_noFB$Yelp_P==F,0,df_RF_noFB$Yelp___of_Reviews)
df_RF_noFB$Yelp_Avg_Rating<-ifelse(df_RF_noFB$Yelp_P==F,0,df_RF_noFB$Yelp_Avg_Rating)


#para test function
runtest1<-function(df_RF){
  library(AUC)
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  for(i in 1:10){
    nRareSamples = 900 * 0.05
    rf.strata = randomForest(Conversion~.,data=df_RF,strata=df_RF$Conversion,
                             sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=dim(df_RF)[2]-1,
                             importance=TRUE)
    print(rf.strata)
    
    rec<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    oob<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(predictions=rf.strata$votes[,2],labels=df_RF$Conversion))
    Area<-c(Area,a)
    
    names<-row.names(as.data.frame(rf.strata$importance) %>% arrange(MeanDecreaseAccuracy))[1]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(rf.strata$importance) %>% arrange(desc(MeanDecreaseAccuracy)))[1:3]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}

runtest0<-function(df_RF){
  library(AUC)
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  for(i in 1:10){
    set.seed(i)
    df_split<-df_RF
    sample <- sample.split(df_split$Conversion, SplitRatio = .75)
    train <- subset(df_split, sample == TRUE)
    test <- subset(df_split, sample == FALSE)
    
    nRareSamples = 900 * 0.05
    rf.strata = randomForest(Conversion~.,data=train,strata=train$Conversion,
                             sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=dim(train)[2]-1,
                             importance=TRUE)
    #print(rf.strata)
    pred <- predict(rf.strata, newdata=test[-ncol(train)])
    prob<-predict(rf.strata, newdata=test[-ncol(train)], type="prob")
    print(table(test[,ncol(train)][[1]], pred))
    
    rec<-confusionMatrix(pred, test$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    oob<-confusionMatrix(pred, test$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(predictions=prob[,2],labels=test$Conversion))
    Area<-c(Area,a)
    
    names<-row.names(as.data.frame(rf.strata$importance) %>% arrange(MeanDecreaseAccuracy))[1]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(rf.strata$importance) %>% arrange(desc(MeanDecreaseAccuracy)))[1:3]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}

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
  }else{
    out<-x['occupation']
  }
}

df_RF_noFB$category_G<-apply(df_RF_noFB,1,category_group)
table(df_RF_noFB$category_G)

cn<-table(df_RF_noFB$Conversion)[2]/nrow(df_RF_noFB)
df_RF_noFB %>% group_by(category_G) %>% 
  summarise(n = n(), c=table(Conversion)[2]) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))


#Aggregate to HA
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
    out<-3
  }else if(x['State_G'] =='Mid'){
    out<-2
  }else{
    out<-1
  }
}
df_agg_noFB$State_G<-apply(df_agg_noFB,1,State_num)

Profile_num<-function(x){
  if(x['P_G'] =='High'){
    out<-3
  }else if(x['P_G'] =='Mid'){
    out<-2
  }else{
    out<-1
  }
}
df_agg_noFB$P_G<-apply(df_agg_noFB,1,Profile_num)

write.csv(df_agg_noFB,file='df_agg_noFB.csv')

library(caret)
#Model1: With agg+P_G, stable
#Simple model:Avg_Rating_mean,HA_Avg_Rating,diff_week_mean,State_G,
nRareSamples =  900 * 0.05
df_RF1_noFB<-df_agg_noFB %>% select(with_web,diff_week_mean,Avg_Rating_mean,State_G,P_G,
                          Conversion)

rf.strata1_no_FB = randomForest(Conversion~.,data=df_RF1_noFB,strata=df_RF1_noFB$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=dim(df_RF1_noFB)[2]-1,
                          importance=TRUE)
print(rf.strata1_no_FB)
result1<-runtest1(df_RF1_noFB)
result1
result11<-runtest0(df_RF1_noFB)
result11
as.data.frame(rf.strata1_no_FB$importance) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata1_no_FB)

#Model2: No agg+P_G, stable
df_RF2_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,P_G,
                                    Conversion)

rf.strata2_no_FB = randomForest(Conversion~.,data=df_RF2_noFB,strata=df_RF2_noFB$Conversion,
                                sampsize=c(nRareSamples,nRareSamples),
                                ntree=400,mtry=dim(df_RF2_noFB)[2]-1,
                                importance=TRUE)
print(rf.strata2_no_FB)
result2<-runtest1(df_RF2_noFB)
result2
result21<-runtest0(df_RF2_noFB)
result21
as.data.frame(rf.strata2_no_FB$importance) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata2_no_FB)


#Model3: No agg, no P_G, less stable, less accuracy/auc
df_RF3_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,
                                    Conversion)

rf.strata3_no_FB = randomForest(Conversion~.,data=df_RF3_noFB,strata=df_RF3_noFB$Conversion,
                                sampsize=c(nRareSamples,nRareSamples),
                                ntree=400,mtry=dim(df_RF3_noFB)[2]-1,
                                importance=TRUE)
print(rf.strata3_no_FB)
result3<-runtest1(df_RF3_noFB)
result3
result31<-runtest0(df_RF3_noFB)
result31
as.data.frame(rf.strata3_no_FB$importance) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata3_no_FB)

#Model4: No agg+category_G,no P_G,
df_RF4_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,category_G,
                                    Conversion)

rf.strata4_no_FB = randomForest(Conversion~.,data=df_RF4_noFB,strata=df_RF4_noFB$Conversion,
                                sampsize=c(nRareSamples,nRareSamples),
                                ntree=400,mtry=dim(df_RF4_noFB)[2]-1,
                                importance=TRUE)
print(rf.strata4_no_FB)
result4<-runtest1(df_RF4_noFB)
result4
result41<-runtest0(df_RF4_noFB)
result41
as.data.frame(rf.strata4_no_FB$importance) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata4_no_FB)


#Compare
result1[[1]]
result2[[1]] #highest
result3[[1]]
result4[[1]]

result11[[1]]
result21[[1]] #highest
result31[[1]]
result41[[1]]

####################################################################
#Take Model2:No agg+P_G, stable
#test-train split: .75
library(caTools)
df_split<-df_RF2_noFB
sample <- sample.split(df_split$Conversion, SplitRatio = .75)
train <- subset(df_split, sample == TRUE)
test <- subset(df_split, sample == FALSE)
dim(train)
dim(test)

library(caret)
rf_noFB<-randomForest(Conversion~.,data=train,strata=train$Conversion,
                 sampsize=c(nRareSamples,nRareSamples),
                 ntree=400,mtry=dim(train)[2]-1,
                 importance=TRUE,
                 keep.inbag=TRUE)
print(rf_noFB)
#result4<-runtest1(train)
#result4
as.data.frame(rf_noFB$importance) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf_noFB)

pred <- predict(rf_noFB, newdata=test[-ncol(test)])
table(test[,ncol(test)][[1]], pred)

detach("package:caret", unload = TRUE)
#detach("package:AUC", unload = TRUE)

#vote
prob<-predict(rf_noFB, newdata=test[-ncol(test)], type="prob")

#Result
result<-as.data.frame(cbind(test[,ncol(test)][[1]],pred,prob))
names(result)<-c('actual','pred','prob_F','prob_T')
result$correct<-ifelse(result['actual']==result['pred'],T,F)
result['actual']<-as.factor(ifelse(result['actual']==1,F,T))
result['pred']<-as.factor(ifelse(result['pred']==1,F,T))
#View(result)

#ROC curve
library(dplyr)
library(ggplot2)
library(plotly)
library(pROC)
library(yardstick)
pdb <- roc_curve(data = result, actual, prob_F)
pdb$specificity <- 1 - pdb$specificity
auc = roc_auc(data = result, actual, prob_F)
auc = auc$.estimate

tit = paste('ROC Curve (AUC = ',toString(round(auc,2)),')',sep = '')


fig_roc <-  plot_ly(data = pdb ,x =  ~specificity, y = ~sensitivity, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(title = tit,xaxis = list(title = "False Positive Rate"), yaxis = list(title = "True Positive Rate")) %>%
  add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = 'black'),inherit = FALSE, showlegend = FALSE)
fig_roc
detach("package:pROC", unload = TRUE)
detach("package:yardstick", unload = TRUE)


#PDP plot
library(pdp)

# Single Variable: yhat=prob_F
pdp<- partial(rf_noFB, pred.var = c("HA_diff_week"), chull = TRUE)
plot.pdp <- autoplot(pdp, contour = TRUE)
plot.pdp

pdp3<- partial(rf_noFB, pred.var = c("HA_Avg_Rating"), chull = TRUE)
plot.pdp3 <- autoplot(pdp3, contour = TRUE)
plot.pdp3


#Correlation heatmap
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(df_RF2_noFB %>% select(-Conversion)))
cor(df_RF2_noFB %>% select(-Conversion))
summary(df_RF2_noFB)





















#Try Smote
library(smotefamily)
runtest2<-function(df_smote,df_RF){
  library(AUC)
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  for(i in 1:10){
    rf.smote = randomForest(Conversion~.,data=df_smote,ntree=400,mtry=dim(df_smote)[2]-1,
                             importance=TRUE)
    #print(rf.smote)
    
    pred <- predict(rf.smote, newdata=df_RF[-ncol(df_RF)])
    prob<-predict(rf.smote, newdata=df_RF[-ncol(df_RF)], type="prob")
    print(table(df_RF$Conversion,pred))
    
    rec<-confusionMatrix(pred, df_RF$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    oob<-confusionMatrix(pred, df_RF$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(predictions=prob[,2],labels=df_RF$Conversion))
    Area<-c(Area,a)
    
    names<-row.names(as.data.frame(rf.smote$importance) %>% arrange(MeanDecreaseAccuracy))[1:2]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(rf.smote$importance) %>% arrange(desc(MeanDecreaseAccuracy)))[1:2]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}

runtest2_stratified<-function(df_smote,df_RF){
  library(AUC)
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  for(i in 1:10){
    nRareSamples = 900 * 0.05
    rf.smote = randomForest(Conversion~.,data=df_smote,strata=df_smote$Conversion,
                            sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=dim(df_smote)[2]-1,
                            importance=TRUE)
    
    #print(rf.smote)
    
    pred <- predict(rf.smote, newdata=df_RF[-ncol(df_RF)])
    prob<-predict(rf.smote, newdata=df_RF[-ncol(df_RF)], type="prob")
    print(table(df_RF$Conversion,pred))
    
    rec<-confusionMatrix(pred, df_RF$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    oob<-confusionMatrix(pred, df_RF$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(predictions=prob[,2],labels=df_RF$Conversion))
    Area<-c(Area,a)
    
    names<-row.names(as.data.frame(rf.smote$importance) %>% arrange(MeanDecreaseAccuracy))[1:2]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(rf.smote$importance) %>% arrange(desc(MeanDecreaseAccuracy)))[1:2]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}

#train-test split, only smote train
library(caret)
df_split<-df_RF2_noFB
sample <- sample.split(df_split$Conversion, SplitRatio = .75)
train <- subset(df_split, sample == TRUE)
test <- subset(df_split, sample == FALSE)
dim(train)
dim(test)

res_smote<-SMOTE(train[,-dim(df_RF2_noFB)[2]],train$Conversion,K=5,dup_size=0)
df_RF_smote<-res_smote$data[,-dim(train)[2]]
df_RF_smote$Conversion <- as.factor(res_smote$data$class)


#train on smote-train, test on non-smote test
runtest2(df_RF_smote,test)
runtest2_stratified(df_RF_smote,test)
detach("package:caret", unload = TRUE)

#Remark: SMOTE changes the distirbution of original data, not good for RF

library(readr)
df_core <- read_csv("df_core.csv",show_col_types = FALSE)
df_RF <- read_csv("df_RF.csv")
df_RF<-df_RF[-1]

df_RF$Conversion <- as.factor(df_RF$Conversion)
table(df_RF$Conversion)
#Replace all NA into 0
df_RF[is.na(df_RF)] <- 0

#If no profile, all other fields=0
#df_RF$Google_P<-ifelse(df_RF$Google___of_Reviews!=0,T,F)
df_RF$Google_Avg_Rating<-ifelse(df_RF$Google_P==F,0,df_RF$Google_Avg_Rating)
df_RF$Google___of_Reviews<-ifelse(df_RF$Google_P==F,0,df_RF$Google___of_Reviews)
df_RF$Yelp___of_Reviews<-ifelse(df_RF$Yelp_P==F,0,df_RF$Yelp___of_Reviews)
df_RF$Yelp_Avg_Rating<-ifelse(df_RF$Yelp_P==F,0,df_RF$Yelp_Avg_Rating)
df_RF$FB_P<-ifelse(df_RF$FB___of_Reviews!=0,T,F)


#Solve imbalanced data
#1. change result cutoff
library(caret)
library(AUC)
rare.class.prevalence = 0.07
rf.cutoff = randomForest(Conversion~.,data=df_RF,cutoff=c(1-rare.class.prevalence,rare.class.prevalence))
print(rf.cutoff)

confusionMatrix(rf.cutoff$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")
auc(roc(rf.cutoff$votes[,2],df_RF$Conversion))

#2. sampling stratification
nRareSamples = 1000 * rare.class.prevalence
rf.strata = randomForest(Conversion~.,data=df_RF,strata=df_RF$Conversion,
                         sampsize=c(nRareSamples,nRareSamples))
print(rf.strata)

confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")

#3 - Balancing by class-weight during training.
rf.classwt = randomForest(Conversion~.,data=df_RF,classwt=c(0.0005,1200))
print(rf.classwt)

confusionMatrix(rf.classwt$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")

#view OOB-CV specificity and sensitiviy
plot(roc(rf.cutoff$votes[,2],df_RF$Conversion),main="black default, red stata, green classwt")
plot(roc(rf.strata$votes[,2],df_RF$Conversion),col=2,add=T)
plot(roc(rf.classwt$votes[,2],df_RF$Conversion),col=3,add=T)


#Set nRareSamples,ntree,mtry
#,ntree=450,mtry=2*sqrt(dim(df_RF)[2])
out<-NULL
out1<-NULL
Area<-NULL
for(i in 1:10){
  nRareSamples = 900 * 0.05
  rf.strata = randomForest(Conversion~.,data=df_RF,strata=df_RF$Conversion,
                           sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=2*sqrt(dim(df_RF)[2]))
  print(rf.strata)
  
  rec<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
  out<-c(out,rec)
  
  oob<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
  out1<-c(out1,oob)
  
  a<-auc(roc(rf.strata$votes[,2],df_RF$Conversion))
  Area<-c(Area,a)
}
summary(out)
summary(out1)
summary(Area)

#para test function
runtest<-function(df_RF){
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  for(i in 1:10){
    nRareSamples = 900 * 0.05
    rf.strata = randomForest(Conversion~.,data=df_RF,strata=df_RF$Conversion,
                             sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=2*sqrt(dim(df_RF)[2]),
                             importance=TRUE)
    print(rf.strata)
    
    rec<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    oob<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(rf.strata$votes[,2],df_RF$Conversion))
    Area<-c(Area,a)
    
    names<-row.names(as.data.frame(importance(rf.strata)) %>% arrange(MeanDecreaseAccuracy))[1:5]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(importance(rf.strata)) %>% arrange(desc(MeanDecreaseAccuracy)))[1:5]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}



#feature importance
nRareSamples =  900 * 0.05
rf.strata = randomForest(Conversion~.,data=df_RF,strata=df_RF$Conversion,
                         sampsize=c(nRareSamples,nRareSamples),
                         ntree=400,mtry=2*sqrt(dim(df_RF)[2]),
                         importance=TRUE)
print(rf.strata)
runtest(df_RF)
as.data.frame(importance(rf.strata)) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata)

ggplot(as.data.frame(importance(rf.strata)), aes(x=reorder(rownames(as.data.frame(importance(rf.strata))),MeanDecreaseAccuracy), 
                                                 y=MeanDecreaseAccuracy,
                                                 fill=MeanDecreaseAccuracy))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Mean Decrease Accuracy")+
  xlab("")+
  #ggtitle("Variable Importance Plot")+
  scale_fill_gradient(low="red", high="blue")+
  theme_minimal()


#feature importance1:drop Google_P,0.6858
df_RF1<-df_RF %>% select(-Google_P)
rf.strata1 = randomForest(Conversion~.,data=df_RF1,strata=df_RF1$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=2*sqrt(dim(df_RF1)[2]),
                          importance=TRUE)
print(rf.strata1)
runtest(df_RF1) #worst=Google_diff_week/Yelp_p/HA_P, best=with_web/HA_diff_week
importance(rf.strata1)
varImpPlot(rf.strata1)

#feature importance2:drop 4 with_P feature,0.6880
df_RF2<-df_RF %>% select(-Yelp_P,-HA_P,-Angi_P,-Google_P)
rf.strata2 = randomForest(Conversion~.,data=df_RF2,strata=df_RF2$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=2*sqrt(dim(df_RF2)[2]),
                          importance=TRUE)
print(rf.strata2)
runtest(df_RF2)
importance(rf.strata2)
varImpPlot(rf.strata2)


#Wilson Score continuity correction
Wilson<-function(df_RF,var_n,var_x,c=T){
  n<-df_RF[df_RF[var_n]!=0,var_n][[1]]
  ar<-df_RF[df_RF[var_n]!=0,var_x][[1]]
  x<-ifelse(n-round((5-ar)*n/2,0)>0,n-round((5-ar)*n/2,0),0)
  new_rating<-NULL
  for(i in 1:length(n)){
    if(c){
      r<-prop.test(x=x[i],n=n[i])$ conf.int[1]
    }else{
      r<-prop.test(x=x[i],n=n[i],correct=F)$ conf.int[1]
    }
    new_rating<-c(new_rating,r)
  }
  new_rating
}
HA_rating<-Wilson(df_RF,'HA___of_Reviews','HA_Avg_Rating')
Angi_rating<-Wilson(df_RF,'Angi_s___of_Reviews','Angi_s_Avg_Rating')
Yelp_rating<-Wilson(df_RF,'Yelp___of_Reviews','Yelp_Avg_Rating')
G_rating<-Wilson(df_RF,'Google___of_Reviews','Google_Avg_Rating')
FB_rating<-Wilson(df_RF,'FB___of_Reviews','FB_Avg_Rating')

summary(HA_rating)
summary(df_RF$HA_Avg_Rating)

df_RF_wilson<-df_RF

df_RF_wilson$HA_rating<-df_RF_wilson$HA___of_Reviews
df_RF_wilson[df_RF_wilson$HA___of_Reviews!=0,'HA_rating']<-HA_rating

df_RF_wilson$Angi_rating<-df_RF_wilson$Angi_s___of_Reviews
df_RF_wilson[df_RF_wilson$Angi_s___of_Reviews!=0,'Angi_rating']<-Angi_rating

df_RF_wilson$Yelp_rating<-df_RF_wilson$Yelp___of_Reviews
df_RF_wilson[df_RF_wilson$Yelp___of_Reviews!=0,'Yelp_rating']<-Yelp_rating

df_RF_wilson$G_rating<-df_RF_wilson$Google___of_Reviews
df_RF_wilson[df_RF_wilson$Google___of_Reviews!=0,'G_rating']<-G_rating

df_RF_wilson$FB_rating<-df_RF_wilson$FB___of_Reviews
df_RF_wilson[df_RF_wilson$FB___of_Reviews!=0,'FB_rating']<-FB_rating

#,-FB_Avg_Rating,-Google_Avg_Rating
df_RF3<-df_RF_wilson %>% select(-Yelp_P,-HA_P,-Angi_P,-Google_P,-FB_Avg_Rating)

rf.strata3 <- randomForest(Conversion~.,data=df_RF3,strata=df_RF3$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=2*sqrt(dim(df_RF)[2]),
                          importance=TRUE)
print(rf.strata3)
runtest(df_RF3)
importance(rf.strata3)
varImpPlot(rf.strata3)  

#Wilson result checking
n<-df_RF[df_RF['FB___of_Reviews']!=0,'FB___of_Reviews'][[1]]
ar<-df_RF[df_RF['FB___of_Reviews']!=0,'FB_Avg_Rating'][[1]]
x<-ifelse(n-round((5-ar)*n/2,0)>0,n-round((5-ar)*n/2,0),0)
prop.test(x=x[1],n=n[1])

FB_rating1<-Wilson(df_RF,'FB___of_Reviews','FB_Avg_Rating',c=T)
FB_rating2<-Wilson(df_RF,'FB___of_Reviews','FB_Avg_Rating',c=F)
View(cbind(n,ar,FB_rating1,FB_rating2))

hist(FB_rating1)
hist(FB_rating2)
hist(df_RF$FB_Avg_Rating)

df_RF_wilson1<-df_RF
df_RF_wilson1$FB_rating1<-df_RF_wilson1$FB___of_Reviews
df_RF_wilson1[df_RF_wilson1$FB___of_Reviews!=0,'FB_rating1']<-FB_rating1
df_RF_wilson1$FB_rating2<-df_RF_wilson1$FB___of_Reviews
df_RF_wilson1[df_RF_wilson1$FB___of_Reviews!=0,'FB_rating2']<-FB_rating2
df_RF_wilson1 %>% group_by(Conversion) %>%
  summarise(n = n(),
            FB_cor1=median(FB_rating1),
            FB_cor2=median(FB_rating2),
            FB_no=median(FB_Avg_Rating)) %>%
  mutate(p=round(n/nrow(df_RF_wilson1),2))#too many 0

View(df_core[which(df_core['FB___of_Reviews']==353),])

#Agg+Wilson
df_wilson1<-df_RF %>% mutate(P_num=Google_P+Angi_P+HA_P+Yelp_P+FB_P,
                             Sum_R=Google___of_Reviews+Angi_s___of_Reviews+HA___of_Reviews+Yelp___of_Reviews+FB___of_Reviews,
                             RA_mean=(Google_Avg_Rating+Angi_s_Avg_Rating+HA_Avg_Rating+Yelp_Avg_Rating+FB_Avg_Rating)/P_num)
View(df_wilson1)
All_rating<-Wilson(df_wilson1,'Sum_R','RA_mean',c=F)
summary(All_rating)
summary(df_wilson1$RA_mean)

df_wilson1[df_wilson1$Sum_R!=0,'RA_mean']


#Aggregate to HA
df_agg<-df_RF_wilson  %>% 
  mutate(Review_sum=Google___of_Reviews+Angi_s___of_Reviews+HA___of_Reviews+Yelp___of_Reviews+FB___of_Reviews,
         Avg_Rating_sum=Google_Avg_Rating+Angi_s_Avg_Rating+HA_Avg_Rating+Yelp_Avg_Rating+FB_Avg_Rating,
         diff_week_sum=Google_diff_week+Angi_diff_week+HA_diff_week+Yelp_diff_week+FB_diff_week,
         P_num=Google_P+Angi_P+HA_P+Yelp_P+FB_P,
         Review_mean=Review_sum/P_num,
         Avg_Rating_mean=Avg_Rating_sum/P_num,
         Avg_Rating_wilsonsum=G_rating+Angi_rating+HA_rating+Yelp_rating+FB_rating,
         diff_week_mean=diff_week_sum/P_num,
         All_rating_wilson=0)
All_rating<-Wilson(df_agg,'Review_sum','Avg_Rating_mean',c=F)
summary(All_rating)

df_agg[df_agg$Review_sum!=0,'All_rating_wilson']<-All_rating

df_RF4<-df_agg #%>% select(-Google_P)
rf.strata4 = randomForest(Conversion~.,data=df_RF4,strata=df_RF4$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=2*sqrt(dim(df_RF4)[2]),
                          importance=TRUE)
print(rf.strata4)
result4<-runtest(df_RF4)
result4
as.data.frame(importance(rf.strata4)) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata4)

#Aggregate to HA, drop similar/useless features
#-Google_diff_week,-FB_diff_week,,
#-Angi_s_Avg_Rating,-Yelp_Avg_Rating,
#-FB_rating
df_RF5<-df_agg %>% select(-FB_diff_week,-Angi_diff_week,-Yelp_diff_week,-Google_diff_week,
                          -Google_P,-FB_Avg_Rating,-Yelp___of_Reviews__that_are_not_recommended_,
                          -FB_rating,-FB_P)
rf.strata5 = randomForest(Conversion~.,data=df_RF5,strata=df_RF5$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=2*sqrt(dim(df_RF5)[2]),
                          importance=TRUE)
print(rf.strata5)
result5<-runtest(df_RF5)
result5
as.data.frame(importance(rf.strata5)) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata5)




#para test function
runtest1<-function(df_RF){
  Recall<-NULL
  ACC<-NULL
  Area<-NULL
  Drop<-NULL
  Good<-NULL
  for(i in 1:10){
    nRareSamples = 900 * 0.05
    rf.strata = randomForest(Conversion~.,data=df_RF,strata=df_RF$Conversion,
                             sampsize=c(nRareSamples,nRareSamples),ntree=400,mtry=2*sqrt(dim(df_RF)[2]),
                             importance=TRUE)
    print(rf.strata)
    
    rec<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$byClass['Recall']
    Recall<-c(Recall,rec)
    
    oob<-confusionMatrix(rf.strata$predicted, df_RF$Conversion, mode = "everything", positive="TRUE")$overall['Accuracy']
    ACC<-c(ACC,oob)
    
    a<-auc(roc(rf.strata$votes[,2],df_RF$Conversion))
    Area<-c(Area,a)
    
    names<-row.names(as.data.frame(importance(rf.strata)) %>% arrange(MeanDecreaseAccuracy))[1]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(importance(rf.strata)) %>% arrange(desc(MeanDecreaseAccuracy)))[1:3]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}




#Simple one:with_web,FB_Page_Follows,diff_week_mean,Avg_Rating_mean,State_G,Conversion
library(caret)
df_RF6<-df_agg %>% select(with_web,FB_Page_Follows,diff_week_mean,Avg_Rating_mean,
                          State_G,Conversion)
rf.strata6 = randomForest(Conversion~.,data=df_RF6,strata=df_RF6$Conversion,
                          sampsize=c(nRareSamples,nRareSamples),
                          ntree=400,mtry=2*sqrt(dim(df_RF6)[2]),
                          importance=TRUE)
print(rf.strata6)
result6<-runtest1(df_RF6)
result6
as.data.frame(rf.strata6$importance) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf.strata6)
detach("package:caret", unload = TRUE)

#test-train split
library(caTools)
sample <- sample.split(df_RF6$Conversion, SplitRatio = .75)
train <- subset(df_RF6, sample == TRUE)
test <- subset(df_RF6, sample == FALSE)
dim(train)
dim(test)

rf<-randomForest(Conversion~.,data=train,strata=train$Conversion,
             sampsize=c(nRareSamples,nRareSamples),
             ntree=400,mtry=2*sqrt(dim(train)[2]),
             importance=TRUE)
print(rf)
pred <- predict(rf, newdata=test[-6])
table(test[,6][[1]], pred)

#vote
prob<-predict(rf, newdata=test[-6], type="prob")

#Result
result<-as.data.frame(cbind(test[,6][[1]],pred,prob))
names(result)<-c('actual','pred','prob_F','prob_T')
result$correct<-ifelse(result['actual']==result['pred'],T,F)
result['actual']<-as.factor(ifelse(result['actual']==1,F,T))
result['pred']<-as.factor(ifelse(result['pred']==1,F,T))
#View(result)

# prob hist
library(plotly)
library(tidymodels)
z <- roc_curve(data = result, 'actual', 'prob_F')
z$specificity <- 1 - z$specificity
colnames(z) <- c('threshold', 'tpr', 'fpr')

fig1 <- plot_ly(x= result['prob_F'][[1]], color = result['actual'][[1]], colors = c('blue', 'red'), type = 'histogram', alpha = 0.5, nbinsx = 50) %>%
  layout(barmode = "overlay")
fig1

# TPR vs FPR curve
fig2 <- plot_ly(data = z, x = ~threshold) %>%
  add_trace(y = ~fpr, mode = 'lines', name = 'False Positive Rate', type = 'scatter')%>%
  add_trace(y = ~tpr, mode = 'lines', name = 'True Positive Rate', type = 'scatter')%>%
  layout(title = 'TPR and FPR at every threshold',yaxis = list(title = "Rate"))
fig2 <- fig2 %>% layout(legend=list(title=list(text='<b> Rate </b>')))
fig2

#ROC curve
library(dplyr)
library(ggplot2)
library(plotly)
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

#pr-curve
pdb <- pr_curve(data = result, actual, prob_F)

tit = paste('PR Curve')

fig_pr <-  plot_ly(data = pdb ,x =  ~recall, y = ~precision, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  add_segments(x = 0, xend = 1, y = 1, yend = 0, line = list(dash = "dash", color = 'black'),inherit = FALSE, showlegend = FALSE) %>%
  layout(title = tit, xaxis = list(title = "Recall"), yaxis = list(title = "Precision") )

fig_pr


#adjust classifier threshold
result_new<-result
result_new$pred_new<-as.factor(ifelse(result_new['prob_F']<0.55,T,F))
result_new$correct_new<-ifelse(result_new['actual']==result_new['pred_new'],T,F)
#View(result_new)

table(result_new['actual'][[1]], result_new['pred_new'][[1]])
table(result['actual'][[1]], result['pred'][[1]])

confusionMatrix(result_new['pred_new'][[1]], result_new['actual'][[1]], mode = "prec_recall", positive="TRUE")
confusionMatrix(result['pred'][[1]], result['actual'][[1]], mode = "prec_recall", positive="TRUE")


# trueT_df<-result[(result['actual']==T),]
# trueF_df<-result[(result['actual']==F),]
# FP_df<-result[(result['pred']==T & result['actual']==F),]
# FN_df<-result[(result['pred']==F & result['actual']==T),]
# FP<-nrow(FP_df)/nrow(trueF_df)
# FN<-nrow(FN_df)/nrow(trueF_df)






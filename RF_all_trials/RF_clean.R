library(readr)
#library(caret)
library(AUC)
df_RF <- read_csv("df_RF.csv")
df_RF<-df_RF[-1]

df_RF$Conversion <- as.factor(df_RF$Conversion)
table(df_RF$Conversion)
#Replace all NA into 0
df_RF[is.na(df_RF)] <- 0

#If no profile, all other fields=0
df_RF$Google_Avg_Rating<-ifelse(df_RF$Google_P==F,0,df_RF$Google_Avg_Rating)
df_RF$Google___of_Reviews<-ifelse(df_RF$Google_P==F,0,df_RF$Google___of_Reviews)
df_RF$Yelp___of_Reviews<-ifelse(df_RF$Yelp_P==F,0,df_RF$Yelp___of_Reviews)
df_RF$Yelp_Avg_Rating<-ifelse(df_RF$Yelp_P==F,0,df_RF$Yelp_Avg_Rating)
df_RF$FB_P<-ifelse(df_RF$FB___of_Reviews!=0,T,F)

#Set nRareSamples,ntree,mtry
#,ntree=450,mtry=2*sqrt(dim(df_RF)[2])
library(caret)
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
    
    names<-row.names(as.data.frame(rf.strata$importance) %>% arrange(MeanDecreaseAccuracy))[1]
    Drop<-c(Drop,names)
    
    top<-row.names(as.data.frame(rf.strata$importance) %>% arrange(desc(MeanDecreaseAccuracy)))[1:3]
    Good<-c(Good,top)
  }
  list(summary(Recall),summary(ACC),summary(Area),
       table(Drop)[order(table(Drop))],table(Good)[order(table(Good))])
}

#Aggregate to HA
df_agg<-df_RF  %>% 
  mutate(Review_sum=Google___of_Reviews+Angi_s___of_Reviews+HA___of_Reviews+Yelp___of_Reviews+FB___of_Reviews,
         Avg_Rating_sum=Google_Avg_Rating+Angi_s_Avg_Rating+HA_Avg_Rating+Yelp_Avg_Rating+FB_Avg_Rating,
         diff_week_sum=Google_diff_week+Angi_diff_week+HA_diff_week+Yelp_diff_week+FB_diff_week,
         P_num=Google_P+Angi_P+HA_P+Yelp_P+FB_P,
         Review_mean=Review_sum/P_num,
         Avg_Rating_mean=Avg_Rating_sum/P_num,
         diff_week_mean=diff_week_sum/P_num)

df_agg$with_web<-ifelse(df_agg$with_web==F,0,1)
State_num<-function(x){
  if(x['State_G'] =='High'){
    out<-3
  }else if(x['State_G'] =='Mid'){
    out<-2
  }else{
    out<-1
  }
}
df_agg$State_G<-apply(df_agg,1,State_num)

write.csv(df_agg,file='df_agg.csv')

#Simple model
nRareSamples =  900 * 0.05
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
                 importance=TRUE,
                 keep.inbag=TRUE)
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
detach("package:pROC", unload = TRUE)

#adjust classifier threshold
result_new<-result
result_new$pred_new<-as.factor(ifelse(result_new['prob_F']<0.55,T,F))
result_new$correct_new<-ifelse(result_new['actual']==result_new['pred_new'],T,F)
#View(result_new)

table(result_new['actual'][[1]], result_new['pred_new'][[1]])
table(result['actual'][[1]], result['pred'][[1]])

library(caret)
confusionMatrix(result_new['pred_new'][[1]], result_new['actual'][[1]], mode = "prec_recall", positive="TRUE")
confusionMatrix(result['pred'][[1]], result['actual'][[1]], mode = "prec_recall", positive="TRUE")
detach("package:caret", unload = TRUE)


#PDP plot
library(pdp)

# Single Variable: yhat=prob_F
pdp<- partial(rf, pred.var = c("diff_week_mean"), chull = TRUE)
plot.pdp <- autoplot(pdp, contour = TRUE)
plot.pdp

pdp1<- partial(rf, pred.var = c("with_web"), chull = TRUE)
plot.pdp1 <- autoplot(pdp1, contour = TRUE)
plot.pdp1

pdp2<- partial(rf, pred.var = c("FB_Page_Follows"), chull = TRUE,trim.outliers=TRUE)
plot.pdp2 <- autoplot(pdp2, contour = TRUE)
plot.pdp2

pdp3<- partial(rf, pred.var = c("Avg_Rating_mean"), chull = TRUE)
plot.pdp3 <- autoplot(pdp3, contour = TRUE)
plot.pdp3

pdp4<- partial(rf, pred.var = c("State_G"), chull = TRUE)
plot.pdp4 <- autoplot(pdp4, contour = TRUE)
plot.pdp4

#Tree Interpeter
library(tree.interpreter)
tidy.RF <- tidyRF(rf, train[, -6], train[, 6])

tree_interp<-function(top){
  feature_con<-NULL
  feature_freq<-NULL
  for(i in 1:nrow(test)){
    out<-featureContrib(tidy.RF, test[i, -6])[,2,]
    feature_con<-rbind(feature_con,out)
    
    freq<-names(sort(abs(out),decreasing = T))[top]
    feature_freq<-c(feature_freq,freq)
  }
  list(feature_con,feature_freq)
}

result_feature<-cbind(result,tree_interp(1)[[1]])
result_feature$sum<-result_feature$with_web+result_feature$FB_Page_Follows+result_feature$diff_week_mean+
  result_feature$Avg_Rating_mean+result_feature$State_G+0.5

result_FN<-result_feature %>% filter(correct==F) %>% filter(pred==F) %>% arrange(prob_T)
#View(result_FN)

result_FP<-result_feature %>% filter(correct==F) %>% filter(pred==T) %>% arrange(prob_F)
#View(result_FP)

#Most frequent deterministic feature
#diff_week_mean>State_G>Avg_Rating_mean>FB_Page_Follows>with_web
tree_interp(1)[[2]]
f1<-sort(table(tree_interp(1)[[2]]),decreasing = T)
f2<-sort(table(tree_interp(2)[[2]]),decreasing = T)
f3<-sort(table(tree_interp(3)[[2]]),decreasing = T)

#rbind('Top1'=f1,'Top2'=f2,'Top3'=f3)

summary(result_feature$diff_week_mean) #negative contribution dominates
summary(result_feature$State_G) #balanced contribution
summary(result_feature$Avg_Rating_mean) #balanced contribution
summary(result_feature$FB_Page_Follows) #negative contribution dominates
summary(result_feature$with_web) #balanced contribution

#library(shapper)
#shapper::install_shap()
library("DALEX")
exp_rf <- explain(rf, data = train[,-6], y = as.numeric(train[,6][[1]]))
#shap(exp_rf, new_observation = test[shap_index,-6])

shap_individual<-function(shap_index){
  shap_1<-predict_parts(explainer = exp_rf,new_observation = test[shap_index,-6],type='shap')
  plot(shap_1,show_boxplots = FALSE) 
}

#from test to all
df_raw <- read_csv("df2.csv",show_col_types = FALSE)
df_core <- read_csv("df_core.csv",show_col_types = FALSE)
df_shap_all<-df_core
df_shap_all$index<-1:nrow(df_shap_all)

df_shap_sim<-df_RF6
df_shap_sim$index<-1:nrow(df_shap_sim)
             
#check Top5 FN error individual 
#704:all negative, but convert(have profile in many platform but no data, reviews 10 years ago)
error_index<-704
shap_individual(error_index)  
#View(df_shap_all[sample==F,][error_index,])
#View(df_shap_sim[sample==F,][error_index,])
error_id<-df_shap_all[sample==F,][error_index,'Id'][[1]]
#View(df_raw[df_raw['Id']==error_id,])
              
#496:have profile in many platform but no data, new reviews 7/20
error_index<-496
shap_individual(error_index)  
#View(df_shap_all[sample==F,][error_index,])
#View(df_shap_sim[sample==F,][error_index,])
error_id<-df_shap_all[sample==F,][error_index,'Id'][[1]]
#View(df_raw[df_raw['Id']==error_id,])

#837/848:HA only, no data
error_index<-837
shap_individual(error_index)  
#View(df_shap_all[sample==F,][error_index,])
#View(df_shap_sim[sample==F,][error_index,])
error_id<-df_shap_all[sample==F,][error_index,'Id'][[1]]
#View(df_raw[df_raw['Id']==error_id,])

#595:only HA(1 review), no web, low state(Minneapolis)
error_index<-595
shap_individual(error_index)  
#View(df_shap_all[sample==F,][error_index,])
#View(df_shap_sim[sample==F,][error_index,])
error_id<-df_shap_all[sample==F,][error_index,'Id'][[1]]
#View(df_raw[df_raw['Id']==error_id,])



#check FP error individual 
#184:HA/Google, mid size good review, active
error_index<-184
shap_individual(error_index)  
#View(df_shap_all[sample==F,][error_index,])
#View(df_shap_sim[sample==F,][error_index,])
error_id<-df_shap_all[sample==F,][error_index,'Id'][[1]]
#View(df_raw[df_raw['Id']==error_id,])

#Correlation heatmap
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(df_RF6 %>% select(-Conversion)))
cor(df_RF6 %>% select(-Conversion))
summary(df_RF6)

#KNN
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
dia_nor <- as.data.frame(lapply(df_RF6[,-6], nor))
ran <- sample(1:nrow(df_RF6),0.75 * nrow(df_RF6))
dia_train <- dia_nor[ran,]
dia_test <- dia_nor[-ran,]
dia_target <- as.factor(df_RF6[ran,6][[1]])
library(class)
pr <- knn(dia_train,dia_test,cl=dia_target,k=3)
pr
table(pr,as.factor(df_RF6[-ran,6][[1]]))              


# #tSNE
# library(Rtsne)
# tsne_out <- Rtsne(unique(df_RF6),pca=FALSE, theta=0.0)
# plot(tsne_out$Y,col=unique(df_RF6)$Conversion, asp=1)

##PCA!!!!!!
library(plotly)

#remove outliers
df_pca<-df_RF6
df_pca$FB_Page_Follows<-ifelse(df_pca$FB_Page_Follows>2000,2000,df_pca$FB_Page_Follows)
df_pca$diff_week_mean<-ifelse(df_pca$diff_week_mean>1000,1000,df_pca$diff_week_mean)


#After
library(plotly)
library(stats)
X <- subset(df_pca, select = c(FB_Page_Follows,diff_week_mean,Avg_Rating_mean,with_web,State_G))
prin_comp <- prcomp(X,scale. = T)
explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
explained_variance_ratio <- 100 * explained_variance_ratio
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, df_RF6$Conversion)
components$PC2 <- -components$PC2

fig <- plot_ly(components, x = ~PC1, y = ~PC2, color = ~df_pca$Conversion, 
               colors = c('#636EFA','#EF553B'), type = 'scatter', mode = 'markers')%>%
  layout(
    legend=list(title=list(text='color')),
    plot_bgcolor='#e5ecf6',
    xaxis = list(
      title = paste("PC1",as.character(explained_variance_ratio['PC1'])),
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor='#ffff'),
    yaxis = list(
      title = paste("PC2",as.character(explained_variance_ratio['PC2'])),
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor='#ffff'))
fig

prin_comp

              
df_agg_noFB <- read_csv("df_agg_noFB.csv")
df_agg <- read_csv("df_agg.csv")

#select features
df_L1<-df_agg %>% select(with_web,FB_Page_Follows,diff_week_mean,Avg_Rating_mean,
                  State_G,Conversion)

df_L1_noFB<-df_agg_noFB %>% select(with_web,HA_diff_week,HA_Avg_Rating,State_G,P_G,
                       Conversion)

#CHECK Assumptions: IND, no extreme outliers

#Correlation heatmap:
#p.mat = cor_pmat(df_L1 %>% select(-Conversion)),
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(df_L1 %>% select(-Conversion)),
                       hc.order = TRUE, type = "lower",
                       lab = TRUE)

cor(df_L1 %>% select(-Conversion))

ggcorrplot::ggcorrplot(cor(df_L1_noFB %>% select(-Conversion)),
                       hc.order = TRUE, type = "lower",
                       lab = TRUE,
                       ggtheme = ggplot2::theme_dark())
cor(df_L1_noFB %>% select(-Conversion))


#SMOTE:oversampling, did not converge
library(smotefamily)

runtest3<-function(df_smote,df_test){
  library(AUC)
  res_recall<-NULL
  res_acc<-NULL
  res_auc<-NULL
  for(i in 1:10){
    m1<-glm(Conversion~.,data=df_smote,family="binomial")
    prod.m1<-predict(m1,df_test,type="response") #numerical
    
    preb.m1<-rep("TRUE",length(prod.m1))
    preb.m1[prod.m1<0.5]<-"FALSE" #categorical
    print(table(df_test$Conversion,preb.m1))
    
    recall<-table(preb.m1,df_test$Conversion)[2,2]/table(df_test$Conversion)[2]
    res_recall<-c(res_recall,recall)
    
    acc<-1-mean(preb.m1!=df_test$Conversion) #ACC
    res_acc<-c(res_acc,acc)
    
    auc<-auc(roc(predictions=prod.m1,labels=as.factor(df_test$Conversion)))
    res_auc<-c(res_auc,auc)
  }
  list(summary(res_recall),summary(res_acc),summary(res_auc))
}

#train-test split, only smote train
library(caTools)
df_split<-df_L1_noFB
sample <- sample.split(df_split$Conversion, SplitRatio = .75)
train <- subset(df_split, sample == TRUE)
test <- subset(df_split, sample == FALSE)
dim(train)
dim(test)

#set parameters:
res_smote<-SMOTE(train[,-dim(train)[2]],as.vector(train$Conversion),K=5,dup_size=0)
df_L1_noFB_smote<-res_smote$data[,-dim(train)[2]]
df_L1_noFB_smote$Conversion <- as.factor(res_smote$data$class)
#View(df_L1_noFB_smote)

#train on smote-train, test on non-smote test
table(test$Conversion)
runtest3(df_L1_noFB_smote,test) #depend on test-train splitting, unstable


#APPLY
m1<-glm(Conversion~.,data=df_L1_noFB_smote,family="binomial")
prod.m1.train<-predict(m1,type="response")
prod.m1<-predict(m1,test,type="response") #numerical

preb.m1<-rep("TRUE",length(prod.m1))
preb.m1[prod.m1<0.5]<-"FALSE" #categorical
print(table(test$Conversion,preb.m1))

summary(m1)

#check outliers
summary(df_L1)
summary(df_L1_noFB)

#influential points
plot(m1, which = 4, id.n = 10)
library(broom)
library(dplyr)
model.data <- augment(m1) %>% mutate(index = 1:n()) 
model.data %>% top_n(10, .cooksd)

library(ggplot2)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Conversion), alpha = .5) +
  theme_bw()

#Modify training data:df_L1_noFB_smote
df_L11_noFB_smote<-model.data %>% 
  filter(abs(.std.resid) < 2.5) %>% 
  select(Conversion,with_web,HA_diff_week,HA_Avg_Rating,State_G,P_G)
dim(df_L1_noFB_smote)
dim(df_L11_noFB_smote)
runtest3(df_L11_noFB_smote,test)

m2<-glm(Conversion~.,data=df_L11_noFB_smote,family="binomial")
prod.m2.train<-predict(m2,type="response")
prod.m2<-predict(m2,test,type="response") #numerical

preb.m2<-rep("TRUE",length(prod.m2))
preb.m2[prod.m2<0.5]<-"FALSE" #categorical
print(table(test$Conversion,preb.m2))

summary(m2)


#odds linear: might need regroup
library(tidyr)
prod.model<-prod.m2
mydata <- test %>% select(HA_diff_week,HA_Avg_Rating) %>%
  mutate(logit = log(prod.model/(1-prod.model))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#vif:exceeds 5 or 10 indicates a problematic amount of collinearity
car::vif(m1)

#regroup HA_Avg_Rating
df_L2_noFB_smote<-df_L1_noFB_smote
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
df_L2_noFB_smote$HA_rating_G<-apply(df_L2_noFB_smote,1,HA_group)
df_L2_noFB$HA_rating_G<-apply(df_L2_noFB,1,HA_group)
test$HA_rating_G<-apply(test,1,HA_group)

cn<-table(df_L2_noFB$Conversion)[2]/nrow(df_L2_noFB)
df_L2_noFB %>% group_by(HA_rating_G) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))

runtest3(df_L2_noFB_smote %>% select(-HA_Avg_Rating),test) #depend on test-train splitting

m3<-glm(Conversion~.,data=df_L2_noFB_smote,family="binomial")
prod.m3.train<-predict(m3,type="response")
prod.m3<-predict(m3,test,type="response") #numerical

preb.m3<-rep("TRUE",length(prod.m3))
preb.m3[prod.m3<0.5]<-"FALSE" #categorical
print(table(test$Conversion,preb.m3))

#odds linear: might need regroup
library(tidyr)
prod.model<-prod.m3
mydata <- test %>% select(HA_rating_G,HA_diff_week) %>%
  mutate(logit = log(prod.model/(1-prod.model))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#plotting
df_rating_test<-data.frame(truth=test$Conversion,predicted=prod.m1,HA_Avg_Rating=test$HA_Avg_Rating)
ggplot(df_rating_test,aes(HA_Avg_Rating,predicted))+
  geom_point(aes(colour=truth))

df_rating_train<-data.frame(truth=df_L1_noFB_smote$Conversion,predicted=prod.m1.train,HA_Avg_Rating=df_L1_noFB_smote$HA_Avg_Rating)
ggplot(df_rating_train,aes(HA_Avg_Rating,predicted))+
  geom_point(aes(colour=truth))

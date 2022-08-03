library(readr)
library(dplyr)
library(tidyr)
df_raw <- read_csv("df2.csv",show_col_types = FALSE)

df_raw$Conversion<-ifelse(df_raw$lead_status=="Convert",TRUE,FALSE)
df_raw$index<-rownames(df_raw)

##I. Data Cleaning
#1.1 Has converted time, but not shown in lead_status, count as convert? or remove?
df_raw[df_raw$Id=='00Q3q00001WqdPcEAJ',
       c('index','Id','business','Category','lead_status','lead_converted_time')]
df<-df_raw

#1.2 Same Id: 30*2obs
# same Id, different business name/info(00Q3q00001XNyeOEAT);
# same Id, different category/indstry(00Q3q00001Wqxz8EAB)
#View(df_raw[df_raw$Id=='00Q3q00001XNyeOEAT',])
sum(duplicated(df_raw$Id)) #30
df_same_Id<-df_raw[df_raw$Id %in% df_raw[duplicated(df_raw$Id),'Id'][[1]],] %>% 
  select(index,Id,business,industry,Category,lead_status,lead_number_of_attempts,Date_Prospected,DateAdded,sales_outcome_date_mt) %>% 
  arrange(Id)
nrow(df_same_Id) #60
#View(df_same_Id)

#Filtering critiria: 30*2->13 obs
# Date_Prospected==max(Date_Prospected): remove 32 obs(include 2 NA in Date_Prospected)
# Date_Prospected<sales_outcome_date_mt: further remove 15 obs
# Same sales_outcome_date_mt, Different Date_Prospected
#Note:There are 511 obs from total 3897 obs(14%) with Date_Prospected later than sales_outcome_date_mt.
df_same_id_left<-df_raw[df_raw$Id %in% df_raw[duplicated(df_raw$Id),'Id'][[1]],] %>% 
  group_by(Id) %>% 
  filter(as.Date(Date_Prospected, format="%m/%d/%Y") < sales_outcome_date_mt) %>% 
  filter(Date_Prospected==max(Date_Prospected)) %>% 
  arrange(business)
#View(df_same_id_left)

#Clean by index
same_id_index<-df_raw[df_raw$Id %in% df_raw[duplicated(df_raw$Id),'Id'][[1]],'index'][[1]]
same_id_index_left<-df_same_id_left[,'index'][[1]]
same_id_index_remove<-setdiff(same_id_index, same_id_index_left)
df<-df[!df$index %in% same_id_index_remove,]

#1.3 Same business name, different id: 13*2 obs
#All with same category(A Gardenscape)
sum(duplicated(df$business)) #13
sum(duplicated(df[,c('Category','business')])) #13

df_same_name<-df[df$business %in% df[duplicated(df[,c('business')]),'business'][[1]],] %>% 
  select(Id,business,industry,Category,lead_status,lead_number_of_attempts,Date_Prospected,DateAdded,sales_outcome_date_mt) %>% 
  arrange(business)
nrow(df_same_name) #26
#View(df_same_name)

#Filtering critiria: 13*2->10 obs
# sales_outcome_date_mt==max(sales_outcome_date_mt): remove 13 obs(6 NA in Date_Prospected)
# Date_Prospected<sales_outcome_date_mt: further remove 3 obs
# Same Date_Prospected, different sales_outcome_date_mt
#Note:There are 511 obs from total 3897 obs(14%) with Date_Prospected later than sales_outcome_date_mt.
df_same_name_left<-df[df$business %in% df[duplicated(df$business),'business'][[1]],] %>% 
  group_by(business) %>% 
  filter(as.Date(Date_Prospected, format="%m/%d/%Y") < sales_outcome_date_mt) %>% 
  filter(sales_outcome_date_mt==max(sales_outcome_date_mt)) %>% 
  select(index,Id,business,industry,Category,lead_status,lead_number_of_attempts,Date_Prospected,DateAdded,sales_outcome_date_mt) %>% 
  arrange(business)
#View(df_same_name_left)

#Clean by index
same_name_index<-df[df$business %in% df[duplicated(df[,c('business')]),'business'][[1]],'index'][[1]]
same_name_index_left<-df_same_name_left[,'index'][[1]]
same_name_index_remove<-setdiff(same_name_index, same_name_index_left)
df<-df[!df$index %in% same_name_index_remove,]

#1.4 After Cleaning Check
#now 3834 obs
nrow(df_raw)-nrow(df) #Remove 63 obs
sum(df_raw$Conversion)-sum(df$Conversion) #remove 12 conversion
sum(df_raw$Conversion)/nrow(df_raw) #Before: 7.36%
sum(df$Conversion)/nrow(df) #After: 7.17%

#duplicate
sum(duplicated(df$Id))
sum(duplicated(df$business))



##II. Conversion across platform
#2.1 Table Check
#Profile feature within context: Larger diff power
cn<-sum(df$Conversion)/nrow(df) #0.072

with_Angi<-df[which(!is.na(df$Angi_s_Profile_Link)),]
sum(duplicated(with_Angi$Angi_s_Profile_Link)) #1
#Turn 1 obs's Angi Profile Link to NA
Angi_error<-with_Angi[with_Angi$Angi_s_Profile_Link %in% 
                 with_Angi[duplicated(with_Angi$Angi_s_Profile_Link),'Angi_s_Profile_Link'][[1]],]
with_Angi[with_Angi$index=='2972', 'Angi_s_Profile_Link']<-NA
sum(duplicated(with_Angi$Angi_s_Profile_Link)) #Now,0
df$Angi_P<-!is.na(df$Angi_s_Profile_Link)

with_HA<-df[which(!is.na(df$HA_Profile_Link)),]
sum(duplicated(with_HA$HA_Profile_Link)) 
df$HA_P<-!is.na(df$HA_Profile_Link)

with_Yelp<-df[which(!is.na(df$Yelp_Profile_Link)),]
sum(duplicated(with_Yelp$Yelp_Profile_Link))
df$Yelp_P<-!is.na(df$Yelp_Profile_Link)

with_G<-df[df$Google_Search_Side_Profile__Y_N_,]
df$Google_P<-ifelse(is.na(df$Google_Search_Side_Profile__Y_N_),FALSE,df$Google_Search_Side_Profile__Y_N_)
summary(df$Google_P)

df_P<-df %>% group_by(Angi_P,HA_P,Yelp_P) %>%
  summarise(n = n(), c=sum(Conversion)) 
#Single Profile feature
A<-c(Angi_P=T,HA_P=NA,Yelp_P=NA,
     n=nrow(with_Angi),c=sum(with_Angi$Conversion))
B<-c(Angi_P=NA,HA_P=T,Yelp_P=NA,
     n=nrow(with_HA),c=sum(with_HA$Conversion))
C<-c(Angi_P=NA,HA_P=NA,Yelp_P=T,
     n=nrow(with_Yelp),c=sum(with_Yelp$Conversion))
df_P1<-rbind(df_P,A,B,C) %>% mutate(pc=round(c/n*100,2), all=round(cn*100,2), diff=pc-all) %>% 
  arrange(desc(pc))
#View(df_P1)

#2.2 Grouping
#Combine into 1 feature
Profile_G<-function(x){
  if(x['Angi_P']=="FALSE" & x['Yelp_P']=="TRUE"){
    out<-'High'
  }else if(x['Angi_P']=="TRUE" & x['HA_P']=="FALSE" & x['Yelp_P']=="FALSE"){
    out<-'Low'
  }else if(x['Angi_P']=="TRUE" & x['HA_P']=="TRUE" & x['Yelp_P']=="TRUE"){
    out<-'Low'
  }else if(x['Angi_P']=="TRUE" & x['HA_P']=="TRUE" & x['Yelp_P']=="FALSE"){
    out<-'Low'
  }else{
    out<-'Mid'
  }
}
df$P_G<-apply(df,1,Profile_G)

df_P1_G<-df %>% group_by(P_G) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))
#View(df_P1_G)
summary(aov(df$Conversion~df$P_G)) #sig, different from binary feature?
summary(aov(df$Conversion~df$Angi_P)) 
summary(aov(df$Conversion~df$HA_P)) 
summary(aov(df$Conversion~df$Yelp_P)) 

chisq.test(df$Conversion,df$P_G)
chisq.test(df$Conversion,df$Angi_P)

#Seperate into 2 features
df_P1_G2<-spread(df,key=P_G,value = Id)
df_P1_G2$High<-ifelse(is.na(df_P1_G2$High),FALSE,TRUE)
df_P1_G2$Low<-ifelse(is.na(df_P1_G2$Low),FALSE,TRUE)
#View(df_P1_G2)
summary(aov(df_P1_G2$Conversion~df_P1_G2$High)) 
summary(aov(df_P1_G2$Conversion~df_P1_G2$Low)) 



##III. Occupation Grouping
#3.1 By indsutry
df_industry<-df %>% group_by(industry) %>%
  summarise(n = n(), c=sum(Conversion)) %>% 
  mutate(pc=round(c/n*100,2),pc_all=round(cn*100,2)) %>% 
  arrange(desc(pc))
#View(df_industry)

#1 occupation to 3 Industry?Home Electronics Specialist?Align with Category more?
df_industry_oc<-df %>% group_by(industry,Category) %>%
  summarise(n = n(), c=sum(Conversion)) %>% 
  mutate(pc=round(c/n*100,2),pc_all=round(cn*100,2)) %>% 
  arrange(desc(pc))
df_industry_oc<-merge(df_industry_oc,df_industry[c('industry','pc')],by='industry')
names(df_industry_oc)=c('industry','Category','n','c','pc_oc','pc_all','pc_industry')
df_industry_oc$diff<-df_industry_oc$pc_oc-df_industry_oc$pc_industry
#View(df_industry_oc)

#occupation vs category
cn<-sum(df$Conversion)/nrow(df)
df_oc<-df %>% group_by(occupation,conversion_rate) %>%
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2)) %>% 
  arrange(desc(conversion_rate))
df_oc<-cbind(df_oc,rank_all=1:nrow(df_oc))
df_oc<-cbind(df_oc %>% arrange(desc(pc)),rank_pc=1:nrow(df_oc))
df_oc$rank_diff<-abs(df_oc$rank_all-df_oc$rank_pc)
df_oc$rank_sum<-df_oc$rank_all+df_oc$rank_pc

df_oc_category<-df %>% group_by(occupation,Category) %>%
  summarise(n = n(), c=sum(Conversion)) %>% 
  mutate(pc=round(c/n*100,2),pc_all=round(cn*100,2)) %>% 
  arrange(desc(pc))
df_oc_category<-merge(df_oc_category,df_oc[c('occupation','pc')],by='occupation')
names(df_oc_category)=c('occupation','Category','n','c','pc','pc_all','pc_occupation')
#View(df_oc_category)
write.csv(df_oc_category,file='df_oc_category.csv')


#3.2 Indsutry vs Platform
library(tidyverse)
Angi<-with_Angi %>% group_by(industry) %>%
  summarise(n = n(), c=sum(Conversion),R=median(Angi_s___of_Reviews,na.rm=T),RA=median(Angi_s_Avg_Rating,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_Angi),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))

HA<-with_HA %>% group_by(industry) %>% 
  summarise(n = n(), c=sum(Conversion),R=median(HA___of_Reviews,na.rm=T),RA=median(HA_Avg_Rating,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_HA),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))

Yelp<-with_Yelp %>% group_by(industry) %>% 
  summarise(n = n(), c=sum(Conversion),R=median(Yelp___of_Reviews,na.rm=T),RA=median(Yelp_Avg_Rating,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_Yelp),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))

list<-list(Angi,HA,Yelp)
df_industry_P<-list %>% reduce(full_join, by='industry') 
names(df_industry_P)=c('industry',
                       'Angi.n','Angi.c','Angi.R','Angi.RA','Angi.p','Angi.pc',
                       'HA.n','HA.c','HA.R','HA.RA','HA.p','HA.pc',
                       'Yelp.n','Yelp.c','Yelp.R','Yelp.RA','Yelp.p','Yelp.pc')
df_industry_P<-merge(df_industry_P,df_industry[c('industry','pc')],by='industry')
#View(df_industry_P)


#Add FB, IG, website:1/0
#check clustering requirement

##IV. EDA of FB and IG
with_FB<-df[which(!is.na(df$FB_URL)),]
sum(duplicated(with_FB$FB_URL)) #0
df$FB_P<-!is.na(df$FB_URL)

with_IG<-df[which(!is.na(df$IG_URL)),]
sum(duplicated(with_IG$IG_URL)) #5, potential error here
df$IG_P<-!is.na(df$IG_URL)

#4.1 FB/IG vs industry
FB<-with_FB %>% group_by(industry) %>%
  summarise(n = n(), c=sum(Conversion),
            R=median(FB___of_Reviews,na.rm=T),RA=median(FB_Avg_Rating,na.rm=T),
            Like=median(FB_Page_Likes,na.rm=T),Follows=median(FB_Page_Follows,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_FB),2),pc=round(c/n*100,2)) %>%
  arrange(desc(pc))
#View(FB)

IG<-with_IG %>% group_by(industry) %>%
  summarise(n = n(), c=sum(Conversion),
            Follows=median(IG_Follows,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_IG),2),pc=round(c/n*100,2)) %>%
  arrange(desc(pc))
#View(IG)

#4.2 FB/IG vs Conversion
FB_cgroup<-with_FB %>% group_by(Conversion) %>%
  summarise(n = n(),
            R=median(FB___of_Reviews,na.rm=T),RA=median(FB_Avg_Rating,na.rm=T),
            Like=median(FB_Page_Likes,na.rm=T),Follows=median(FB_Page_Follows,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_FB),2))
#View(FB_cgroup)

IG_cgroup<-with_IG %>% group_by(Conversion) %>%
  summarise(n = n(),
            Follows=median(IG_Follows,na.rm=T)) %>%
  mutate(p=round(n/nrow(with_IG),2))
#View(IG_cgroup)

##V. EDA of Lastest Review Date
#View(df %>% select(FB_Latest_Review_Date,DateAdded,Yelp_diff_week))
df$Angi_diff_week<-ifelse(is.na(df$Angi_Lastest_Review_Date),NA, 
                        difftime(df$DateAdded, df$Angi_Lastest_Review_Date,units = "weeks"))
summary(df$Angi_diff_week) #there is 1 negative

df$HA_diff_week<-ifelse(is.na(df$HA_Lastest_Review_Date),NA, 
                        difftime(df$DateAdded, df$HA_Lastest_Review_Date,units = "weeks"))
summary(df$HA_diff_week)

df$Yelp_diff_week<-ifelse(is.na(df$Yelp__Lastest_Review_Date),NA, 
                        difftime(df$DateAdded, df$Yelp__Lastest_Review_Date,units = "weeks"))
summary(df$Yelp_diff_week) #there are 2 negative

df$FB_diff_week<-ifelse(is.na(df$FB_Latest_Review_Date),NA, 
                          difftime(df$DateAdded, df$FB_Latest_Review_Date,units = "weeks"))
summary(df$FB_diff_week) #there are 2 negative

df$Google_diff_week<-ifelse(is.na(df$Google_Lastest_Review__XXX_months_ago_),NA, 
                        df$Google_Lastest_Review__XXX_months_ago_*4)
summary(df$Google_diff_week) #there is a extreme large one

df %>% group_by(Conversion) %>%
  summarise(n = n(),
            Angi_diff=median(Angi_diff_week,na.rm=T),
            HA_diff=median(HA_diff_week,na.rm=T),
            Yelp_diff=median(Yelp_diff_week,na.rm=T),
            FB_diff=median(FB_diff_week,na.rm=T),
            G_diff=median(Google_diff_week,na.rm=T)) %>%
  mutate(p=round(n/nrow(df),2))

df %>% group_by(industry) %>%
  summarise(n = n(),
            c=sum(Conversion),
            Angi_diff=median(Angi_diff_week,na.rm=T),
            HA_diff=median(HA_diff_week,na.rm=T),
            Yelp_diff=median(Yelp_diff_week,na.rm=T),
            FB_diff=median(FB_diff_week,na.rm=T),
            G_diff=median(Google_diff_week,na.rm=T)) %>%
  mutate(p=round(n/nrow(df),2),pc=round(c/n*100,2))


#Group state
State_G<-function(x){
  if(x['State'] %in% c("UT","SC","MA","CA","TN","MO","FL")){
    out<-'High'
  }else if(x['State'] %in% c("RI","NJ","DE","PA","AZ","OH","IL",'MN')){
    out<-'Low'
  }else{
    out<-'Mid'
  }
}
df$State_G<-apply(df,1,State_G)

df %>% group_by(State_G) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))

#Stand Along Website
df$with_web<-ifelse(is.na(df$Stand_Alone_Website),FALSE,TRUE)
summary(df$with_web) 
df %>% group_by(with_web) %>%
  summarise(n = n(),
            c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(df),2),pc=round(c/n*100,2))
#Website vs Profile group=no interaction effect
df %>% group_by(with_web,P_G) %>%
  summarise(n = n(),
            c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(df),2),pc=round(c/n*100,2))

write.csv(df,file='df_clean.csv')
#Narrow down core features

df_core<-df %>% select(Id, Conversion,industry,Category,with_web,P_G,occupation,State_G,
              Google_P,Google___of_Reviews,Google_Avg_Rating,Google_diff_week,
              Angi_P,Angi_s___of_Reviews,Angi_s_Avg_Rating,Angi_diff_week,
              HA_P,HA___of_Reviews,HA_Avg_Rating,HA_diff_week,
              Yelp_P,Yelp___of_Reviews,Yelp_Avg_Rating,Yelp_diff_week,Yelp___of_Reviews__that_are_not_recommended_,
              FB_P,FB___of_Reviews,FB_Avg_Rating,FB_diff_week,FB_Page_Likes,FB_Page_Follows,
              IG_P,IG_Follows)
#View(df_core) #30 features
summary(df_core)

#Removing Outliers and NA
df_core[which.max(df_core$Google_Avg_Rating),'Google_Avg_Rating']<-4.6
df_core[which.max(df_core$Google_diff_week),'Google_diff_week']<-8
df_core[which.max(df_core$Google_diff_week),'Google_diff_week']<-96
df_core[which.max(df_core$Angi_diff_week),'Angi_diff_week']<-179
df_core[which.max(df_core$Angi_diff_week),'Angi_diff_week']<-380
df_core[which.max(df_core$Angi_diff_week),'Angi_diff_week']<-860
df_core[which.max(df_core$Yelp_Avg_Rating),'Yelp_Avg_Rating']<-4
df_core[which.max(df_core$FB_Avg_Rating),'FB_Avg_Rating']<-5
df_core[which.max(df_core$FB_Avg_Rating),'FB_Avg_Rating']<-4.7
df_core[which.max(df_core$FB_Avg_Rating),'FB_Avg_Rating']<-4.7
df_core[which.max(df_core$FB_Avg_Rating),'FB_Avg_Rating']<-NA
df_core[which.max(df_core$FB_diff_week),'FB_diff_week']<-NA

df[which.max(df_core$FB_Page_Likes),c('FB_URL','DateAdded')]

write.csv(df_core,file='df_core.csv')
df_numeric<-df_core %>% select(Google___of_Reviews,Google_Avg_Rating,Google_diff_week,
                      Angi_s___of_Reviews,Angi_s_Avg_Rating,Angi_diff_week,
                       HA___of_Reviews,HA_Avg_Rating,HA_diff_week,
                       Yelp___of_Reviews,Yelp_Avg_Rating,Yelp_diff_week,
                       FB___of_Reviews,FB_Avg_Rating,FB_diff_week,FB_Page_Likes,FB_Page_Follows,
                       IG_Follows)


#MODELING: RF
library(randomForest)
df_RF<-df_core %>% select(Conversion,industry,Category,with_web,P_G,occupation,State_G,
                          Google_P,Google___of_Reviews,Google_Avg_Rating,Google_diff_week,
                          Angi_P,Angi_s___of_Reviews,Angi_s_Avg_Rating,Angi_diff_week,
                          HA_P,HA___of_Reviews,HA_Avg_Rating,HA_diff_week,
                          Yelp_P,Yelp___of_Reviews,Yelp_Avg_Rating,Yelp_diff_week,Yelp___of_Reviews__that_are_not_recommended_,
                          FB_P,FB___of_Reviews,FB_Avg_Rating,FB_diff_week,FB_Page_Likes,FB_Page_Follows,
                          IG_P,IG_Follows)
write.csv(df_RF,file='df_RF.csv')








#Correlation between variables
df_numeric<-df_RF %>% select(Google___of_Reviews,Google_Avg_Rating,Google_diff_week,
                             Angi_s___of_Reviews,Angi_s_Avg_Rating,Angi_diff_week,
                             HA___of_Reviews,HA_Avg_Rating,HA_diff_week,
                             Yelp___of_Reviews,Yelp_Avg_Rating,Yelp_diff_week,
                             FB___of_Reviews,FB_Avg_Rating,FB_diff_week,FB_Page_Likes,FB_Page_Follows,
                             IG_Follows)
df_review<-df_RF %>% select(Google___of_Reviews,
                            Angi_s___of_Reviews,
                            HA___of_Reviews,
                            Yelp___of_Reviews,
                            FB___of_Reviews)
df_avg<-df_RF %>% select(Google_Avg_Rating,
                         Angi_s_Avg_Rating,
                         HA_Avg_Rating,
                         Yelp_Avg_Rating,
                         FB_Avg_Rating)
df_review_avg<-df_RF %>% select(Google___of_Reviews,Google_Avg_Rating,
                                Angi_s___of_Reviews,Angi_s_Avg_Rating,
                                HA___of_Reviews,HA_Avg_Rating,
                                Yelp___of_Reviews,Yelp_Avg_Rating,
                                FB___of_Reviews,FB_Avg_Rating)
library(psych)
pairs.panels(df_review,
             gap = 0,
             bg = c("red", "blue")[df_RF$Conversion],
             pch=21)
pairs.panels(df_avg,
             gap = 0,
             bg = c("red", "blue")[df_RF$Conversion],
             pch=21)
pairs.panels(df_review_avg,
             gap = 0,
             bg = c("red", "blue")[df_RF$Conversion],
             pch=21)

with_HA<-df_RF[df_RF$HA_P,]
cor(with_HA$HA___of_Reviews,with_HA$HA_Avg_Rating)

#Clustering of categories
pc <- prcomp(df_numeric,
             center = TRUE,
             scale. = TRUE)
print(pc)
summary(pc)
pairs.panels(pc$x[,1:8],
             gap = 0,
             bg = c("red", "blue")[df_RF$Conversion],
             pch=21)

clusters <- hclust(dist(df_numeric), method = 'complete')
plot(clusters)
clusterCut <- cutree(clusters, 8)
table(clusterCut, df_RF$Conversion)

View(df_RF[clusterCut==2,])

library(readr)
library(dplyr)
df_raw <- read_csv("df.csv",show_col_types = FALSE)

df_raw$Conversion<-ifelse(df_raw$lead_status=="Convert",TRUE,FALSE)

##O. How to define conversion?
#Has converted time, but not shown in lead_status, count as convert?
df_raw[df_raw$Id=='00Q3q00001WqdPcEAJ',
       c('Id','business','Category','lead_status','lead_converted_time')]
df<-df_raw

##I. How to identify a pro(Id, business, Category)?
#Check same Id
sum(duplicated(df$Id)) #30
df_same_Id<-df[df$Id %in% df[duplicated(df$Id),'Id'][[1]],] %>% 
  select(Id,business,Category,lead_status,lead_number_of_attempts,sales_outcome_date_mt) %>% 
  arrange(business)
nrow(df_same_Id) #60
#View(df_same_Id)

#Check business with same name
sum(duplicated(df$business)) #36
sum(duplicated(df[,c('Category','business')])) #17 with diff category, 19 with same category

df_same_name<-df[df$business %in% df[duplicated(df[,c('Category','business')]),'business'][[1]],] %>% 
       select(Id,business,Category,lead_status,lead_number_of_attempts,sales_outcome_date_mt) %>% 
       arrange(business)
nrow(df_same_name) #38
#View(df_same_name)

#Ambiguous data: save df$business==c('Iron Solutions, LLC')?
#same Id, diff info mostly by diff people
df1<-df %>% 
  filter(Id %in% c('00Q3q00001WsgtHEAR','00Q3q00001ZdQAIEA3', '00Q3q00001Vqyp6EAB', 
                   '00Q3q00001SySeuEAF', '00Q3q00001Zwl9tEAB','00Q3q00001aQ2zjEAC')) %>% 
  arrange(business)
#View(df1)


#different Id, same biz, randomly drop 1 if same info?
df2<-df %>% 
       filter(business %in% c('Specialty Kitchen and Bath, Inc.',
                              'formerly Lumber Liquidators',
                              'Caruso Heating & Air Conditioning, Inc.',
                              'Teckrom, Inc.',
                              'Turf Master Lawn & Landscape',
                              'A Gardenscape',
                              'A New Image Heating & Cooling, Inc.',
                              'Bull Mountain Heating, Air Conditioning & Insulation',
                              'Busen Appliance, Inc',
                              'Eco Home Builders, Inc',
                              'A-AAA Tree Service',
                              'A~Appliance Repair Service',
                              "Valley Oak Maytag Home Appliance Center \n")) %>% 
       arrange(business)

#View(df2)

#Overview of ambiguous data
nrow(rbind(df1,df2))
#View(rbind(df1,df2))

#Remove all ambiguous data in I
df<-df %>% 
  filter(!Id %in% c('00Q3q00001WsgtHEAR','00Q3q00001ZdQAIEA3', '00Q3q00001Vqyp6EAB', 
                        '00Q3q00001SySeuEAF', '00Q3q00001Zwl9tEAB','00Q3q00001aQ2zjEAC')) %>% 
  filter(!business %in% c('Specialty Kitchen and Bath, Inc.',
                         'formerly Lumber Liquidators',
                         'Caruso Heating & Air Conditioning, Inc.',
                         'Teckrom, Inc.',
                         'Turf Master Lawn & Landscape',
                         'A Gardenscape',
                         'A New Image Heating & Cooling, Inc.',
                         'Bull Mountain Heating, Air Conditioning & Insulation',
                         'Busen Appliance, Inc',
                         'Eco Home Builders, Inc',
                         'A-AAA Tree Service',
                         'A~Appliance Repair Service',
                         "Valley Oak Maytag Home Appliance Center \n"))

sum(duplicated(df[,c('Category','business')])) #0
sum(duplicated(df$Id))  #23

#Same Id, different category
idddd<-df[df$business %in% df[duplicated(df[,c('Id')]),'business'][[1]],] %>%
  select(Id,business,occupation,Category,lead_status,lead_number_of_attempts,Date,sales_outcome_date_mt) %>%
  arrange(business)
df[duplicated(df[,c('Id')]),'Id'][[1]]
idddd[['Id']]
View(df[df$business %in% df[duplicated(df[,c('Id')]),'business'][[1]],] %>%
  select(Id,business,occupation,Category,lead_status,lead_number_of_attempts,Date_Prospected,DateAdded,sales_outcome_date_mt) %>%
  arrange(business))

nrow(df_raw)-nrow(df) #remove 38 in total
sum(df_raw$Conversion)-sum(df$Conversion) #remove 5 conversion
sum(df_raw$Conversion)/nrow(df_raw) #Before: 7.36%
sum(df$Conversion)/nrow(df) #After: 7.31%

##II. Occupation vs Conversion rate: Consider both rank_all and rank_pc if segments occupation?
df_oc<-df %>% group_by(occupation,conversion_rate) %>%
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2)) %>% 
  arrange(desc(conversion_rate))
df_oc<-cbind(df_oc,rank_all=1:nrow(df_oc))
df_oc<-cbind(df_oc %>% arrange(desc(pc)),rank_pc=1:nrow(df_oc))
df_oc$rank_diff<-abs(df_oc$rank_all-df_oc$rank_pc)
df_oc$rank_sum<-df_oc$rank_all+df_oc$rank_pc
cor(df_oc$rank_all,df_oc$rank_pc) #-0.05
#View(df_oc)

##III. Profile exist or not across platforms
with_Angi<-df[which(!is.na(df$Angi_s_Profile_Link)),]
sum(duplicated(with_Angi$Angi_s_Profile_Link)) #same Id, diff category
df$Angi_P<-!is.na(df$Angi_s_Profile_Link)

with_HA<-df[which(!is.na(df$HA_Profile_Link)),]
sum(duplicated(with_HA$HA_Profile_Link)) #same Id, diff category
df$HA_P<-!is.na(df$HA_Profile_Link)

with_Yelp<-df[which(!is.na(df$Yelp_Profile_Link)),]
sum(duplicated(with_Yelp$Yelp_Profile_Link)) #same Id, diff category
df$Yelp_P<-!is.na(df$Yelp_Profile_Link)

# View(df[df$Yelp_Profile_Link %in% with_Yelp[duplicated(with_Yelp$Yelp_Profile_Link),'Yelp_Profile_Link'][[1]],] %>%
#        select(Id,business,Category,lead_status,lead_number_of_attempts,Yelp_Profile_Link) %>%
#        arrange(business))

##3.1 Conversion across platform
#Profile feature within context: Larger diff power
cn<-sum(df$Conversion)/nrow(df) #0.073
df_P<-df %>% group_by(Angi_P,HA_P,Yelp_P) %>%
  summarise(n = n(), c=sum(Conversion)) 

df_P3<-df_P %>% mutate(pc=round(c/n*100,2), all=round(cn*100,2)) %>% 
  arrange(desc(pc))

df_P3$diff<-df_P3$pc-df_P3$all
df_P3$num<-df_P3$Angi_P+df_P3$HA_P+df_P3$Yelp_P
cor(df_P3$num,df_P3$pc) #-0.14
#View(df_P3)

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

sum(with_Angi$Conversion)/nrow(with_Angi) #0.05
sum(with_HA$Conversion)/nrow(with_HA) #0.07
sum(with_Yelp$Conversion)/nrow(with_Yelp) #0.08

##3.2Conversion across platform:Combine Google
df_G<-df[-which(is.na(df$Google_Search_Side_Profile__Y_N_)),] #drop 6 with NA
df_G$Google_P<-df_G$Google_Search_Side_Profile__Y_N_

#Google Profile less effective
cn1<-sum(df_G$Conversion)/nrow(df_G) #0.0731
df_G %>% group_by(Google_P) %>%
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2), all=round(cn1*100,2)) %>% 
  arrange(desc(pc))

#Google Profile does not change previous patterns
df_G %>% group_by(Angi_P,HA_P,Yelp_P,Google_P) %>%
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2), all=round(cn1*100,2)) %>% 
  arrange(desc(pc))

##3.3 Occupation % and conversin rate across platforms: 
#More HVAC Specialist in Angi, More General Contractor in HA
#HA and Yelp have similar pattern, Inspector/Pest/Furniture/CLeaner/Floor/Landscaper more conversion
# Angi different patter, Inspector/Pest/Carpenter more conversion, not others
table(df$occupation)
library(tidyverse)
Angi<-with_Angi %>% group_by(occupation,conversion_rate) %>% summarise(n = n(), c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(with_Angi),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))
Angi<-cbind(Angi,rank_Angi=1:nrow(Angi))
HA<-with_HA %>% group_by(occupation) %>% summarise(n = n(), c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(with_HA),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))
HA<-cbind(HA,rank_HA=1:nrow(HA))
Yelp<-with_Yelp %>% group_by(occupation) %>% summarise(n = n(), c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(with_Yelp),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))
Yelp<-cbind(Yelp,rank_Yelp=1:nrow(Yelp))

list<-list(Angi,HA,Yelp)
df_occu_P<-list %>% reduce(full_join, by='occupation') %>% arrange(desc(conversion_rate))
df_occu_P<-cbind(df_occu_P,rank_all=1:nrow(df_occu_P)) 
df_occu_P$conversion_rate<-df_occu_P$conversion_rate*100
names(df_occu_P)=c('occupation','conversion_rate',
                 'Angi.n','Angi.c','Angi.p','Angi.pc','Angi.rank',
                 'HA.n','HA.c','HA.p','HA.pc','HA.rank',
                 'Yelp.n','Yelp.c','Yelp.p','Yelp.pc','Yelp.rank',
                 'rank_all')
df_occu_P<-merge(df_occu_P,df_oc[c('occupation','pc')],by='occupation')
df_occu_P$rank_sum<-df_occu_P$Angi.rank+df_occu_P$HA.rank+df_occu_P$Yelp.rank
df_occu_P<-df_occu_P %>% arrange(rank_sum)
#View(df_occu_P)

##IV. Numbers of Review for each platforms
with_Angi$Angi_s___of_Reviews<-ifelse(is.na(with_Angi$Angi_s___of_Reviews),0,with_Angi$Angi_s___of_Reviews)
with_HA$HA___of_Reviews<-ifelse(is.na(with_HA$HA___of_Reviews),0,with_HA$HA___of_Reviews)
with_Yelp$Yelp___of_Reviews<-ifelse(is.na(with_Yelp$Yelp___of_Reviews),0,with_Yelp$Yelp___of_Reviews)

##4.1Angi:5.02% avg conversion rate
summary(with_Angi$Angi_s___of_Reviews)
ac_Angi<-sum(with_Angi$Conversion)/nrow(with_Angi)*100 

Angi_R<-with_Angi %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(Angi_s___of_Reviews), Reviews=list(summary(Angi_s___of_Reviews)))
#View(Angi_R)

#Convert number of reviews into group: more reviews=more convert
Angi_regroup_fct<-function(x){
  if(x<=2){
    out<-'Low'
  }else if(x<=21){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
with_Angi$rew_group<-sapply(with_Angi$Angi_s___of_Reviews,Angi_regroup_fct)
Angi_rew_group<-with_Angi %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Angi) %>% 
  arrange(desc(pc))
Angi_rew_group$diff<-Angi_rew_group$pc-Angi_rew_group$all
#View(Angi_rew_group)

#number of reviews has no sig in Angi, lower power after grouping
summary(lm(with_Angi$Conversion~with_Angi$rew_group))
summary(lm(with_Angi$Conversion~with_Angi$Angi_s___of_Reviews))

#Check outliers
#View(with_Angi[which(with_Angi$Angi_s___of_Reviews>500),]) #Top 7 are HVAC Specialist, 3 Plumber

#Hard to differentiate without segmentation
library(ggplot2)
p<-with_Angi %>% filter(Angi_s___of_Reviews<=150) %>% 
  ggplot( aes(x=Angi_s___of_Reviews, fill=Conversion)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")
#hist(with_Angi[with_Angi$Conversion,]$Angi_s___of_Reviews)

#describe number of reviews by occu and conversion rate: sample size too small
Angi_occu_R<-with_Angi %>% group_by(occupation,Conversion) %>% 
  summarise(n = n(), median_R=median(Angi_s___of_Reviews),Reviews=list(summary(Angi_s___of_Reviews)))

Angi_occu_R<-merge(Angi_occu_R,df_occu_P[c('occupation','Angi.pc','pc')],by='occupation')  %>% 
  arrange(desc(pc))
#View(Angi_occu_R)

#regroup occupation by Angi's conversion rate:unbalance group?
#Add 'Roofer and Sider','	Locksmith','Cleaner' as high conversion group?--change pattern
with_Angi$occu_group<-ifelse(with_Angi$occupation %in% c('Inspector and Restorer','Pest Control Specialist','Carpenter',
                                                         'Roofer and Sider','	Locksmith','Cleaner'),
       'High','Low')
Angi_reoccu_R<-with_Angi %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_R=median(Angi_s___of_Reviews),Reviews=list(summary(Angi_s___of_Reviews)))
#View(Angi_reoccu_R)
summary(lm(with_Angi$Conversion~with_Angi$occu_group)) #sig

#regroup occupation by median number of review:overall median=6
#larger diff power for low group
median(with_Angi$Angi_s___of_Reviews)
Angi_reoccu_R2<-with_Angi %>% group_by(occupation) %>% 
  summarise(n=n(),median_R=median(Angi_s___of_Reviews), Reviews=list(summary(Angi_s___of_Reviews))) %>%
  arrange(desc(median_R))
with_Angi$occu_group<-ifelse(with_Angi$occupation %in% Angi_reoccu_R2[1:6,'occupation'][[1]],
                             'High','Low')
Angi_reoccu_R2_f<-with_Angi %>% group_by(occu_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Angi)
#View(Angi_reoccu_R2_f)

summary(lm(with_Angi$Conversion~with_Angi$Angi_s___of_Reviews)) #not sig
summary(lm(with_Angi$Conversion~with_Angi$occu_group)) #not sig, smaller p value, larger R^2

Angi_reoccu_R2_h<-with_Angi %>% filter(occu_group=='High') %>% select(Angi_s___of_Reviews,Conversion)
summary(lm(Angi_reoccu_R2_h$Conversion~Angi_reoccu_R2_h$Angi_s___of_Reviews)) #not sig, smaller p value
Angi_reoccu_R2_l<-with_Angi %>% filter(occu_group=='Low') %>% select(Angi_s___of_Reviews,Conversion)
summary(lm(Angi_reoccu_R2_l$Conversion~Angi_reoccu_R2_l$Angi_s___of_Reviews)) #not sig, larger p value

##4.2 HA: 6.82% avg conversion rate
summary(with_HA$HA___of_Reviews)
ac_HA<-sum(with_HA$Conversion)/nrow(with_HA)*100 

HA_R<-with_HA %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(HA___of_Reviews),n_Reviews=list(summary(HA___of_Reviews)))
#View(HA_R)

#Convert number of reviews into group: more reviews=more convert
HA_regroup_fct<-function(x){
  if(x<=0){
    out<-'Low'
  }else if(x<=10){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
with_HA$rew_group<-sapply(with_HA$HA___of_Reviews,HA_regroup_fct)
HA_rew_group<-with_HA %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_HA) %>% 
  arrange(desc(pc))
HA_rew_group$diff<-HA_rew_group$pc-HA_rew_group$all
#View(HA_rew_group)

#Higher power after grouping
summary(lm(with_HA$Conversion~with_HA$rew_group))
summary(lm(with_HA$Conversion~with_HA$HA___of_Reviews))


#Check outliers
#View(with_HA[which(with_HA$HA___of_Reviews>500),]) #3/8 are HVAC Specialist

#Hard to differentiate without segmentation
library(ggplot2)
p<-with_HA %>% filter(HA___of_Reviews<=10) %>% 
  ggplot( aes(x=HA___of_Reviews, fill=Conversion)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")
#hist(with_HA[with_HA$Conversion,]$HA___of_Reviews)

#describe number of reviews by occu and conversion rate: sample size too small, more reviews higher rate
HA_occu_R<-with_HA %>% group_by(occupation,Conversion) %>% 
  summarise(n = n(), median_R=median(HA___of_Reviews),Reviews=list(summary(HA___of_Reviews)))

HA_occu_R<-merge(HA_occu_R,df_occu_P[c('occupation','HA.pc','pc')],by='occupation')  %>% 
  arrange(desc(pc))
#View(HA_occu_R)

#regroup occupation by HA's conversion rate(remove Interior design): same pattern across group
with_HA$occu_group<-ifelse(with_HA$occupation %in% c('Inspector and Restorer','Pest Control Specialist',
                                                     'Furniture Service Specialist','Cleaner','Floor, Carpet, and Tile Specialist',
                                                     'Landscaper'), 
                             'High','Low')
HA_reoccu_R<-with_HA %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_R=median(HA___of_Reviews),Reviews=list(summary(HA___of_Reviews)))
#View(HA_reoccu_R)
summary(lm(with_HA$Conversion~with_HA$occu_group)) #sig occu&conversion group

#regroup occupation by median number of review:overall median=2
#larger diff power for high group
median(with_HA$HA___of_Reviews)
HA_reoccu_R2<-with_HA %>% group_by(occupation) %>% 
  summarise(n=n(),median_R=median(HA___of_Reviews), Reviews=list(summary(HA___of_Reviews))) %>%
  arrange(desc(median_R))
with_HA$occu_group<-ifelse(with_HA$occupation %in% HA_reoccu_R2[1:8,'occupation'][[1]],
                             'High','Low')
HA_reoccu_R2_f<-with_HA %>% group_by(occu_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_HA)
#View(HA_reoccu_R2_f)

summary(lm(with_HA$Conversion~with_HA$HA___of_Reviews)) #sig

HA_reoccu_R2_h<-with_HA %>% filter(occu_group=='High') %>% select(HA___of_Reviews,Conversion)
summary(lm(HA_reoccu_R2_h$Conversion~HA_reoccu_R2_h$HA___of_Reviews)) #more sig
HA_reoccu_R2_l<-with_HA %>% filter(occu_group=='Low') %>% select(HA___of_Reviews,Conversion)
summary(lm(HA_reoccu_R2_l$Conversion~HA_reoccu_R2_l$HA___of_Reviews)) #not sig

##4.3 Yelp: 8.14% avg conversion rate
summary(with_Yelp$Yelp___of_Reviews)
ac_Yelp<-sum(with_Yelp$Conversion)/nrow(with_Yelp)*100 

Yelp_R<-with_Yelp %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(Yelp___of_Reviews),n_Reviews=list(summary(Yelp___of_Reviews)))
#View(Yelp_R)

#Convert number of reviews into group: more reviews=more convert
Yelp_regroup_fct<-function(x){
  if(x<=0){
    out<-'Low'
  }else if(x<=10){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
with_Yelp$rew_group<-sapply(with_Yelp$Yelp___of_Reviews,Yelp_regroup_fct)
Yelp_rew_group<-with_Yelp %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Yelp) %>% 
  arrange(desc(pc))
Yelp_rew_group$diff<-Yelp_rew_group$pc-Yelp_rew_group$all
#View(Yelp_rew_group)

#lower power after grouping, not sig
summary(lm(with_Yelp$Conversion~with_Yelp$rew_group))
summary(lm(with_Yelp$Conversion~with_Yelp$Yelp___of_Reviews))

#Check outliers
#View(with_Yelp[which(with_Yelp$Yelp___of_Reviews>350),]) #2 Plumber, 2 HVAC Specialist

#describe number of reviews by occu and conversion rate: sample size too small
Yelp_occu_R<-with_Yelp %>% group_by(occupation,Conversion) %>% 
  summarise(n = n(), median_R=median(Yelp___of_Reviews),Reviews=list(summary(Yelp___of_Reviews)))

Yelp_occu_R<-merge(Yelp_occu_R,df_occu_P[c('occupation','Yelp.pc','pc')],by='occupation')  %>% 
  arrange(desc(pc))
#View(Yelp_occu_R)

#regroup occupation by Yelp's conversion rate: different pattern across group
with_Yelp$occu_group<-ifelse(with_Yelp$occupation %in% c('Inspector and Restorer','Pest Control Specialist',
                                                     'Furniture Service Specialist','Cleaner','Floor, Carpet, and Tile Specialist',
                                                     'Landscaper'), 
                           'High','Low')
Yelp_reoccu_R<-with_Yelp %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_R=median(Yelp___of_Reviews),Reviews=list(summary(Yelp___of_Reviews)))
#View(Yelp_reoccu_R)
summary(lm(with_Yelp$Conversion~with_Yelp$occu_group)) #sig occu_group setting

#regroup occupation by median number of review:overall median=1
#larger diff power for high group
median(with_Yelp$Yelp___of_Reviews)
Yelp_reoccu_R2<-with_Yelp %>% group_by(occupation) %>% 
  summarise(n=n(),median_R=median(Yelp___of_Reviews), Reviews=list(summary(Yelp___of_Reviews))) %>%
  arrange(desc(median_R))
with_Yelp$occu_group<-ifelse(with_Yelp$occupation %in% Yelp_reoccu_R2[1:10,'occupation'][[1]],
                           'High','Low')
Yelp_reoccu_R2_f<-with_Yelp %>% group_by(occu_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Yelp)
#View(Yelp_reoccu_R2_f)

summary(lm(with_Yelp$Conversion~with_Yelp$Yelp___of_Reviews)) #not sig overall
summary(lm(with_Yelp$Conversion~with_Yelp$occu_group)) #not sig, larger p-value, worse

Yelp_reoccu_R2_h<-with_Yelp %>% filter(occu_group=='High') %>% select(Yelp___of_Reviews,Conversion)
summary(lm(Yelp_reoccu_R2_h$Conversion~Yelp_reoccu_R2_h$Yelp___of_Reviews)) #not sig, larger p value
Yelp_reoccu_R2_l<-with_Yelp %>% filter(occu_group=='Low') %>% select(Yelp___of_Reviews,Conversion)
summary(lm(Yelp_reoccu_R2_l$Conversion~Yelp_reoccu_R2_l$Yelp___of_Reviews)) #slightly sig, smaller p value


#Check Not Recommended comments:useless
median(with_Yelp$Yelp___of_Reviews__that_are_not_recommended_,na.rm = T)
Yelp_notr_R<-with_Yelp %>% filter(!is.na(Yelp___of_Reviews__that_are_not_recommended_)) %>% 
  group_by(Conversion) %>% summarise(n=n(),
                                     median_R=median(Yelp___of_Reviews__that_are_not_recommended_),
                                     Reviews=list(summary(Yelp___of_Reviews__that_are_not_recommended_)))
Yelp_notr_R['Reviews'][[1]]
summary(lm(with_Yelp$Conversion~with_Yelp$Yelp___of_Reviews__that_are_not_recommended_))

#Note: occupation group is useful
#Note: number of reviews is useful for HA, not for Angi, may depend on occu group for Yelp

##4.4 Google: 7.61% avg conversion rate
#df_G<-df[-which(is.na(df$Google_Search_Side_Profile__Y_N_)),] #drop 6 with NA
#df_G$Google_P<-df_G$Google_Search_Side_Profile__Y_N_
with_G<-df_G[df_G$Google_P,]
with_G$Google___of_Reviews<-ifelse(is.na(with_G$Google___of_Reviews),0,with_G$Google___of_Reviews)

summary(with_G$Google___of_Reviews)
ac_G<-sum(with_G$Conversion)/nrow(with_G)*100 

G_R<-with_G %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(Google___of_Reviews),n_Reviews=list(summary(Google___of_Reviews)))
#View(G_R)

#Convert number of reviews into group: mid reviews=most convert,high reviews=least convert
G_regroup_fct<-function(x){
  if(x<=0){
    out<-'Low'
  }else if(x<=19){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
with_G$rew_group<-sapply(with_G$Google___of_Reviews,G_regroup_fct)
G_rew_group<-with_G %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_G) %>% 
  arrange(desc(pc))
G_rew_group$diff<-G_rew_group$pc-G_rew_group$all
#View(G_rew_group) #non-linear relationship

#higher power after grouping, not sig
summary(lm(with_G$Conversion~with_G$rew_group))
summary(lm(with_G$Conversion~with_G$Google___of_Reviews))

#Check outliers
#View(with_G[which(with_G$Google___of_Reviews>1000),]) #More reviews than other platforms, no occu pref

#regroup occupation by Yelp&HA's conversion rate: different pattern across group
with_G$occu_group<-ifelse(with_G$occupation %in% c('Inspector and Restorer','Pest Control Specialist',
                                                         'Furniture Service Specialist','Cleaner','Floor, Carpet, and Tile Specialist',
                                                         'Landscaper'), 
                             'High','Low')
G_reoccu_R<-with_G %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_R=median(Google___of_Reviews),Reviews=list(summary(Google___of_Reviews)))
#View(G_reoccu_R) #no diff


#regroup occupation by median number of review:overall median=1
#larger diff power for high group
median(with_G$Google___of_Reviews)
G_reoccu_R2<-with_G %>% group_by(occupation) %>% 
  summarise(n=n(),median_R=median(Google___of_Reviews), Reviews=list(summary(Google___of_Reviews))) %>%
  arrange(desc(median_R))
with_G$occu_group<-ifelse(with_G$occupation %in% G_reoccu_R2[1:10,'occupation'][[1]],
                             'High','Low')
G_reoccu_R2_f<-with_G %>% group_by(occu_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_G)
#View(G_reoccu_R2_f)

summary(lm(with_G$Conversion~with_G$Google___of_Reviews)) #not sig overall
summary(lm(with_G$Conversion~with_G$occu_group)) #not sig, smaller p-value, but worse than just group reviews

G_reoccu_R2_h<-with_G %>% filter(occu_group=='High') %>% select(Google___of_Reviews,Conversion)
summary(lm(G_reoccu_R2_h$Conversion~G_reoccu_R2_h$Google___of_Reviews)) #not sig, smaller p value
G_reoccu_R2_l<-with_G %>% filter(occu_group=='Low') %>% select(Google___of_Reviews,Conversion)
summary(lm(G_reoccu_R2_l$Conversion~G_reoccu_R2_l$Google___of_Reviews)) #not sig, larger p value


##V. Avg Rating of Reviews for each platforms
with_Angi$Angi_s_Avg_Rating<-ifelse(is.na(with_Angi$Angi_s_Avg_Rating),0,with_Angi$Angi_s_Avg_Rating)
with_HA$HA_Avg_Rating<-ifelse(is.na(with_HA$HA_Avg_Rating),0,with_HA$HA_Avg_Rating)
with_Yelp$Yelp_Avg_Rating<-ifelse(is.na(with_Yelp$Yelp_Avg_Rating),0,with_Yelp$Yelp_Avg_Rating)

Angi_RA<-with_Angi %>% filter(Angi_s_Avg_Rating>0) %>% select(occupation,Conversion,Angi_s___of_Reviews,Angi_s_Avg_Rating)
HA_RA<-with_HA %>% filter(HA_Avg_Rating>0) %>% select(occupation,Conversion,HA___of_Reviews,HA_Avg_Rating)
Yelp_RA<-with_Yelp %>% filter(Yelp_Avg_Rating>0) %>% select(occupation,Conversion,Yelp___of_Reviews,Yelp_Avg_Rating)
G_RA<-with_G %>% filter(Google_Avg_Rating>0 & Google_Avg_Rating<6) %>% select(occupation,Conversion,Google___of_Reviews,Google_Avg_Rating)

##5.1Angi:4.49% avg conversion rate
summary(Angi_RA$Angi_s_Avg_Rating)
ac_Angi1<-sum(Angi_RA$Conversion)/nrow(Angi_RA)*100 

Angi_A<-Angi_RA %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(Angi_s_Avg_Rating), Reviews=list(summary(Angi_s_Avg_Rating)))
#View(Angi_A)

#Convert avg rating into group: low rating=less convert, sample size two small?
Angi_regroup_fct_A<-function(x){
  if(x<=4.6){
    out<-'Low'
  }else if(x<=4.9){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
Angi_RA$rew_group<-sapply(Angi_RA$Angi_s_Avg_Rating,Angi_regroup_fct_A)
Angi_rew_group<-Angi_RA %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Angi1) %>% 
  arrange(desc(pc))
Angi_rew_group$diff<-Angi_rew_group$pc-Angi_rew_group$all
#View(Angi_rew_group) 

#lower power after grouping, not sig
summary(lm(Angi_RA$Conversion~Angi_RA$rew_group))
summary(lm(Angi_RA$Conversion~Angi_RA$Angi_s_Avg_Rating))

#regroup occupation by Angi's conversion rate: low convert group=more demanding(both higher)
Angi_RA$occu_group<-ifelse(Angi_RA$occupation %in% c('Inspector and Restorer','Pest Control Specialist','Carpenter',
                                                     'Roofer and Sider','	Locksmith','Cleaner'), 
                          'High','Low')
Angi_reoccu_RA<-Angi_RA %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_RA=median(Angi_s_Avg_Rating),median_R=median(Angi_s___of_Reviews),Reviews=list(summary(Angi_s_Avg_Rating)))
#View(Angi_reoccu_RA) #note its diff from Angi_reoccu_R

#regroup occupation by median avg rating:overall median=4.9
median(Angi_RA$Angi_s_Avg_Rating)
Angi_reoccu_RA2<-Angi_RA %>% group_by(occupation) %>% 
  summarise(n=n(),median_RA=median(Angi_s_Avg_Rating), median_R=median(Angi_s___of_Reviews),Reviews=list(summary(Angi_s_Avg_Rating))) %>%
  arrange(desc(median_RA))
Angi_RA$occu_group<-ifelse(Angi_RA$occupation %in% Angi_reoccu_RA2[1:10,'occupation'][[1]],
                          'High','Low')
Angi_reoccu_RA2_f<-Angi_RA %>% group_by(occu_group) %>% 
  summarise(n = n(), c=sum(Conversion),median_RA=median(Angi_s_Avg_Rating),median_R=median(Angi_s___of_Reviews),details=list(summary(Angi_s_Avg_Rating))) %>% 
  mutate(pc=round(c/n*100,2),all=ac_Angi1)
#View(Angi_reoccu_RA2_f) #no diff



##5.2HA:7.80% avg conversion rate
summary(HA_RA$HA_Avg_Rating)
ac_HA1<-sum(HA_RA$Conversion)/nrow(HA_RA)*100 

HA_A<-HA_RA %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(HA_Avg_Rating), Reviews=list(summary(HA_Avg_Rating)))
#View(HA_A) #no diff

#Convert avg ratings into group:
HA_regroup_fct_A<-function(x){
  if(x<=4.7){
    out<-'Low'
  }else if(x<=4.9){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
HA_RA$rew_group<-sapply(HA_RA$HA_Avg_Rating,HA_regroup_fct_A)
HA_rew_group<-HA_RA %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Angi1) %>% 
  arrange(desc(pc))
HA_rew_group$diff<-HA_rew_group$pc-HA_rew_group$all
#View(HA_rew_group) #non-linear relationship, sample size ok?

#Higher power after grouping, sig
summary(lm(HA_RA$Conversion~HA_RA$rew_group))
summary(lm(HA_RA$Conversion~HA_RA$HA_Avg_Rating))

#regroup occupation by HA's conversion rate: 
HA_RA$occu_group<-ifelse(HA_RA$occupation %in% c('Inspector and Restorer','Pest Control Specialist',
                                                     'Furniture Service Specialist','Cleaner','Floor, Carpet, and Tile Specialist',
                                                     'Landscaper'), 
                           'High','Low')
HA_reoccu_RA<-HA_RA %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_RA=median(HA_Avg_Rating),median_R=median(HA___of_Reviews),details=list(summary(HA_Avg_Rating)))
#View(HA_reoccu_RA) #no diff

#regroup occupation by median avg rating:overall median=4.9
median(HA_RA$HA_Avg_Rating)
HA_reoccu_RA2<-HA_RA %>% group_by(occupation) %>% 
  summarise(n=n(),median_RA=median(HA_Avg_Rating), median_R=median(HA___of_Reviews),details=list(summary(HA_Avg_Rating))) %>%
  arrange(desc(median_RA))
# HA_RA$occu_group<-ifelse(HA_RA$occupation %in% HA_reoccu_RA2[1:10,'occupation'][[1]],
#                            'High','Low')
# HA_reoccu_RA2<-HA_RA %>% group_by(occu_group) %>% 
#   summarise(n = n(), c=sum(Conversion),median_R=median(HA___of_Reviews),details=list(summary(HA___of_Reviews))) %>% 
#   mutate(pc=round(c/n*100,2),all=ac_Angi1)

#View(HA_reoccu_RA2) #avg rating for different occupation is too close to group



##5.3 Yelp:8.74% avg conversion rate
summary(Yelp_RA$Yelp_Avg_Rating)
ac_Yelp1<-sum(Yelp_RA$Conversion)/nrow(Yelp_RA)*100 

Yelp_A<-Yelp_RA %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(Yelp_Avg_Rating), Reviews=list(summary(Yelp_Avg_Rating)))
#View(Yelp_A) #no diff

#Convert avg ratings into group:
Yelp_regroup_fct_A<-function(x){
  if(x<=3.5){
    out<-'Low'
  }else if(x<5){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
Yelp_RA$rew_group<-sapply(Yelp_RA$Yelp_Avg_Rating,Yelp_regroup_fct_A)
Yelp_rew_group<-Yelp_RA %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_Yelp1) %>% 
  arrange(desc(pc))
Yelp_rew_group$diff<-Yelp_rew_group$pc-Yelp_rew_group$all
#View(Yelp_rew_group) #linear relationship, sample size ok?

#? power after grouping, sig
summary(lm(Yelp_RA$Conversion~Yelp_RA$rew_group))
summary(lm(Yelp_RA$Conversion~Yelp_RA$Yelp_Avg_Rating))

#regroup occupation by Yelp's conversion rate: more demanding for low convert group
Yelp_RA$occu_group<-ifelse(Yelp_RA$occupation %in% c('Inspector and Restorer','Pest Control Specialist',
                                                 'Furniture Service Specialist','Cleaner','Floor, Carpet, and Tile Specialist',
                                                 'Landscaper'), 
                         'High','Low')
Yelp_reoccu_RA<-Yelp_RA %>% group_by(occu_group,Conversion) %>% 
  summarise(n = n(), median_RA=median(Yelp_Avg_Rating),median_R=median(Yelp___of_Reviews),details=list(summary(Yelp_Avg_Rating)))

#View(Yelp_reoccu_RA) 

#regroup occupation by median avg rating:overall median=4.5
median(Yelp_RA$Yelp_Avg_Rating)
Yelp_reoccu_RA2<-Yelp_RA %>% group_by(occupation) %>% 
  summarise(n=n(),median_RA=median(Yelp_Avg_Rating), median_R=median(Yelp___of_Reviews),details=list(summary(Yelp_Avg_Rating))) %>%
  arrange(desc(median_RA))
# Yelp_RA$occu_group<-ifelse(Yelp_RA$occupation %in% Yelp_reoccu_RA2[1:10,'occupation'][[1]],
#                          'High','Low')
# Yelp_reoccu_RA2<-Yelp_RA %>% group_by(occu_group) %>%
#   summarise(n = n(), c=sum(Conversion),median_R=median(Yelp___of_Reviews),details=list(summary(Yelp___of_Reviews))) %>%
#   mutate(pc=round(c/n*100,2),all=ac_Yelp1)

#View(Yelp_reoccu_RA2) #avg rating for different occupation is too close to group


##5.4 Google:7.77% avg conversion rate
summary(G_RA$Google_Avg_Rating)
ac_G1<-sum(G_RA$Conversion)/nrow(G_RA)*100 

G_A<-G_RA %>% group_by(Conversion) %>% 
  summarise(n=n(),median_R=median(Google_Avg_Rating), Reviews=list(summary(Google_Avg_Rating)))
#View(G_A) #high rating=more convert

#Convert avg ratings into group:
G_regroup_fct_A<-function(x){
  if(x<=4.4){
    out<-'Low'
  }else if(x<5){
    out<-'Mid'
  }else{
    out<-'High'
  }
  out
}
G_RA$rew_group<-sapply(G_RA$Google_Avg_Rating,G_regroup_fct_A)
G_rew_group<-G_RA %>% group_by(rew_group) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=ac_G1) %>% 
  arrange(desc(pc))
G_rew_group$diff<-G_rew_group$pc-G_rew_group$all
#View(G_rew_group) #linear relationship, sample size ok?

#lower power after grouping, sig
summary(lm(G_RA$Conversion~G_RA$rew_group))
summary(lm(G_RA$Conversion~G_RA$Google_Avg_Rating))

#regroup occupation by median avg rating:overall median=4.5
median(G_RA$Google_Avg_Rating)
G_reoccu_RA2<-G_RA %>% group_by(occupation) %>% 
  summarise(n=n(),median_RA=median(Google_Avg_Rating), median_R=median(Google___of_Reviews),details=list(summary(Google_Avg_Rating))) %>%
  arrange(desc(median_RA))

G_RA$occu_group<-ifelse(G_RA$occupation %in% G_reoccu_RA2[1:11,'occupation'][[1]],
                           'High','Low')
G_reoccu_RA2_f<-G_RA %>% group_by(occu_group) %>%
  summarise(n = n(), c=sum(Conversion), median_RA=median(Google_Avg_Rating), median_R=median(Google___of_Reviews),details=list(summary(Google_Avg_Rating))) %>%
  mutate(pc=round(c/n*100,2),all=ac_G1)

summary(lm(G_RA$Conversion~G_RA$Google_Avg_Rating))
G_reoccu_RA2_h<-with_G %>% filter(occu_group=='High') %>% select(Google_Avg_Rating,Conversion)
summary(lm(G_reoccu_RA2_h$Conversion~G_reoccu_RA2_h$Google_Avg_Rating)) #sig, larger p value
G_reoccu_RA2_l<-with_G %>% filter(occu_group=='Low') %>% select(Google_Avg_Rating,Conversion)
summary(lm(G_reoccu_RA2_l$Conversion~G_reoccu_RA2_l$Google_Avg_Rating)) #not sig, larger p value





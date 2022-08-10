df_clean <- read_csv("df_clean.csv",show_col_types = FALSE)

df_clean %>% select(Id,Conversion,occupation,Category,business,State,City,PostalCode)

df_state<-df_clean %>% group_by(State) %>% 
  summarise(n = n(), c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(df_clean),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))
View(df_state)
df_state$state<-df_state$State

df_city<-df_clean %>% group_by(City) %>% 
  summarise(n = n(), c=sum(Conversion)) %>%
  mutate(p=round(n/nrow(df),2),pc=round(c/n*100,2)) %>% arrange(desc(pc))
View(df_city)

library(usmap)
library(ggplot2)

plot_usmap(data = df_state, values = "pc", regions = "states", exclude = c("AK","HI"), color = "red",labels=T) + 
  scale_fill_continuous(low = "white", high = "red", name = "Conversion rate", label = scales::comma) + 
  labs(title = "Conversion rate across state") +
  theme(legend.position = "right")


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
df_clean$State_G<-apply(df_clean,1,State_G)

df_clean %>% group_by(State_G) %>% 
  summarise(n = n(), c=sum(Conversion)) %>% mutate(pc=round(c/n*100,2),all=round(cn*100,2)) %>% 
  mutate(dff=pc-all) %>% 
  arrange(desc(pc))



df_RF_loc<-df_clean %>% select(Conversion,industry,Category,with_web,P_G,occupation,State_G,
                          Google_P,Google___of_Reviews,Google_Avg_Rating,Google_diff_week,
                          Angi_P,Angi_s___of_Reviews,Angi_s_Avg_Rating,Angi_diff_week,
                          HA_P,HA___of_Reviews,HA_Avg_Rating,HA_diff_week,
                          Yelp_P,Yelp___of_Reviews,Yelp_Avg_Rating,Yelp_diff_week,Yelp___of_Reviews__that_are_not_recommended_,
                          FB_P,FB___of_Reviews,FB_Avg_Rating,FB_diff_week,FB_Page_Likes,FB_Page_Follows,
                          IG_P,IG_Follows)

df_RF_loc$Conversion <- as.factor(df_RF_loc$Conversion)
table(df_RF_loc$Conversion)
#Replace all NA into 0
df_RF_loc[is.na(df_RF_loc)] <- 0



nRareSamples =  900 * 0.05
rf_loc = randomForest(Conversion~.,data=df_RF_loc,strata=df_RF_loc$Conversion,
                         sampsize=c(nRareSamples,nRareSamples),
                         ntree=400,mtry=2*sqrt(dim(df_RF_loc)[2]),
                         importance=TRUE)
print(rf_loc)
runtest(df_RF_loc)
as.data.frame(importance(rf_loc)) %>% arrange(MeanDecreaseAccuracy)
varImpPlot(rf_loc)

#explore pro_pk
names(df_clean)
as.character(df_clean[!is.na(df_clean['opp_lead_pro_user_pk']),'opp_lead_pro_user_pk'])




#Library ----
library(tidyverse)
library(readr)
library(dplyr)
#####
covid_df <- read_csv("D:/Desktop/covid19.csv")
dim(covid_df)
colnames(covid_df)
head(covid_df)
glimpse(covid_df)


covid_df_all_states<- covid_df %>% 
  filter(Province_State=="All States") %>% 
  select(-Province_State)

dim(covid_df)
dim(covid_df_all_states)

covid_df_all_states_daily<-covid_df_all_states %>% 
  select(Date, Country_Region, active, hospitalizedCurr,daily_tested,daily_positive)
view(covid_df_all_states_daily)


covid_df_all_states_daily_sum<-covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% 
  summarise(tested=sum(daily_tested),
            positive=sum(daily_positive),
            active= sum(active),
            hospitalized=sum(hospitalizedCurr)
  ) %>% 
  arrange(-tested)

view(covid_df_all_states_daily_sum)
covid_top_10<-head(covid_df_all_states_daily_sum,10)

countries<- covid_top_10 %>% 
  pull(Country_Region)

tested_cases<- covid_top_10$tested
positive_cases<-covid_top_10[["positive"]]
active_cases<-covid_top_10$active
hospitalized_cases<- covid_top_10$hospitalized

names(tested_cases)<-countries
names(positive_cases)<-countries
names(active_cases)<-countries
names(hospitalized_cases)<-countries

positive_test_top_3<-positive_cases/tested_cases 
positive_test_top_3

#uk/us/turkey

united_kingdom<-c(0.11, 1473672, 166909, 0, 0)
united_states<-c(0.10, 17282363, 1877179, 0, 0)
turkey<-c(0.08, 2031192, 163941, 2980960, 0)

covid_mat<-rbind(united_kingdom,united_states,turkey)
colnames(covid_mat)<-c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat

question<-"Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_test_top_3)

data_structure_list<-list(covid_df,covid_df_all_states,covid_df_all_states_daily,covid_top_10, covid_mat, countries)
covid_analysis_list<-list(question, answer ,data_structure_list)
covid_analysis_list[[2]]

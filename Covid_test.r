#COMMENTED COVID PROJECT.
#REMEMBER THAT THERE ARE ALWAYS MORE WAYS TO MAKE A PROJECT
#THIS IS ONE OF THEM!

#IF YOU USE ---- YOU CAN CREATE A SECTION IN RSTUDIO 

#Library ----                  
library(tidyverse)
library(readr)          #HERE WE IMPORT THE LIBRARIES THAT ARE NEEDED
library(dplyr)

#THE FIRST STEP IS TO DOWNLOAD THE .CSV FILE --> (https://dq-content.s3.amazonaws.com/505/covid19.csv)

covid_df <- read_csv("D:/Desktop/covid19.csv") #WE CAN USE THE read_csv() FUNCTION TO READ OUR FILE (REMEMBER THAT THE FUNCTION IS CONTAINED IN THE readr PACKAGE)
vector_cols<-colnames(covid_df)   

#Let's explore the data

dim(covid_df)        #dim() function to see the size of the dataframe
head(covid_df)       #head() function to see the first rows of the dataframe
glimpse(covid_df)    #We use the glimpse() function x to see the summary of the dataframe


#LET'S START FILTERING THE DATA

#1.) Filter the rows related to "All States" from the Province_State column and remove the Province_State column from covid_df dataframe.
covid_df_all_states<- covid_df %>% 
  filter(Province_State=="All States") %>%  #FILTER all data related to "All States"
  select(-Province_State)                   #REMOVE Province_State

dim(covid_df)
dim(covid_df_all_states)

#2:)Select the following column, related to the daily measures, from the covid_df_all_states: Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive.
covid_df_all_states_daily<-covid_df_all_states %>% 
  select(Date, Country_Region, active, hospitalizedCurr,daily_tested,daily_positive)
view(covid_df_all_states_daily)

#3.)Write code to summarize the covid_df_all_states_daily dataframe by computing the sum of the number of tested, positive, active and hospitalized cases grouped by the Country_Region column.

#Use the function group_by() to group rows by Country_Region column.
#Combine the function summarize() and the function sum() to compute the sum for each column.

#Assign the sum of daily_tested to the column name tested.
#Assign the sum of daily_positive to the column name positive.
#Assign the sum of active to the column name active.
#Assign the sum of hospitalizedCurr to the column name hospitalized.
#Arrange the tested column in descending order using the function arrange().

covid_df_all_states_daily_sum<-covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% 
  summarise(tested=sum(daily_tested),
            positive=sum(daily_positive),
            active= sum(active),
            hospitalized=sum(hospitalizedCurr)) %>% 
  arrange(-tested)

view(covid_df_all_states_daily_sum)

#View the top 10 states from covid_df_all_states_daily_sum
covid_top_10<-head(covid_df_all_states_daily_sum,10)

#4.)Create the following vectors from the covid_top_10 dataframe.
#Create the countries vector that contains the Country_Region column values. We can use covid_top_10$Country_Region to extract this column from the covid_top_10 dataframe.
countries<- covid_top_10 %>% 
  pull(Country_Region)

#Create the tested_cases vector that contains the tested column values.
#Create the positive_cases vector that contains the positive column values.
#Create the active_cases vector that contains the active column values.
#Create the hospitalized_cases vector that contains the hospitalized column values.
tested_cases<- covid_top_10$tested
positive_cases<-covid_top_10[["positive"]]
active_cases<-covid_top_10$active
hospitalized_cases<- covid_top_10$hospitalized

#5.)Write code to name the previous vectors: tested_cases, positive_cases, active_cases, and hospitalized_cases with the country names' vector countries using the function names()
names(tested_cases)<-countries
names(positive_cases)<-countries
names(active_cases)<-countries
names(hospitalized_cases)<-countries

#6.)Identify the top three positive against tested cases.
positive_test_top_3<-positive_cases/tested_cases 
positive_test_top_3

#Create the following vectors from the table above.

#Create the united_kingdom vector using this vector: c(0.11, 1473672, 166909, 0, 0).
#Create the united_states vector using this vector: c(0.10, 17282363, 1877179, 0, 0).
#Create the turkey vector using this vector: c(0.08, 2031192, 163941, 2980960, 0).

united_kingdom<-c(0.11, 1473672, 166909, 0, 0)
united_states<-c(0.10, 17282363, 1877179, 0, 0)
turkey<-c(0.08, 2031192, 163941, 2980960, 0)

#Create a matrix combining the vectors: united_kingdom, united_states, and turkey.
covid_mat<-rbind(united_kingdom,united_states,turkey)

#Rename the columns of this matrix with the vector c("Ratio", "tested", "positive", "active", "hospitalized") using the function colnames().
colnames(covid_mat)<-c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat #Display matrix

#Create character variables
question<-"Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_test_top_3)

#7.)Create a list that contains the data structures mentioned above.

#Create a list that contains the dataframes.
#Create a list that contains the matrices.
#Create a list that contains the vectors.
#Create a named list that contains the three previous lists associated with the data structure names.
datasets <- list(
  original = covid_df,
  allstates = covid_df_all_states,
  daily = covid_df_all_states_daily,
  top_10 = covid_top_10)

matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)
data_structure_list <- list("dataframe" = datasets, "matrix" = matrices, "vector" = vectors)

#Create a list that contains the following lists: question, answer, and data_structure_list.
covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list[[2]] #Display the second element of this list.

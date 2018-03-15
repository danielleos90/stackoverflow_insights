library(readr)
library(dplyr)
#read in results
survey_results<-read_csv("C:/Users/Danielleos/Desktop/COLLEGE/Year 2/Group Project/survey_results_public.csv")

#create country and formal education subset 
survey_results_subset<-subset(survey_results, select=c(Country, FormalEducation))
#Filter countries (States) and group by education levels
educationLevel<-survey_results_subset %>% group_by(FormalEducation, Country)
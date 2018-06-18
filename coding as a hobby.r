library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(plyr)




survey_results_part<-read_csv("C:/Users/Danielleos/Desktop/COLLEGE/Year 2/Group Project/survey_results_part.csv")
ggplot(survey_results_part, aes(x=as.factor(ProgramHobby)))+geom_bar()
survey_results_part$ProgramHobby<-substr(x = survey_results_part$ProgramHobby, start = 1, stop = 3)
total$percent<- NA
total$percent<- total$n / sum(total$n)
total$percent2 <- sprintf("%.0f%%", 100 * total$percent)
ggplot(total, aes(x=as.factor(ProgramHobby),y=percent2))+geom_bar(stat="identity", fill="lightsalmon3")+theme_classic()+ labs(title =" Coding as a Hobby", x = "Coding as a Hobby", y = "Percentage") 



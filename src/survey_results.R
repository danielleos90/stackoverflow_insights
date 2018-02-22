install.packages("readr")
survey_results<-data.frame(read.csv("Z:/groupProject/survey_results_public.csv"))

survey_results$Country
survey_results$FormalEducation

subset_survey_results<-subset(survey_results, select = c("Country", "FormalEducation"))


summary(survey_results$FormalEducation)


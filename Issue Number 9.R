
survey_results_public <- read.csv("C:/Users/Vinesh/Dropbox/DIT/Higher Diploma/Year 2/Group Project/survey_results_public.csv")

View(survey_results_public)

library(dplyr)

survey_results_public_states <- subset(survey_results_public, Country=="United States", group_by(survey_results_public_states$FormalEducation))

View(survey_results_public_states)

survey_results_states <- select(survey_results_public_states, Country, FormalEducation)

head(survey_results_states)

by_formal_education <- group_by(survey_results_states, Country, FormalEducation)

sum_formal_education <- summarise(by_formal_education, count=n())

View(by_formal_education)



View(sum_formal_education)

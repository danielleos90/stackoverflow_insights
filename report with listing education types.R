setwd("C:/Users/andy/desktop/group project")
library (readr)
library(tidyr)

results <- read_csv("survey_results_public.csv")

View(results)

sub <- data.frame(results$Country, results$FormalEducation)
names(sub) <- c("Country", "Foraml Education")
View(sub)

head(results)

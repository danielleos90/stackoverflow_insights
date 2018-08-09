library(ggplot2)

library(dplyr)

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

parents_professional <- subset(surveyResultsPublic, Professional == "Professional developer")

parents_profdev <- subset(surveyResultsPublic, select = c("HighestEducationParents"))

parents_profdev <- na.omit(parents_profdev)

count_parents_all <- separate(data = parents_all, col = HighestEducationParents, into = c("Var1", "Var2"), sep = ";")

parents_education_prof_count <- as.data.frame(table(unlist(parents_profdev)))

parents_education_prof_count$Percentage <- parents_education_prof_count$Freq / sum(parents_education_prof_count$Freq) * 100

parents_education_prof_count$Percentage <- round(parents_education_prof_count$Percentage, digits = 2)

plot <- ggplot(parents_education_prof_count, aes(Var1, Percentage))
plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
  labs(title="Parents Education", 
       subtitle="Professional", 
       caption="source: stackoverflow") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
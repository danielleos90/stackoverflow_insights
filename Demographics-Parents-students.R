library(ggplot2)

library(dplyr)

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

parents_students <- subset(surveyResultsPublic, Professional == "Student")

parents_studev <- subset(surveyResultsPublic, select = c("HighestEducationParents"))

parents_studev <- na.omit(parents_studev)

parents_students_count <- as.data.frame(table(unlist(parents_studev)))

parents_students_count$Percentage <- parents_students_count$Freq / sum(parents_students_count$Freq) * 100

parents_students_count$Percentage <- round(parents_students_count$Percentage, digits = 2)

plot <- ggplot(parents_students_count, aes(Var1, Percentage))
plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
  labs(title="Parents Education", 
       subtitle="Professional", 
       caption="source: stackoverflow") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
install.packages("ggplot2")

library(ggplot2)

library(dplyr)

install.packages("dplyr")

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

ethnicity_student <- subset(surveyResultsPublic, Professional == "Student")

View(ethnicity_student)

ethnicity_students <- subset.data.frame(ethnicity_student, select = c("Race"))

ethnicity_students <- na.omit(ethnicity_students)

View(ethnicity_students)

count_ethnicity_students <- separate(data = ethnicity_students, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")

View(count_ethnicity_students)

ethnicity_student_occurences <- as.data.frame(table(unlist(count_ethnicity_students)))

View(ethnicity_student_occurences)

str(ethnicity_student_occurences)

#combines East Asian Ethnicity
ethnicity_student_occurences[1, ] <- ethnicity_student_occurences[1, ] + ethnicity_student_occurences[10, ]
ethnicity_student_occurences[1, 1] <- "East Asian"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(10),]

#combines Hispanic or Latino/Latina 
ethnicity_student_occurences [2, ] <- ethnicity_student_occurences[2, ] + ethnicity_student_occurences[10, ]
ethnicity_student_occurences[2, 1] <- "Hispanic or Latino/Latina"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(10),]

#combines I prefer not to say
ethnicity_student_occurences [4, ] <- ethnicity_student_occurences[4, ] + ethnicity_student_occurences[11, ]
ethnicity_student_occurences[4, 1] <- "I prefer not to say"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]

#combines Middle Eastern
ethnicity_student_occurences [5, ] <- ethnicity_student_occurences[5, ] + ethnicity_student_occurences[11, ]
ethnicity_student_occurences[5, 1] <- "Middle Eastern"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]

#combines Native American, Pacific Islander, or Indigenous Australian
ethnicity_student_occurences [6, ] <- ethnicity_student_occurences[6, ] + ethnicity_student_occurences[11, ]
ethnicity_student_occurences[6, 1] <- "Native American, Pacific Islander, or Indigenous Australian"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]

#combines South Asian
ethnicity_student_occurences [7, ] <- ethnicity_student_occurences[7, ] + ethnicity_student_occurences[11, ]
ethnicity_student_occurences[7, 1] <- "South Asian"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]

#combines White or of European descent
ethnicity_student_occurences [8, ] <- ethnicity_student_occurences[8, ] + ethnicity_student_occurences[11, ]
ethnicity_student_occurences[8, 1] <- "White or of European descent"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]

#combines I don't Know
ethnicity_student_occurences [3, ] <- ethnicity_student_occurences[3, ] + ethnicity_student_occurences[10, ]
ethnicity_student_occurences[3, 1] <- "I don't Know"
ethnicity_student_occurences <- ethnicity_student_occurences[-c(10),]

ethnicity_student_occurences$Var1 <- as.character(ethnicity_student_occurences$Var1)

ethnicity_student_occurences$Percentage <- ethnicity_student_occurences$Freq / sum(ethnicity_student_occurences$Freq) * 100

ethnicity_student_occurences$Percentage <- round(ethnicity_student_occurences$Percentage, digits = 2)

plot <- ggplot(ethnicity_student_occurences, aes(Var1, Percentage))
plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
  labs(title="Ethnicity", 
       subtitle="Professional Developers", 
       caption="source: stackoverflow") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
  
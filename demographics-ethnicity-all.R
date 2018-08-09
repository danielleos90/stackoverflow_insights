library(ggplot2)

library(dplyr)

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

ethnicity_all <- subset(surveyResultsPublic, select = c("Race"))

View(ethnicity_all)

ethnicity_all <- na.omit(ethnicity_all)

View(ethnicity_all)

count_ethnicity_all <- separate(data = ethnicity_all, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")

View(count_ethnicity_all)

ethnicity_occurences <- as.data.frame(table(unlist(count_ethnicity_all)))

View(ethnicity_occurences)

str(ethnicity_occurences)

ethnicity_occurences$Var1 <- as.character(ethnicity_occurences$Var1)

#combines East Asian Ethnicity
ethnicity_occurences[1, ] <- ethnicity_occurences[1, ] + ethnicity_occurences[10, ]
ethnicity_occurences[1, 1] <- "East Asian"
ethnicity_occurences <- ethnicity_occurences[-c(10),]

#combines Hispanic or Latino/Latina 
ethnicity_occurences [2, ] <- ethnicity_occurences[2, ] + ethnicity_occurences[10, ]
ethnicity_occurences[2, 1] <- "Hispanic or Latino/Latina"
ethnicity_occurences <- ethnicity_occurences[-c(10),]

#combines I prefer not to say
ethnicity_occurences [4, ] <- ethnicity_occurences[4, ] + ethnicity_occurences[11, ]
ethnicity_occurences[4, 1] <- "I prefer not to say"
ethnicity_occurences <- ethnicity_occurences[-c(11),]

#combines Middle Eastern
ethnicity_occurences [5, ] <- ethnicity_occurences[5, ] + ethnicity_occurences[11, ]
ethnicity_occurences[5, 1] <- "Middle Eastern"
ethnicity_occurences <- ethnicity_occurences[-c(11),]

#combines Native American, Pacific Islander, or Indigenous Australian
ethnicity_occurences [6, ] <- ethnicity_occurences[6, ] + ethnicity_occurences[11, ]
ethnicity_occurences[6, 1] <- "Native American, Pacific Islander, or Indigenous Australian"
ethnicity_occurences <- ethnicity_occurences[-c(11),]

#combines South Asian
ethnicity_occurences [7, ] <- ethnicity_occurences[7, ] + ethnicity_occurences[11, ]
ethnicity_occurences[7, 1] <- "South Asian"
ethnicity_occurences <- ethnicity_occurences[-c(11),]

#combines White or of European descent
ethnicity_occurences [8, ] <- ethnicity_occurences[8, ] + ethnicity_occurences[11, ]
ethnicity_occurences[8, 1] <- "White or of European descent"
ethnicity_occurences <- ethnicity_occurences[-c(11),]

#combines I don't Know
ethnicity_occurences [3, ] <- ethnicity_occurences[3, ] + ethnicity_occurences[10, ]
ethnicity_occurences[3, 1] <- "I don't Know"
ethnicity_occurences <- ethnicity_occurences[-c(10),]

ethnicity_occurences$Percentage <- ethnicity_occurences$Freq / sum(ethnicity_occurences$Freq) * 100

ethnicity_occurences$Percentage <- round(ethnicity_occurences$Percentage, digits = 2)

plot <- ggplot(ethnicity_occurences, aes(Var1, Percentage))
plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
  geom_text(aes(label = Percentage, y = Percentage), vjust = 0, hjust = 0.5)+
  labs(title="Ethnicity", 
       subtitle="All Respondents", 
       caption="source: stackoverflow") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
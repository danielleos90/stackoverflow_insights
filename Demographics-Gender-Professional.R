install.packages("ggplot2")

library(ggplot2)

library(dplyr)

install.packages("dplyr")

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)


surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

genders_professional <- subset(surveyResultsPublic, Professional == "Professional developer")

View(genders_professional)

genders_profdev <- subset.data.frame(genders_professional, select = c("Gender"))

genders_profdev <- na.omit(genders_profdev)

View(genders_profdev)

count_prof_genders <- separate(data = genders_profdev, col = Gender, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")

View(count_prof_genders)

prof_occurences <- as.data.frame(table(unlist(count_prof_genders)))

View(prof_occurences)

str(prof_occurences)

prof_occurences$Var1 <- as.factor(prof_occurences$Var1)

#combines Female gender
prof_occurences[1, ] <- prof_occurences[1, ] + prof_occurences[5, ]
prof_occurences[1, 1] <- "Female"
prof_occurences <- prof_occurences[-c(5),]

#combines Transgender 
prof_occurences [4, ] <- prof_occurences[4, ] + prof_occurences[8, ]
prof_occurences[4, 1] <- "Transgender"
prof_occurences <- prof_occurences[-c(8),]

#combines Other
prof_occurences [3, ] <- prof_occurences[3, ] + prof_occurences[7, ]
prof_occurences[3, 1] <- "Other"
prof_occurences <- prof_occurences[-c(7),]

#combines Gender Non-Conforming
prof_occurences [2, ] <- prof_occurences[2, ] + prof_occurences[5, ]
prof_occurences[2, 1] <- "Gender non-conforming"
prof_occurences <- prof_occurences[-c(5),]


prof_occurences$Percentage <- prof_occurences$Freq / sum(prof_occurences$Freq) * 100


ggplot(prof_occurences, aes(x= Var1, y= Percentage))+ 
  geom_point(size=3, color = "Orange")+ 
  geom_segment(aes(x=Var1, 
                   xend=Var1, 
                   y=0, 
                   yend=Percentage), color = "Orange")+ 
  labs(title="Gender", 
       subtitle="Professional Developers Only", 
       caption="source: stackoverflow")+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
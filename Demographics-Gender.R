install.packages("ggplot2")

library(ggplot2)

library(dplyr)

install.packages("dplyr")

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)


surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

genders <- subset.data.frame(surveyResultsPublic, select = c("Gender"))

genders <- na.omit(genders)

View(genders)

count_all_genders <- separate(data = genders, col = Gender, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")

View(count_all_genders)

occurences <- as.data.frame(table(unlist(count_all_genders)))

View(occurences)

str(occurences)

occurences$Var1 <- as.factor(occurences$Var1)

#combines Female gender
occurences[1, ] <- occurences[1, ] + occurences[5, ]
occurences[1, 1] <- "Female"
occurences <- occurences[-c(5),]

#combines Transgender 
occurences [4, ] <- occurences[4, ] + occurences[8, ]
occurences[4, 1] <- "Transgender"
occurences <- occurences[-c(8),]

#combines Other
occurences [3, ] <- occurences[3, ] + occurences[7, ]
occurences[3, 1] <- "Other"
occurences <- occurences[-c(7),]

#combines Gender Non-Conforming
occurences [2, ] <- occurences[2, ] + occurences[5, ]
occurences[2, 1] <- "Gender non-conforming"
occurences <- occurences[-c(5),]


occurences$Percentage <- occurences$Freq / sum(occurences$Freq) * 100


ggplot(occurences, aes(x= Var1, y= Percentage))+ 
  geom_point(size=3, color = "Orange")+ 
  geom_segment(aes(x=Var1, 
                   xend=Var1, 
                   y=0, 
                   yend=Percentage), color = "Orange")+ 
  labs(title="Gender", 
       subtitle="All Respondents", 
       caption="source: stackoverflow")+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
install.packages("ggplot2")

library(ggplot2)

install.packages("dplyr")

library(dplyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE)

expDevelopers <- subset.data.frame(surveyResultsPublic, select = c("YearsProgram", "Professional"))

expDevelopers <- na.omit(expDevelopers)

expDevelopers <- filter(expDevelopers, Professional == "Professional developer")

count_expDevelopers <- as.data.frame(table(unlist(expDevelopers$YearsProgram)))

View(count_expDevelopers)

count_expDevelopers$Percentage <- count_expDevelopers$Freq / sum(count_expDevelopers$Freq) * 100

colnames(count_expDevelopers)[colnames(count_expDevelopers)=="Var1"] <- "YearsProgramming"

profDeveloperYears <- as.data.frame(matrix(NA, ncol = 2, nrow = 21))

colnames(profDeveloperYears) <- c("Years_Programming", "Percentage")


profDeveloperYears$Years_Programming <- c("Less than a year", "1 to 2 years", "2 to 3 years", "3 to 4 years",
                                        "4 to 5 years", "5 to 6 years", "6 to 7 years", "7 to 8 years",
                                        "8 to 9 years", "9 to 10 years", "10 to 11 years", "11 to 12 years",
                                        "12 to 13 years", "13 to 14 years", "14 to 15 years", "15 to 16 years",
                                        "16 to 17 years", "17 to 18 years", "18 to 19 years", "19 to 20 years", "20 or more years") 


profDeveloperYears$Percentage <- c(1.10, 2.51, 3.72, 5.18, 
                                 6.53, 6.80, 5.85, 5.24,
                                 4.14, 7.08, 5.02, 3.32,
                                 3.18, 2.60, 4.76, 4.01,
                                 2.52, 2.15, 1.52, 2.43, 20.33)

str(profDeveloperYears)

profDeveloperYears$Years_Programming <- factor(profDeveloperYears$Years_Programming, levels = unique(as.character(profDeveloperYears$Years_Programming)))

theme_set(theme_bw())

ggplot(profDeveloperYears, aes(x= Years_Programming, y= Percentage))+ 
  geom_point(size=3, color = "Orange")+ 
  geom_segment(aes(x=Years_Programming, 
                   xend=Years_Programming, 
                   y=0, 
                   yend=Percentage), color = "Orange")+ 
  labs(title="Years Programming", 
       subtitle="Professional Developers Only", 
       caption="source: stackoverflow")+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
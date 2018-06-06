install.packages("ggplot2")

library(ggplot2)


# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)



surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE)

expAllRespondents <- subset.data.frame(surveyResultsPublic, select = c("YearsProgram"))

expAllRespondents <- na.omit(expAllRespondents)

count_expAllRespondents <- as.data.frame(table(unlist(expAllRespondents)))

View(count_expAllRespondents)

count_expAllRespondents$Percentage <- count_expAllRespondents$Freq / sum(count_expAllRespondents$Freq) * 100

colnames(count_expAllRespondents)[colnames(count_expAllRespondents)=="Var1"] <- "YearsProgramming"

programmingYears <- as.data.frame(matrix(NA, ncol = 2, nrow = 21))

colnames(programmingYears) <- c("Years_Programming", "Percentage")


programmingYears$Years_Programming <- c("Less than a year", "1 to 2 years", "2 to 3 years", "3 to 4 years",
                                        "4 to 5 years", "5 to 6 years", "6 to 7 years", "7 to 8 years",
                                        "8 to 9 years", "9 to 10 years", "10 to 11 years", "11 to 12 years",
                                        "12 to 13 years", "13 to 14 years", "14 to 15 years", "15 to 16 years",
                                        "16 to 17 years", "17 to 18 years", "18 to 19 years", "19 to 20 years", "20 or more years") 


programmingYears$Percentage <- c(2.90, 5.42, 6.36, 7.24, 
                                 7.56, 6.98, 5.61, 4.82,
                                 3.73, 6.25, 4.26, 2.72,
                                 2.56, 2.14, 3.93, 3.26,
                                 2.05, 1.72, 1.27, 2.00, 17.24)

str(programmingYears)

programmingYears$Years_Programming <- factor(programmingYears$Years_Programming, levels = unique(as.character(programmingYears$Years_Programming)))

theme_set(theme_bw())

ggplot(programmingYears, aes(x= Years_Programming, y= Percentage))+ 
  geom_point(size=3, color = "Orange")+ 
  geom_segment(aes(x=Years_Programming, 
                   xend=Years_Programming, 
                   y=0, 
                   yend=Percentage), color = "Orange")+ 
  labs(title="Years Programming", 
       subtitle="All Respondents", 
       caption="source: stackoverflow")+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()

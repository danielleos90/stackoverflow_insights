install.packages("ggplot2")

library(ggplot2)


# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)


# read in the CSV containing  football data and store it in a variable 
surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE)

expAllRespondents <- subset.data.frame(surveyResultsPublic, select = c("YearsProgram"))

count_expAllRespondents <- as.data.frame(table(unlist(expAllRespondents)))

View(count_expAllRespondents)

count_expAllRespondents$Percentage <- count_expAllRespondents$Freq / sum(count_expAllRespondents$Freq) * 100

colnames(count_expAllRespondents)[colnames(count_expAllRespondents)=="Var1"] <- "YearsProgramming"

programmingYears <- as.data.frame(matrix(NA, ncol = 2, nrow = 8))

colnames(programmingYears) <- c("Years_Programming", "Percentage")


programmingYears$Years_Programming <- c("0-2 Years", "3-5 Years", "6-8 Years", "9-11 Years",
                                        "12-14 Years", "15-17 Years", "18-20 Years", "20 Years or More") 


programmingYears$Percentage <- c(8.32, 21.15, 17.41, 14.24, 7.42, 9.24, 4.99, 17.23)

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
  
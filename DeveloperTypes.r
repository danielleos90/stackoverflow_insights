library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(plyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)


# read in the CSV containing  football data and store it in a variable 
surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE)


totalDeveloperType<- surveyResultsPublic  %>% group_by(DeveloperType2) %>%  tally()
totalDeveloperType2<-na.omit(totalDeveloperType) %>% group_by(DeveloperType2) %>% tally()
totalDeveloperType2$percent<- NA
totalDeveloperType2$percent<- totalDeveloperType2$nn / sum(totalDeveloperType2$nn)
totalDeveloperType2$percent2 <- sprintf("%.0f%%", 100 * totalDeveloperType2$percent)

ggplot(totalDeveloperType2, aes(x= as.factor(DeveloperType2), y= percent2))+ 
  geom_point(size=3, color = "Orange")+ 
  geom_segment(aes(x=DeveloperType2, 
                   xend=DeveloperType2, 
                   y=0, 
                   yend=percent2), color = "Orange")+ 
  labs(title="Developer Types", 
       subtitle="All Respondents", 
       caption="source: stackoverflow")+ 
  #theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+ xlab("Developer Types") + ylab("Percentage")+
  coord_flip()



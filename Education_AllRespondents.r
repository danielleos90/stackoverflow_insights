library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(plyr)
library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)



surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE)
surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

formalEducation <- subset.data.frame(surveyResultsPublic, select = c("FormalEducation"))
FormalEducationNew<- formalEducation %>% group_by(FormalEducation) %>%  tally() 

#Education All respondants plot
ggplot(FormalEducationNew, aes(x=as.factor(FormalEducation), y=n))+ 
  geom_point(aes(y = (n)/sum(n)),size=3,color="orange")+
  scale_y_continuous(labels=scales::percent) + 
  geom_segment(aes(x=FormalEducation, 
                   xend=FormalEducation,
                   y=0, 
                   yend=(n)/sum(n)), color = "Orange")+
  theme(axis.text.x = element_text(angle=45, vjust=0.6))+
  labs(title="Educational Attainment", 
       subtitle="All Respondents",
       caption="source: stackoverflow")+
  theme_light()+ xlab("Educational Attainment") + ylab("Percentage")+
  coord_flip()+scale_x_discrete(breaks=c("Some college/university study without earning a bachelor's degree","Secondary school","Professional degree", "Primary/elementary school", "Master's degree", "I prefer not to answer", "I never completed any formal education", "Doctoral degree", "Bachelor's degree"),
                                labels=c("college without earning degree","Secondary School","Professional Degree", "Primary/Elementary School", "Masters Degree", "Prefer not to answer", "No formal education", "Doctoral Degree", "Bachelors Degree"))


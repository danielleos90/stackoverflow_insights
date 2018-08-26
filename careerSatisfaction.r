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

careerSatisfaction <- subset.data.frame(surveyResultsPublic, select = c("CareerSatisfaction"))
careerSatisfaction<- na.omit(careerSatisfaction)

careerSatisfactionNew<- careerSatisfaction %>% group_by(CareerSatisfaction) %>%  tally() 

#Job Satisfaction plot
ggplot(careerSatisfactionNew, aes(x=as.factor(CareerSatisfaction), y = (n)/sum(n)))+
  geom_bar(stat="identity", fill="orange", width=0.4)+coord_flip()+scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs(title="How do Developers Feel about their Jobs and Careers?", 
       subtitle="Career Satisfaction (0- Not at all satisfied/ 10- completely satisfied)",
       caption="source: stackoverflow")+
  theme_light()+ xlab("Career Satisfaction") + ylab("Percentage")
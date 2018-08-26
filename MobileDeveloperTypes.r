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

MobileDevTypes <- subset.data.frame(surveyResultsPublic, select = c("MobileDeveloperType"))
MobileDevTypes <- na.omit(MobileDevTypes)
count_MobileTypes <- separate(data = MobileDevTypes, col = MobileDeveloperType, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")
newMobileType<- count_MobileTypes %>% group_by(Var1) %>%  tally() 

#Mobile developer types plot
ggplot(newMobileType, aes(x=as.factor(Var1), y=n))+ 
  geom_point(aes(y = (n)/sum(n)),size=3,color="orange")+
  scale_y_continuous(labels=scales::percent) + 
  geom_segment(aes(x=Var1, 
                   xend=Var1,
                   y=0, 
                   yend=(n)/sum(n)), color = "Orange")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs(title="Mobile Developer Types", 
       caption="source: stackoverflow")+
  theme_light()+ xlab("Mobile Developer Types") + ylab("Percentage")+
  coord_flip()
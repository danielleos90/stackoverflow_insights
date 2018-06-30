library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(plyr)


# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)



surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE)


# totalDeveloperType<- surveyResultsPublic  %>% group_by(DeveloperType2) %>%  tally()
# totalDeveloperType2<-na.omit(totalDeveloperType) %>% group_by(DeveloperType2) %>% tally()
# totalDeveloperType2$percent<- NA
# totalDeveloperType2$percent<- totalDeveloperType2$nn / sum(totalDeveloperType2$nn)
# totalDeveloperType2$percent2 <- sprintf("%.0f%%", 100 * totalDeveloperType2$percent)


#NEW
ibrary(ggplot2)

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)
surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)
DevTypes <- subset.data.frame(surveyResultsPublic, select = c("DeveloperType"))
DevTypes <- na.omit(DevTypes)
count_DevTypes <- separate(data = DevTypes, col = DeveloperType, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")
newDevType<- count_DevTypes %>% group_by(Var1) %>%  tally() 
#newDevType$percent<- NA
#newDevType$percent<- newDevType$n / sum(newDevType$n)
#newDevType$percent2 <- sprintf("%.0f%%", 100 * newDevType$percent)


# myplot <- ggplot(newDevType, aes(Var1)) + 
#   geom_bar(aes(y = (n)/sum(n)),stat="identity", fill="Orange") +coord_flip()+
#   scale_y_continuous(labels=scales::percent) +
#   ylab("relative frequencies")
  

ggplot(newDevType, aes(x=as.factor(Var1), y=n))+ 
  #geom_bar(aes(y = (n)/sum(n)),stat="identity", fill="orange")+
  geom_point(aes(y = (n)/sum(n)),size=3,color="orange")+
  #geom_point(size=3, color = "Orange")+
  scale_y_continuous(labels=scales::percent) + 
  geom_segment(aes(x=Var1, 
                   xend=Var1,
                   y=0, 
                   yend=(n)/sum(n)), color = "Orange")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs(title="Developer Types", 
       subtitle="All Respondents", 
       caption="source: stackoverflow")+
  theme_light()+ xlab("Developer Types") + ylab("Percentage")+
  coord_flip()
 


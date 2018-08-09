install.packages("ggplot2")

library(ggplot2)

library(dplyr)

install.packages("dplyr")

library(tidyr)

# select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)


surveyResultsPublic <- read.csv(fileToLoad, stringsAsFactors = FALSE, strip.white = TRUE)

genders_student <- subset(surveyResultsPublic, Professional == "Student")

View(genders_student)

genders_stu <- subset.data.frame(genders_student, select = c("Gender"))

genders_stu <- na.omit(genders_stu)

View(genders_stu)

count_student_genders <- separate(data = genders_stu, col = Gender, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")

View(count_student_genders)

student_occurences <- as.data.frame(table(unlist(count_student_genders)))

View(student_occurences)

str(student_occurences)

student_occurences$Var1 <- as.factor(student_occurences$Var1)

#combines Female gender
student_occurences[1, ] <- student_occurences[1, ] + student_occurences[5, ]
student_occurences[1, 1] <- "Female"
student_occurences <- student_occurences[-c(5),]

#combines Transgender 
student_occurences [4, ] <- student_occurences[4, ] + student_occurences[8, ]
student_occurences[4, 1] <- "Transgender"
student_occurences <- student_occurences[-c(8),]

#combines Other
student_occurences [3, ] <- student_occurences[3, ] + student_occurences[7, ]
student_occurences[3, 1] <- "Other"
student_occurences <- student_occurences[-c(7),]

#combines Gender Non-Conforming
student_occurences [2, ] <- student_occurences[2, ] + student_occurences[5, ]
student_occurences[2, 1] <- "Gender non-conforming"
student_occurences <- student_occurences[-c(5),]


student_occurences$Percentage <- student_occurences$Freq / sum(student_occurences$Freq) * 100


ggplot(student_occurences, aes(x= Var1, y= Percentage))+ 
  geom_point(size=3, color = "Orange")+ 
  geom_segment(aes(x=Var1, 
                   xend=Var1, 
                   y=0, 
                   yend=Percentage), color = "Orange")+ 
  labs(title="Gender", 
       subtitle="Students Only", 
       caption="source: stackoverflow")+ 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme_light()+
  coord_flip()
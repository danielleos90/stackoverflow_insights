
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  theme = "bootswatch-cerulean.css",
  navbarPage(title = "Stackoverflow Insights"),
  sidebarLayout(
    sidebarPanel(
      tags$ul(
        tags$li(a("Overview", href="#overview")), 
        tags$li(a("Developer Profile", href="#developer_profile")), 
        tags$li(a("Technology", href="#technology")), 
        tags$li(a("Work", href="#work")), 
        tags$li(a("Community", href="#community")), 
        tags$li(a("Methodology", href="#methodology"))
      )
    ),
    mainPanel(
      fluidRow(h4(id="overview","Overview")),
      fluidRow(h5("This year, over 100,000 developers told us how they learn, build their careers, which tools they're using, and what they want in a job.")),
      fluidRow(
        plotOutput("occurences_plot"),
        plotOutput("student_occurences_plot"),
        plotOutput("prof_occurences_plot")    
      ),
      fluidRow(h4(id="developer_profile","Developer Profile")),
      fluidRow(h5("Who codes? More people in more places than ever before.")),
      fluidRow(
        plotOutput("profDeveloperYears_plot"),
        plotOutput("ethnicity_occurences_plot"),
        plotOutput("ethnicity_prof_occurences_plot"),
        plotOutput("ethnicity_student_occurences_plot")
    ),
    fluidRow(h4(id="technology","Technology")),
    fluidRow(h4(id="work","Work")),
    fluidRow(h4(id="community","Community")),
    fluidRow(h4(id="methodology","Methodology"))
  )
))



server <- function(input, output){
  surveyResultsPublic <- read.csv("/Users/paulkelly/Dropbox/lecturing/GroupProject/stackoverflow_insights/datasets/survey_results_public.csv")
  
  #Gender All Respondents
  
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
  
  output$occurences_plot<-renderPlot({
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
  })
  
  #Years Learning to Code (Professional)
  
  expDevelopers <- subset.data.frame(surveyResultsPublic, select = c("YearsProgram", "Professional"))
  
  expDevelopers <- na.omit(expDevelopers)
  
  expDevelopers <- filter(expDevelopers, Professional == "Professional developer")
  
  count_expDevelopers <- as.data.frame(table(unlist(expDevelopers$YearsProgram)))
  
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
  
  
  profDeveloperYears$Years_Programming <- factor(profDeveloperYears$Years_Programming, levels = unique(as.character(profDeveloperYears$Years_Programming)))
  
  theme_set(theme_bw())
  
  output$proDeveloperYears_plot<-renderPlot({
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
  })
  
  
  #Genders Student
  genders_student <- subset(surveyResultsPublic, Professional == "Student")
  
  genders_stu <- subset.data.frame(genders_student, select = c("Gender"))
  
  genders_stu <- na.omit(genders_stu)
  
  count_student_genders <- separate(data = genders_stu, col = Gender, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")
  
  student_occurences <- as.data.frame(table(unlist(count_student_genders)))
  
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
  
  output$student_occurences_plot<-renderPlot({
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
  })
  
  
  # GENDERS PROFESSIONAL
  genders_professional <- subset(surveyResultsPublic, Professional == "Professional developer")
  
  genders_profdev <- subset.data.frame(genders_professional, select = c("Gender"))
  
  genders_profdev <- na.omit(genders_profdev)
  
  count_prof_genders <- separate(data = genders_profdev, col = Gender, into = c("Var1", "Var2", "Var3", "Var4", "Var5"), sep = ";")
  
  prof_occurences <- as.data.frame(table(unlist(count_prof_genders)))
  
  str(prof_occurences)
  
  prof_occurences$Var1 <- as.factor(prof_occurences$Var1)
  
  #combines Female gender
  prof_occurences[1, ] <- prof_occurences[1, ] + prof_occurences[5, ]
  prof_occurences[1, 1] <- "Female"
  prof_occurences <- prof_occurences[-c(5),]
  
  #combines Transgender 
  prof_occurences [4, ] <- prof_occurences[4, ] + prof_occurences[8, ]
  prof_occurences[4, 1] <- "Transgender"
  prof_occurences <- prof_occurences[-c(8),]
  
  #combines Other
  prof_occurences [3, ] <- prof_occurences[3, ] + prof_occurences[7, ]
  prof_occurences[3, 1] <- "Other"
  prof_occurences <- prof_occurences[-c(7),]
  
  #combines Gender Non-Conforming
  prof_occurences [2, ] <- prof_occurences[2, ] + prof_occurences[5, ]
  prof_occurences[2, 1] <- "Gender non-conforming"
  prof_occurences <- prof_occurences[-c(5),]
  
  
  prof_occurences$Percentage <- prof_occurences$Freq / sum(prof_occurences$Freq) * 100
  
  
  output$prof_occurences_plot<-renderPlot({
    ggplot(prof_occurences, aes(x= Var1, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Var1, 
                       xend=Var1, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Gender", 
           subtitle="Professional Developers Only", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  # ETHNICITY All
  
     ethnicity_all <- subset(surveyResultsPublic, select = c("Race"))
     ethnicity_all <- na.omit(ethnicity_all)
     count_ethnicity_all <- separate(data = ethnicity_all, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")
  
     ethnicity_occurences <- as.data.frame(table(unlist(count_ethnicity_all)))
  
     str(ethnicity_occurences)
  
     ethnicity_occurences$Var1 <- as.character(ethnicity_occurences$Var1)
  
     #combines East Asian Ethnicity
     ethnicity_occurences[1, ] <- ethnicity_occurences[1, ] + ethnicity_occurences[10, ]
     ethnicity_occurences[1, 1] <- "East Asian"
     ethnicity_occurences <- ethnicity_occurences[-c(10),]
  
     #combines Hispanic or Latino/Latina 
     ethnicity_occurences [2, ] <- ethnicity_occurences[2, ] + ethnicity_occurences[10, ]
     ethnicity_occurences[2, 1] <- "Hispanic or Latino/Latina"
     ethnicity_occurences <- ethnicity_occurences[-c(10),]
  
     #combines I prefer not to say
     ethnicity_occurences [4, ] <- ethnicity_occurences[4, ] + ethnicity_occurences[11, ]
     ethnicity_occurences[4, 1] <- "I prefer not to say"
     ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
     #combines Middle Eastern
     ethnicity_occurences [5, ] <- ethnicity_occurences[5, ] + ethnicity_occurences[11, ]
     ethnicity_occurences[5, 1] <- "Middle Eastern"
     ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
     #combines Native American, Pacific Islander, or Indigenous Australian
     ethnicity_occurences [6, ] <- ethnicity_occurences[6, ] + ethnicity_occurences[11, ]
     ethnicity_occurences[6, 1] <- "Native American, Pacific Islander, or Indigenous Australian"
     ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
     #combines South Asian
     ethnicity_occurences [7, ] <- ethnicity_occurences[7, ] + ethnicity_occurences[11, ]
     ethnicity_occurences[7, 1] <- "South Asian"
     ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
     #combines White or of European descent
     ethnicity_occurences [8, ] <- ethnicity_occurences[8, ] + ethnicity_occurences[11, ]
     ethnicity_occurences[8, 1] <- "White or of European descent"
     ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
     #combines I don't Know
     ethnicity_occurences [3, ] <- ethnicity_occurences[3, ] + ethnicity_occurences[10, ]
     ethnicity_occurences[3, 1] <- "I don't Know"
     ethnicity_occurences <- ethnicity_occurences[-c(10),]
  
     ethnicity_occurences$Percentage <- ethnicity_occurences$Freq / sum(ethnicity_occurences$Freq) * 100
  
     ethnicity_occurences$Percentage <- round(ethnicity_occurences$Percentage, digits = 2)
  
     output$ethnicity_occurences_plot <- renderPlot({
       ggplot(ethnicity_occurences, aes(Var1, Percentage))
       plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
         geom_text(aes(label = Percentage, y = Percentage), vjust = 0, hjust = 0.5)+
         labs(title="Ethnicity", 
              subtitle="All Respondents", 
              caption="source: stackoverflow") +
         theme(axis.text.x = element_text(angle=65, vjust=0.6))+
         theme_light()+
         coord_flip()
     })
     
  # Ethnicity Professional
     ethnicity_professional <- subset(surveyResultsPublic, Professional == "Professional developer")
     
     View(ethnicity_professional)
     
     ethnicity_profdev <- subset.data.frame(ethnicity_professional, select = c("Race"))
     
     ethnicity_profdev <- na.omit(ethnicity_profdev)
     
     View(ethnicity_profdev)
     
     count_ethnicity_profdev <- separate(data = ethnicity_profdev, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")
     
     View(count_ethnicity_profdev)
     
     ethnicity_prof_occurences <- as.data.frame(table(unlist(count_ethnicity_profdev)))
     
     View(ethnicity_prof_occurences)
     
     str(ethnicity_prof_occurences)
     
     ethnicity_prof_occurences$Var1 <- as.factor(ethnicity_prof_occurences$Var1)
     
     #combines East Asian Ethnicity
     ethnicity_prof_occurences[1, ] <- ethnicity_prof_occurences[1, ] + ethnicity_prof_occurences[10, ]
     ethnicity_prof_occurences[1, 1] <- "East Asian"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(10),]
     
     #combines Hispanic or Latino/Latina 
     ethnicity_prof_occurences [2, ] <- ethnicity_prof_occurences[2, ] + ethnicity_prof_occurences[10, ]
     ethnicity_prof_occurences[2, 1] <- "Hispanic or Latino/Latina"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(10),]
     
     #combines I prefer not to say
     ethnicity_prof_occurences [4, ] <- ethnicity_prof_occurences[4, ] + ethnicity_prof_occurences[11, ]
     ethnicity_prof_occurences[4, 1] <- "I prefer not to say"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(11),]
     
     #combines Middle Eastern
     ethnicity_prof_occurences [5, ] <- ethnicity_prof_occurences[5, ] + ethnicity_prof_occurences[11, ]
     ethnicity_prof_occurences[5, 1] <- "Middle Eastern"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(11),]
     
     #combines Native American, Pacific Islander, or Indigenous Australian
     ethnicity_prof_occurences [6, ] <- ethnicity_prof_occurences[6, ] + ethnicity_prof_occurences[11, ]
     ethnicity_prof_occurences[6, 1] <- "Native American, Pacific Islander, or Indigenous Australian"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(11),]
     
     #combines South Asian
     ethnicity_prof_occurences [7, ] <- ethnicity_prof_occurences[7, ] + ethnicity_prof_occurences[11, ]
     ethnicity_prof_occurences[7, 1] <- "South Asian"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(11),]
     
     #combines White or of European descent
     ethnicity_prof_occurences [8, ] <- ethnicity_prof_occurences[8, ] + ethnicity_prof_occurences[11, ]
     ethnicity_prof_occurences[8, 1] <- "White or of European descent"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(11),]
     
     #combines I don't Know
     ethnicity_prof_occurences [3, ] <- ethnicity_prof_occurences[3, ] + ethnicity_prof_occurences[10, ]
     ethnicity_prof_occurences[3, 1] <- "I don't Know"
     ethnicity_prof_occurences <- ethnicity_prof_occurences[-c(10),]
     
     ethnicity_prof_occurences$Var1 <- as.character(ethnicity_prof_occurences$Var1)
     
     ethnicity_prof_occurences[3, 1] <- "I don't Know"
     
     ethnicity_prof_occurences$Percentage <- ethnicity_prof_occurences$Freq / sum(ethnicity_prof_occurences$Freq) * 100
     
     ethnicity_prof_occurences$Percentage <- round(ethnicity_prof_occurences$Percentage, digits = 2)
     
     output$ethnicity_prof_occurences_plot <-renderPlot({
       ggplot(ethnicity_prof_occurences, aes(Var1, Percentage))
       plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
         labs(title="Ethnicity", 
              subtitle="Professional Developers", 
              caption="source: stackoverflow") +
         theme(axis.text.x = element_text(angle=65, vjust=0.6))+
         theme_light()+
         coord_flip()
     })

  #Ethnicity Student
     
     ethnicity_student <- subset(surveyResultsPublic, Professional == "Student")
     
     View(ethnicity_student)
     
     ethnicity_students <- subset.data.frame(ethnicity_student, select = c("Race"))
     
     ethnicity_students <- na.omit(ethnicity_students)
     
     View(ethnicity_students)
     
     count_ethnicity_students <- separate(data = ethnicity_students, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")
     
     View(count_ethnicity_students)
     
     ethnicity_student_occurences <- as.data.frame(table(unlist(count_ethnicity_students)))
     
     View(ethnicity_student_occurences)
     
     str(ethnicity_student_occurences)
     
     #combines East Asian Ethnicity
     ethnicity_student_occurences[1, ] <- ethnicity_student_occurences[1, ] + ethnicity_student_occurences[10, ]
     ethnicity_student_occurences[1, 1] <- "East Asian"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(10),]
     
     #combines Hispanic or Latino/Latina 
     ethnicity_student_occurences [2, ] <- ethnicity_student_occurences[2, ] + ethnicity_student_occurences[10, ]
     ethnicity_student_occurences[2, 1] <- "Hispanic or Latino/Latina"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(10),]
     
     #combines I prefer not to say
     ethnicity_student_occurences [4, ] <- ethnicity_student_occurences[4, ] + ethnicity_student_occurences[11, ]
     ethnicity_student_occurences[4, 1] <- "I prefer not to say"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]
     
     #combines Middle Eastern
     ethnicity_student_occurences [5, ] <- ethnicity_student_occurences[5, ] + ethnicity_student_occurences[11, ]
     ethnicity_student_occurences[5, 1] <- "Middle Eastern"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]
     
     #combines Native American, Pacific Islander, or Indigenous Australian
     ethnicity_student_occurences [6, ] <- ethnicity_student_occurences[6, ] + ethnicity_student_occurences[11, ]
     ethnicity_student_occurences[6, 1] <- "Native American, Pacific Islander, or Indigenous Australian"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]
     
     #combines South Asian
     ethnicity_student_occurences [7, ] <- ethnicity_student_occurences[7, ] + ethnicity_student_occurences[11, ]
     ethnicity_student_occurences[7, 1] <- "South Asian"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]
     
     #combines White or of European descent
     ethnicity_student_occurences [8, ] <- ethnicity_student_occurences[8, ] + ethnicity_student_occurences[11, ]
     ethnicity_student_occurences[8, 1] <- "White or of European descent"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(11),]
     
     #combines I don't Know
     ethnicity_student_occurences [3, ] <- ethnicity_student_occurences[3, ] + ethnicity_student_occurences[10, ]
     ethnicity_student_occurences[3, 1] <- "I don't Know"
     ethnicity_student_occurences <- ethnicity_student_occurences[-c(10),]
     
     ethnicity_student_occurences$Var1 <- as.character(ethnicity_student_occurences$Var1)
     
     ethnicity_student_occurences$Percentage <- ethnicity_student_occurences$Freq / sum(ethnicity_student_occurences$Freq) * 100
     
     ethnicity_student_occurences$Percentage <- round(ethnicity_student_occurences$Percentage, digits = 2)
     
     output$ethnicity_student_occurences_plot <-renderPlot({
       ggplot(ethnicity_student_occurences, aes(Var1, Percentage))
       plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
         labs(title="Ethnicity", 
              subtitle="Professional Developers", 
              caption="source: stackoverflow") +
         theme(axis.text.x = element_text(angle=65, vjust=0.6))+
         theme_light()+
         coord_flip()
     })
          
  #   output$plot<-renderPlot({plot})
  
}
shinyApp(ui = ui, server = server)

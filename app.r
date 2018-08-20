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
      fluidRow(h5("This year, over 100,000 developers told us how they learn, build their careers, which tools they’re using, and what they want in a job.")),
      fluidRow(
        plotOutput("student_occurences_plot"),
        plotOutput("prof_occurences_plot")    
      ),
      fluidRow(h4(id="developer_profile","Developer Profile")),
      fluidRow(h4(id="technology","Technology")),
      fluidRow(h4(id="work","Work")),
      fluidRow(h4(id="community","Community")),
      fluidRow(h4(id="methodology","Methodology"))
    )
  )
)



server <- function(input, output){
  surveyResultsPublic <- read.csv("/Users/paulkelly/Dropbox/lecturing/GroupProject/stackoverflow_insights/datasets/survey_results_public.csv")
  
  
  
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
  
  # # ETHNICITY
  
  #   ethnicity_all <- subset(surveyResultsPublic, select = c("Race"))
  #   ethnicity_all <- na.omit(ethnicity_all)
  #   count_ethnicity_all <- separate(data = ethnicity_all, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")
  #   count_ethnicity_all <- separate(data = ethnicity_all, col = Race, into = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9"), sep = ";")
  
  #   ethnicity_occurences <- as.data.frame(table(unlist(count_ethnicity_all)))
  
  #   str(ethnicity_occurences)
  
  #   ethnicity_occurences$Var1 <- as.character(ethnicity_occurences$Var1)
  
  #   #combines East Asian Ethnicity
  #   ethnicity_occurences[1, ] <- ethnicity_occurences[1, ] + ethnicity_occurences[10, ]
  #   ethnicity_occurences[1, 1] <- "East Asian"
  #   ethnicity_occurences <- ethnicity_occurences[-c(10),]
  
  #   #combines Hispanic or Latino/Latina 
  #   ethnicity_occurences [2, ] <- ethnicity_occurences[2, ] + ethnicity_occurences[10, ]
  #   ethnicity_occurences[2, 1] <- "Hispanic or Latino/Latina"
  #   ethnicity_occurences <- ethnicity_occurences[-c(10),]
  
  #   #combines I prefer not to say
  #   ethnicity_occurences [4, ] <- ethnicity_occurences[4, ] + ethnicity_occurences[11, ]
  #   ethnicity_occurences[4, 1] <- "I prefer not to say"
  #   ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
  #   #combines Middle Eastern
  #   ethnicity_occurences [5, ] <- ethnicity_occurences[5, ] + ethnicity_occurences[11, ]
  #   ethnicity_occurences[5, 1] <- "Middle Eastern"
  #   ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
  #   #combines Native American, Pacific Islander, or Indigenous Australian
  #   ethnicity_occurences [6, ] <- ethnicity_occurences[6, ] + ethnicity_occurences[11, ]
  #   ethnicity_occurences[6, 1] <- "Native American, Pacific Islander, or Indigenous Australian"
  #   ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
  #   #combines South Asian
  #   ethnicity_occurences [7, ] <- ethnicity_occurences[7, ] + ethnicity_occurences[11, ]
  #   ethnicity_occurences[7, 1] <- "South Asian"
  #   ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
  #   #combines White or of European descent
  #   ethnicity_occurences [8, ] <- ethnicity_occurences[8, ] + ethnicity_occurences[11, ]
  #   ethnicity_occurences[8, 1] <- "White or of European descent"
  #   ethnicity_occurences <- ethnicity_occurences[-c(11),]
  
  #   #combines I don't Know
  #   ethnicity_occurences [3, ] <- ethnicity_occurences[3, ] + ethnicity_occurences[10, ]
  #   ethnicity_occurences[3, 1] <- "I don't Know"
  #   ethnicity_occurences <- ethnicity_occurences[-c(10),]
  
  #   ethnicity_occurences$Percentage <- ethnicity_occurences$Freq / sum(ethnicity_occurences$Freq) * 100
  
  #   ethnicity_occurences$Percentage <- round(ethnicity_occurences$Percentage, digits = 2)
  
  #   plot <- ggplot(ethnicity_occurences, aes(Var1, Percentage))
  #   plot + geom_bar(stat="identity", width = 0.5, fill="Orange") +
  #     geom_text(aes(label = Percentage, y = Percentage), vjust = 0, hjust = 0.5)+
  #     labs(title="Ethnicity", 
  #          subtitle="All Respondents", 
  #          caption="source: stackoverflow") +
  #     theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  #     theme_light()+
  #     coord_flip()
  
  #   output$plot<-renderPlot({plot})
  
}
shinyApp(ui = ui, server = server)
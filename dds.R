library(shiny)
library(ggmap)
library(maps)
library(stringr)

library (readr)
library(tidyr)
library(plyr)
library(plotly)
library(dplyr)




ui <- fluidPage(
  theme = "bootswatch-cerulean.css",
  #includeCSS(),
  navbarPage(title = "Stackoverflow Insights"),
  sidebarLayout(
    sidebarPanel(
      
      tags$ul(
        tags$li(a("Overview", href="#overview")),
        tags$li(a("Developer Profile", href="#developer_profile")), 
        tags$ol(
          tags$li(a("Geography", href="#Geography")),
          tags$li(a("Developer Roles", href="#Developer Roles")),
          tags$li(a("Experience", href="#Experience")),
          tags$li(a("Education", href="#Education")),
          tags$li(a("Demographics", href="#Demographics")),
          tags$li(a("Connection and Competition", href="#Connection and Competition")),
          tags$li(a("Life Outside Work", href="#Life Outside Work"))),
        
        
        tags$li(a("Technology", href="#technology")), 
        tags$li(a("Work", href="#work")), 
        tags$li(a("Community", href="#community")), 
        tags$li(a("Methodology", href="#methodology"))
      )
    ),
    mainPanel(
      fluidRow(h4(id="overview","Overview")),
      fluidRow(h5("This year, over 100,000 developers told us how they learn, build their careers, which tools they're using, and what they want in a job.")),
      tags$p("Each year, we ask the developer community about everything from their favorite technologies to their job preferences. This year marks the eighth year we've published our Annual Developer Survey results-with the largest number of respondents yet. Over 100,000 developers took the 30-minute survey this past January."),
      tags$p("This year, we covered a few new topics ranging from artificial intelligence to ethics in coding. Here are a few of the top takeaways from this year's results:"),
      tags$ul(
        tags$li(
          "DevOps and machine learning are important trends in the software industry today. Languages and frameworks associated with these kinds of works are on the rise, and developers working in these areas command the highest salaries."
        ),
        tags$li("Only tiny fractions of developers say that they would write unethical code or that they have no obligation to consider the ethical implications of code, but beyond that, respondents see a lot of ethical gray. Developers are not sure how they would report ethical problems, and have differing ideas about who ultimately is responsible for unethical code."),
        tags$li("Developers are overall optimistic about the possibilities that artificial intelligence offers, but are not in agreement about what the dangers of AI are."),
        tags$li("Python has risen in the ranks of programming languages on our survey, surpassing C# in popularity this year, much like it surpassed PHP last year."),
        tags$li("When assessing a prospective job, different kinds of developers apply different sets of priorities. Women say their highest priorities are company culture and opportunities for professional development, while men say their highest priorities are compensation and working with specific technologies.")
      ),
      
      fluidRow(
        #plotOutput("student_occurences_plot"),
        #plotOutput("prof_occurences_plot")
        
      ),
      fluidRow(h4(id="developer_profile","Developer Profile")),
      fluidRow(h4(id="Geography","Geography")),
      
      #tabsetPanel(
        #tabPanel("Survey Respondents", plotlyOutput("respond_plot")), 
        #tabPanel("Professional Developers", plotlyOutput("prof_devs"))),
      
      tags$div(
        tags$p("Each month, about 50 million people visit Stack Overflow to learn, share, and build their careers. We estimate that 21 million of these people are professional developers and university-level students."),
        tags$p("Our estimate of professional developers comes from the things people read and do when they visit Stack Overflow. We collect data on user activity to help surface jobs we think you might find interesting and questions we think you can answer. You can download and clear this data at any time.")),
      
      fluidRow(
        plotOutput("gender_all_plot"),
        plotOutput("gender_student_plot"),
        plotOutput("gender_professional_plot"), 
        plotOutput("profDeveloperYears_plot"),
        plotOutput("ethnicity_all_plot"),
        plotOutput("ethnicity_student_plot"),
        plotOutput("ethnicity_professional_plot"),
        plotOutput("parents_all_plot"),
        plotOutput("parents_professional_plot"),
        plotOutput("parents_student_plot"),
        plotOutput("education_all_plot"),
        plotOutput("education_professional_plot"),
        plotOutput("web_developer_plot"),
        plotOutput("mobile_developer_plot"),
        plotOutput("other_occupation_plot")
      ),
      fluidRow(h4("Developer Roles", href="#Developer Roles")),
      
      tags$div(
        tags$p("Almost 60% of respondents identify as back-end developers, and about 20% consider themselves mobile developers. The median number of developer type identifications per respondent is 2, and the most common pairs are combinations of back-end, front-end, and full-stack developer. Pairs that are highly correlated are database administrator and system administrator, DevOps specialist and system administrator, and designer and front-end developer.")
      ),
      
      fluidRow(h4("Experience", href="#Experience")),
      
      tags$div(
        tags$p()),
      
      fluidRow(h4("Education", href="#Education")),
      
      tags$div(
        tags$p()),
      
      fluidRow(h4("Demographics", href="#Demographics")),
      
      tags$div(
        tags$p()),
      
      fluidRow(h4("Connection and Competition", href="#Connection and Competition")),
      
      tags$div(
        tags$p()),
      
      fluidRow(h4("Life Outside Work", href="#Life Outside Work")),
      tags$div(
        tags$p()),
      
      fluidRow(h4(id="technology","Technology")),
      fluidRow(h4(id="work","Work")),
      fluidRow(h4(id="community","Community")),
      fluidRow(h4(id="methodology","Methodology"))
    )
  )
)



server <- function(input, output){
  surveyResultsPublic <- read_csv("C:/Users/amonk/Downloads/dataset.csv")
  
  #Geography - map
  country_codes<- read_csv("C:/Users/amonk/Downloads/countries.csv")
  #cleaning up data
  #View(country_codes)
  country_codes <- country_codes[c(-1)]
  country_codes <- country_codes[c(3,1,2)]
  colnames(country_codes) <- c("Country", "long", "lat") 
  #update
  results_pruned <- surveyResultsPublic[c(1:7)]
  
  #1. survey respondents - subset on country and sum of respondants
  
  respondents <- results_pruned[c("Respondent", "Country")]
  #plyr
  respondents_count <- count(respondents, "Country")
  
  #merge respondents_count with the clean county code data using the plyr package. very clean
  respondents_merged <- merge(respondents_count, country_codes, by = "Country")
  respondents_merged <- na.omit(respondents_merged)
  respondents_merged <- subset(respondents_merged, freq > 5)
  
  #percentages
  respondents_merged$percentage <- (respondents_merged$freq/sum(respondents_merged$freq))*100
  #2 decimal places
  respondents_merged$percentage2 <- format(round(respondents_merged$percentage, 2), nsmall = 2)
  
  # Get the world polygon and extract U
  world <- map_data("world")
  
  #map for respondants by country
  respond <- ggplot(respondents_merged, aes()) + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#efefea", col="lightgrey")+xlab("")+ylab("") + 
    coord_fixed(1.3) + geom_point(data = respondents_merged, aes(x = lat, y = long, text = paste(Country,":", percentage2, "percentage of survey respondents")),color = "blue", size = respondents_merged$freq/800, alpha = 0.4)+xlab("")+ylab("")
  
  
  #2. monthly stack overflow visits -
  
  
  
  #3. professional developers - subset on developer column for professional developers and sum by country
  
  professional_dev <- subset(results_pruned, Professional == "Professional developer")
  #below I use the count function from plyr to perform count of country occurence
  
  pro_count <- count(respondents, "Country")
  
  #merge pro with the clean county code data using the plyr package. very clean
  pro_merged <- merge(pro_count, country_codes, by = "Country")
  pro_merged <- na.omit(pro_merged)
  
  
  pro_merged$percentage <- (pro_merged$freq/sum(pro_merged$freq))*100
  #2 decimal places
  
  pro_merged$percentage2 <- format(round(pro_merged$percentage, 2), nsmall = 2)
  
  
  prof_devs<-ggplot(respondents_merged, aes()) + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#efefea", color="lightgrey")+xlab("")+ylab("") + 
    coord_fixed(1.3) + geom_point(data = respondents_merged, aes(x = lat, y = long, text = paste(Country,":", percentage2, "percentage of survey respondents")), color = "blue", size = respondents_merged$freq/800, alpha = 0.4)+xlab("")+ylab("")
  
  
  #map 1
  output$respond_plot<-renderPlotly({ggplotly(respond)
  })
  #map 3
  output$prof_devs<-renderPlotly({ggplotly(prof_devs)
  })

  
  ###DEVELOPER PROFILE
  
  
  #Gender All Respondents
  
  genders_all <- subset.data.frame(surveyResultsPublic, select = c("Gender"))
  
  genders_all <- na.omit(genders_all)
  
  genders_all$Gender <- as.character(genders_all$Gender)
  
  #create empty dataframe that for gender and count of gender
  gender_all_df <- data.frame(matrix(ncol = 2, nrow = 5))
  
  gender_all_df_x <- c("Gender", "Count")
  
  colnames(gender_all_df) <- gender_all_df_x
  
  All_gender <- c("Female", "Male", "Gender non-conforming", "Other", "Transgender")
  
  gender_all_df$Gender <- All_gender
  
  gender_all_df
  
  #count the frequency of each gender
  gender_all_df[2, 2] <- sum(str_count(genders_all, "Male"))
  
  gender_all_df[1, 2] <- sum(str_count(genders_all, "Female"))
  
  gender_all_df[3, 2] <- sum(str_count(genders_all, "Gender non-conforming"))
  
  gender_all_df[4, 2] <- sum(str_count(genders_all, "Other"))
  
  gender_all_df[5, 2] <- sum(str_count(genders_all, "Transgender"))
  
  gender_all_df$Percentage <- gender_all_df$Count / sum(gender_all_df$Count) * 100
  
  output$gender_all_plot<- renderPlot({
    ggplot(gender_all_df, aes(x= Gender, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Gender, 
                       xend=Gender, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Gender", 
           subtitle="All Respondents", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  }) 
  
  ###Gender - Students
  genders_students <- subset(surveyResultsPublic, Professional == "Student")
  
  genders_student <- subset.data.frame(genders_students, select = c("Gender"))
  
  genders_student <- na.omit(genders_student)
  
  genders_student$Gender <- as.character(genders_student$Gender)
  
  #create empty dataframe that for gender and count of gender
  gender_student_df <- data.frame(matrix(ncol = 2, nrow = 5))
  
  gender_student_df_x <- c("Gender", "Count")
  
  colnames(gender_student_df) <- gender_student_df_x
  
  Student_Gender <- c("Female", "Male", "Gender non-conforming", "Other", "Transgender")
  
  gender_student_df$Gender <- Student_Gender
  
  gender_student_df
  
  #count the frequency of each gender
  gender_student_df[2, 2] <- sum(str_count(genders_student, "Male"))
  
  gender_student_df[1, 2] <- sum(str_count(genders_student, "Female"))
  
  gender_student_df[3, 2] <- sum(str_count(genders_student, "Gender non-conforming"))
  
  gender_student_df[4, 2] <- sum(str_count(genders_student, "Other"))
  
  gender_student_df[5, 2] <- sum(str_count(genders_student, "Transgender"))
  
  gender_student_df$Percentage <- gender_student_df$Count / sum(gender_student_df$Count) * 100
  
  output$gender_student_plot<- renderPlot({
    ggplot(gender_student_df, aes(x= Gender, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Gender, 
                       xend=Gender, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Gender", 
           subtitle="All Respondents", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()  
  })
  
  
  # Gender - Professional
  genders_professional <- subset(surveyResultsPublic, Professional == "Professional developer")
  
  genders_profdev <- subset.data.frame(genders_professional, select = c("Gender"))
  
  genders_profdev <- na.omit(genders_profdev)
  
  genders_profdev$Gender <- as.character(genders_profdev$Gender)
  
  #create empty dataframe that for gender and count of gender
  gender_professional_df <- data.frame(matrix(ncol = 2, nrow = 5))
  
  gender_professional_df_x <- c("Gender", "Count")
  
  colnames(gender_professional_df) <- gender_professional_df_x
  
  Professional_gender <- c("Female", "Male", "Gender non-conforming", "Other", "Transgender")
  
  gender_professional_df$Gender <- Professional_gender
  
  gender_professional_df
  
  #count the frequency of each gender
  gender_professional_df[2, 2] <- sum(str_count(genders_profdev, "Male"))
  
  gender_professional_df[1, 2] <- sum(str_count(genders_profdev, "Female"))
  
  gender_professional_df[3, 2] <- sum(str_count(genders_profdev, "Gender non-conforming"))
  
  gender_professional_df[4, 2] <- sum(str_count(genders_profdev, "Other"))
  
  gender_professional_df[5, 2] <- sum(str_count(genders_profdev, "Transgender"))
  
  gender_professional_df$Percentage <- gender_professional_df$Count / sum(gender_professional_df$Count) * 100
  
  output$gender_professional_plot<- renderPlot({
    ggplot(gender_professional_df, aes(x= Gender, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Gender, 
                       xend=Gender, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Gender", 
           subtitle="Professional Developers Only", 
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
  
  #Ethnicity - Students
  ethnicity_students <- subset(surveyResultsPublic, Professional == "Student")
  
  ethnicity_student <- subset.data.frame(ethnicity_students, select = c("Race"))
  
  ethnicity_student <- na.omit(ethnicity_student)
  
  ethnicity_student$Race <- as.character(ethnicity_student$Race)
  
  #create empty dataframe that for race and count of race
  ethnicity_student_df <- data.frame(matrix(ncol = 2, nrow = 9))
  
  ethnicity_student_df_x <- c("Race", "Count")
  
  colnames(ethnicity_student_df) <- ethnicity_student_df_x
  
  Student_Ethnicity <- c("East Asian", "Hispanic or Latino/Latina", "I don't know", "I prefer not to say", "Middle Eastern", "Native American, Pacific Islander, or Indigenous Australian", "South Asian", "White or of European Decent", "Black or of African Decent")
  
  ethnicity_student_df$Race <- Student_Ethnicity
  
  ethnicity_student_df
  
  #count the frequency of each Ethnicity
  ethnicity_student_df[1, 2] <- sum(str_count(ethnicity_student, "East Asian"))
  
  ethnicity_student_df[2, 2] <- sum(str_count(ethnicity_student, "Hispanic or Latino/Latina"))
  
  ethnicity_student_df[3, 2] <- sum(str_count(ethnicity_student, "I don't know"))
  
  ethnicity_student_df[4, 2] <- sum(str_count(ethnicity_student, "I prefer not to say"))
  
  ethnicity_student_df[5, 2] <- sum(str_count(ethnicity_student, "Middle Eastern"))
  
  ethnicity_student_df[6, 2] <- sum(str_count(ethnicity_student, "Native American, Pacific Islander, or Indigenous Australian"))
  
  ethnicity_student_df[7, 2] <- sum(str_count(ethnicity_student, "South Asian"))
  
  ethnicity_student_df[8, 2] <- sum(str_count(ethnicity_student, "White or of European descent"))
  
  ethnicity_student_df[9, 2] <- sum(str_count(ethnicity_student, "Black or of African descent"))
  
  ethnicity_student_df$Percentage <- ethnicity_student_df$Count / sum(ethnicity_student_df$Count) * 100
  
  output$ethnicity_student_plot<- renderPlot({
    ggplot(ethnicity_student_df, aes(x= Race, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Race, 
                       xend=Race, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Race", 
           subtitle="Students", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  # Ethnicity - Professional
  
  ethnicity_professionals <- subset(surveyResultsPublic, Professional == "Professional developer")
  
  ethnicity_professional <- subset.data.frame(ethnicity_professionals, select = c("Race"))
  
  ethnicity_professional <- na.omit(ethnicity_professional)
  
  ethnicity_professional$Race <- as.character(ethnicity_professional$Race)
  
  #create empty dataframe that for race and count of race
  ethnicity_professional_df <- data.frame(matrix(ncol = 2, nrow = 9))
  
  ethnicity_professional_df_x <- c("Race", "Count")
  
  colnames(ethnicity_professional_df) <- ethnicity_professional_df_x
  
  Professional_Ethnicity <- c("East Asian", "Hispanic or Latino/Latina", "I don't know", "I prefer not to say", "Middle Eastern", "Native American, Pacific Islander, or Indigenous Australian", "South Asian", "White or of European Decent", "Black or of African Decent")
  
  ethnicity_professional_df$Race <- Professional_Ethnicity
  
  ethnicity_professional_df
  
  #count the frequency of each ethnicity
  ethnicity_professional_df[1, 2] <- sum(str_count(ethnicity_professional, "East Asian"))
  
  ethnicity_professional_df[2, 2] <- sum(str_count(ethnicity_professional, "Hispanic or Latino/Latina"))
  
  ethnicity_professional_df[3, 2] <- sum(str_count(ethnicity_professional, "I don't know"))
  
  ethnicity_professional_df[4, 2] <- sum(str_count(ethnicity_professional, "I prefer not to say"))
  
  ethnicity_professional_df[5, 2] <- sum(str_count(ethnicity_professional, "Middle Eastern"))
  
  ethnicity_professional_df[6, 2] <- sum(str_count(ethnicity_professional, "Native American, Pacific Islander, or Indigenous Australian"))
  
  ethnicity_professional_df[7, 2] <- sum(str_count(ethnicity_professional, "South Asian"))
  
  ethnicity_professional_df[8, 2] <- sum(str_count(ethnicity_professional, "White or of European descent"))
  
  ethnicity_professional_df[9, 2] <- sum(str_count(ethnicity_professional, "Black or of African descent"))
  
  ethnicity_professional_df$Percentage <- ethnicity_professional_df$Count / sum(ethnicity_professional_df$Count) * 100
  
  output$ethnicity_professional_plot<- renderPlot({
    ggplot(ethnicity_professional_df, aes(x= Race, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Race, 
                       xend=Race, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Race", 
           subtitle="Professional", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  # Ethnicity - All Respondents
  ethnicity_all <- subset.data.frame(surveyResultsPublic, select = c("Race"))
  
  ethnicity_all <- na.omit(ethnicity_all)
  
  ethnicity_all$Race <- as.character(ethnicity_all$Race)
  
  #create empty dataframe that for race and count of race
  ethnicity_all_df <- data.frame(matrix(ncol = 2, nrow = 9))
  
  ethnicity_all_df_x <- c("Race", "Count")
  
  colnames(ethnicity_all_df) <- ethnicity_all_df_x
  
  all_Ethnicity <- c("East Asian", "Hispanic or Latino/Latina", "I don't know", "I prefer not to say", "Middle Eastern", "Native American, Pacific Islander, or Indigenous Australian", "South Asian", "White or of European Decent", "Black or of African Decent")
  
  ethnicity_all_df$Race <- all_Ethnicity
  
  ethnicity_all_df
  
  #count the frequency of each ethnicity
  ethnicity_all_df[1, 2] <- sum(str_count(ethnicity_all, "East Asian"))
  
  ethnicity_all_df[2, 2] <- sum(str_count(ethnicity_all, "Hispanic or Latino/Latina"))
  
  ethnicity_all_df[3, 2] <- sum(str_count(ethnicity_all, "I don't know"))
  
  ethnicity_all_df[4, 2] <- sum(str_count(ethnicity_all, "I prefer not to say"))
  
  ethnicity_all_df[5, 2] <- sum(str_count(ethnicity_all, "Middle Eastern"))
  
  ethnicity_all_df[6, 2] <- sum(str_count(ethnicity_all, "Native American, Pacific Islander, or Indigenous Australian"))
  
  ethnicity_all_df[7, 2] <- sum(str_count(ethnicity_all, "South Asian"))
  
  ethnicity_all_df[8, 2] <- sum(str_count(ethnicity_all, "White or of European descent"))
  
  ethnicity_all_df[9, 2] <- sum(str_count(ethnicity_all, "Black or of African descent"))
  
  ethnicity_all_df$Percentage <- ethnicity_all_df$Count / sum(ethnicity_all_df$Count) * 100
  
  output$ethnicity_all_plot<- renderPlot({
    ggplot(ethnicity_all_df, aes(x= Race, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Race, 
                       xend=Race, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Race", 
           subtitle="All Respondents", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  
  #Parents - All Respondents
  parents_all <- subset(surveyResultsPublic, select = c("HighestEducationParents"))
  
  parents_all <- na.omit(parents_all)
  
  parents_all$HighestEducationParents <- as.character(parents_all$HighestEducationParents)
  
  #create empty dataframe that for race and count of race
  parents_all_df <- data.frame(matrix(ncol = 2, nrow = 10))
  
  parents_all_df_x <- c("Education", "Count")
  
  colnames(parents_all_df) <- parents_all_df_x
  
  all_Parents <- c("High school", "A master's degree", "A professional degree", "A doctoral degree", "A bachelor's degree", "Some college/university study, no bachelor's degree", "I prefer not to answer", "Primary/elementary school", "I don't know/not sure", "No education")
  
  parents_all_df$Education <- all_Parents
  
  parents_all_df
  
  #count the frequency of each ethnicity
  parents_all_df[1, 2] <- sum(str_count(parents_all, "High school"))
  
  parents_all_df[2, 2] <- sum(str_count(parents_all, "A master's degree"))
  
  parents_all_df[3, 2] <- sum(str_count(parents_all, "A professional degree"))
  
  parents_all_df[4, 2] <- sum(str_count(parents_all, "A doctoral degree"))
  
  parents_all_df[5, 2] <- sum(str_count(parents_all, "A bachelor's degree"))
  
  parents_all_df[6, 2] <- sum(str_count(parents_all, "Some college/university study, no bachelor's degree"))
  
  parents_all_df[7, 2] <- sum(str_count(parents_all, "I prefer not to answer"))
  
  parents_all_df[8, 2] <- sum(str_count(parents_all, "Primary/elementary school"))
  
  parents_all_df[9, 2] <- sum(str_count(parents_all, "I don't know/not sure"))
  
  parents_all_df[10, 2] <- sum(str_count(parents_all, "No education"))
  
  parents_all_df$Percentage <- parents_all_df$Count / sum(parents_all_df$Count) * 100
  
  output$parents_all_plot<- renderPlot({
    ggplot(parents_all_df, aes(x= Education, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Education, 
                       xend=Education, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Parents Education", 
           subtitle="All Respondents", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  #Parents - Professional
  parent_professionals <- subset(surveyResultsPublic, Professional == "Professional developer")
  
  parent_professional <- subset.data.frame(parent_professionals, select = c("HighestEducationParents"))
  
  parent_professional <- na.omit(parent_professional)
  
  parent_professional$HighestEducationParents <- as.character(parent_professional$HighestEducationParents)
  
  #create empty dataframe that for race and count of race
  parent_professional_df <- data.frame(matrix(ncol = 2, nrow = 10))
  
  parent_professional_df_x <- c("Education", "Count")
  
  colnames(parent_professional_df) <- parent_professional_df_x
  
  professional_Parents <- c("High school", "A master's degree", "A professional degree", "A doctoral degree", "A bachelor's degree", "Some college/university study, no bachelor's degree", "I prefer not to answer", "Primary/elementary school", "I don't know/not sure", "No education")
  
  parent_professional_df$Education <- professional_Parents
  
  parent_professional_df
  
  #count the frequency of each ethnicity
  parent_professional_df[1, 2] <- sum(str_count(parent_professional, "High school"))
  
  parent_professional_df[2, 2] <- sum(str_count(parent_professional, "A master's degree"))
  
  parent_professional_df[3, 2] <- sum(str_count(parent_professional, "A professional degree"))
  
  parent_professional_df[4, 2] <- sum(str_count(parent_professional, "A doctoral degree"))
  
  parent_professional_df[5, 2] <- sum(str_count(parent_professional, "A bachelor's degree"))
  
  parent_professional_df[6, 2] <- sum(str_count(parent_professional, "Some college/university study, no bachelor's degree"))
  
  parent_professional_df[7, 2] <- sum(str_count(parent_professional, "I prefer not to answer"))
  
  parent_professional_df[8, 2] <- sum(str_count(parent_professional, "Primary/elementary school"))
  
  parent_professional_df[9, 2] <- sum(str_count(parent_professional, "I don't know/not sure"))
  
  parent_professional_df[10, 2] <- sum(str_count(parent_professional, "No education"))
  
  parent_professional_df$Percentage <- parent_professional_df$Count / sum(parent_professional_df$Count) * 100
  
  output$parents_professional_plot<- renderPlot({
    ggplot(parent_professional_df, aes(x= Education, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Education, 
                       xend=Education, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Parents Education", 
           subtitle="Professional Developer", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  #Parents - Students
  parent_students <- subset(surveyResultsPublic, Professional == "Student")
  
  parent_student <- subset.data.frame(parent_students, select = c("HighestEducationParents"))
  
  parent_student <- na.omit(parent_student)
  
  parent_student$HighestEducationParents <- as.character(parent_student$HighestEducationParents)
  
  #create empty dataframe that for race and count of race
  parent_student_df <- data.frame(matrix(ncol = 2, nrow = 10))
  
  parent_student_df_x <- c("Education", "Count")
  
  colnames(parent_student_df) <- parent_student_df_x
  
  student_Parents <- c("High school", "A master's degree", "A professional degree", "A doctoral degree", "A bachelor's degree", "Some college/university study, no bachelor's degree", "I prefer not to answer", "Primary/elementary school", "I don't know/not sure", "No education")
  
  parent_student_df$Education <- student_Parents
  
  parent_student_df
  
  #count the frequency of each ethnicity
  parent_student_df[1, 2] <- sum(str_count(parent_student, "High school"))
  
  parent_student_df[2, 2] <- sum(str_count(parent_student, "A master's degree"))
  
  parent_student_df[3, 2] <- sum(str_count(parent_student, "A professional degree"))
  
  parent_student_df[4, 2] <- sum(str_count(parent_student, "A doctoral degree"))
  
  parent_student_df[5, 2] <- sum(str_count(parent_student, "A bachelor's degree"))
  
  parent_student_df[6, 2] <- sum(str_count(parent_student, "Some college/university study, no bachelor's degree"))
  
  parent_student_df[7, 2] <- sum(str_count(parent_student, "I prefer not to answer"))
  
  parent_student_df[8, 2] <- sum(str_count(parent_student, "Primary/elementary school"))
  
  parent_student_df[9, 2] <- sum(str_count(parent_student, "I don't know/not sure"))
  
  parent_student_df[10, 2] <- sum(str_count(parent_student, "No education"))
  
  parent_student_df$Percentage <- parent_student_df$Count / sum(parent_student_df$Count) * 100
  
  output$parents_student_plot<- renderPlot({
    ggplot(parent_student_df, aes(x= Education, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Education, 
                       xend=Education, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Parents Education", 
           subtitle="Students", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  
  #Education Attainment - All Repsondents
  
  formalEducation <- subset.data.frame(surveyResultsPublic, select = c("FormalEducation"))
  FormalEducationNew<- formalEducation %>% group_by(FormalEducation) %>%  tally() 
  
  #Education All respondants plot
  output$education_all_plot<- renderPlot({
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
  })
  
  
  #Education Attainment - Professional
  
  educationSubset <- subset(surveyResultsPublic, Professional == "Professional developer")
  EducationProfessional <- subset.data.frame(educationSubset, select = c("FormalEducation"))
  EducationProfessionalNew<- EducationProfessional %>% group_by(FormalEducation) %>%  tally() 
  
  #Education professional respondants plot
  output$education_professional_plot<- renderPlot({
    ggplot(EducationProfessionalNew, aes(x=as.factor(FormalEducation), y=n))+ 
      geom_point(aes(y = (n)/sum(n)),size=3,color="orange")+
      scale_y_continuous(labels=scales::percent) + 
      geom_segment(aes(x=FormalEducation, 
                       xend=FormalEducation,
                       y=0, 
                       yend=(n)/sum(n)), color = "Orange")+
      theme(axis.text.x = element_text(angle=45, vjust=0.6))+
      labs(title="Educational Attainment", 
           subtitle="Professional Developers",
           caption="source: stackoverflow")+
      theme_light()+ xlab("Educational Attainment") + ylab("Percentage")+
      coord_flip()+scale_x_discrete(breaks=c("Some college/university study without earning a bachelor's degree","Secondary school","Professional degree", "Primary/elementary school", "Master's degree", "I prefer not to answer", "I never completed any formal education", "Doctoral degree", "Bachelor's degree"),
                                    labels=c("college without earning degree","Secondary School","Professional Degree", "Primary/Elementary School", "Masters Degree", "Prefer not to answer", "No formal education", "Doctoral Degree", "Bachelors Degree"))
    
  })
  
  
  #Specific Developer Type - Mobile Developers
  WebDevTypes <- subset.data.frame(surveyResultsPublic, select = c("WebDeveloperType"))
  
  WebDevType <- na.omit(WebDevTypes)
  
  WebDevType$WebDeveloperType <- as.character(WebDevType$WebDeveloperType)
  
  WebDevType_df <- data.frame(matrix(ncol = 2, nrow = 3))
  
  WebDevType_df_x <- c("Web_Developer_Type", "Count")
  
  colnames(WebDevType_df) <- WebDevType_df_x
  
  web_developers <- c("Full stack Web developer", "Back-end Web developer", "Front-end Web developer")
  
  WebDevType_df$Web_Developer_Type <- web_developers
  
  WebDevType_df
  
  WebDevType_df[1, 2] <- sum(str_count(WebDevType, "Full stack Web developer"))
  
  WebDevType_df[2, 2] <- sum(str_count(WebDevType, "Back-end Web developer"))
  
  WebDevType_df[3, 2] <- sum(str_count(WebDevType, "Front-end Web developer"))
  
  WebDevType_df$Percentage <- WebDevType_df$Count / sum(WebDevType_df$Count) * 100
  
  output$web_developer_plot <- renderPlot({
    ggplot(WebDevType_df, aes(x= Web_Developer_Type, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Web_Developer_Type, 
                       xend=Web_Developer_Type, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Specific Developer Type", 
           subtitle="Web Developer Types", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  #Specific Developer Type - Mobile Developers
  MobileDevTypes <- subset.data.frame(surveyResultsPublic, select = c("MobileDeveloperType"))
  
  MobileDevType <- na.omit(MobileDevTypes)
  
  MobileDevType$MobileDeveloperType <- as.character(MobileDevType$MobileDeveloperType)
  
  MobileDevType_df <- data.frame(matrix(ncol = 2, nrow = 4))
  
  MobileDevType_df_x <- c("Mobile_Developer_Type", "Count")
  
  colnames(MobileDevType_df) <- MobileDevType_df_x
  
  mobile_developers <- c("Windows Phone", "iOS", "Android", "Blackberry")
  
  MobileDevType_df$Mobile_Developer_Type <- mobile_developers
  
  MobileDevType_df
  
  MobileDevType_df[1, 2] <- sum(str_count(MobileDevType, "Windows Phone"))
  
  MobileDevType_df[2, 2] <- sum(str_count(MobileDevType, "iOS"))
  
  MobileDevType_df[3, 2] <- sum(str_count(MobileDevType, "Android"))
  
  MobileDevType_df[4, 2] <- sum(str_count(MobileDevType, "Blackberry"))
  
  MobileDevType_df$Percentage <- MobileDevType_df$Count / sum(MobileDevType_df$Count) * 100
  
  output$mobile_developer_plot <- renderPlot({
    ggplot(MobileDevType_df, aes(x= Mobile_Developer_Type, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Mobile_Developer_Type, 
                       xend=Mobile_Developer_Type, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Specific Developer Type", 
           subtitle="Mobile Developer Types", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  
  otherOccupations <- subset.data.frame(surveyResultsPublic, select = c("NonDeveloperType"))
  
  otherOccupation <- na.omit(otherOccupations)
  
  otherOccupation$NonDeveloperType <- as.character(otherOccupation$NonDeveloperType)
  
  otherOccupation_df <- data.frame(matrix(ncol = 2, nrow = 9))
  
  otherOccupation_df_x <- c("Other_Occupations", "Count")
  
  colnames(otherOccupation_df) <- otherOccupation_df_x
  
  other_Occupations <- c("Analyst or consultant", "Other", "Data scientist", "Educator or academic", "Designer or illustrator", "Product manager", "C-suite executive", "Marketing or sales manager", "Elected official")
  
  otherOccupation_df$Other_Occupations <- other_Occupations
  
  otherOccupation_df
  
  otherOccupation_df[1, 2] <- sum(str_count(otherOccupation, "Analyst or consultant"))
  
  otherOccupation_df[2, 2] <- sum(str_count(otherOccupation, "Other"))
  
  otherOccupation_df[3, 2] <- sum(str_count(otherOccupation, "Data scientist"))
  
  otherOccupation_df[4, 2] <- sum(str_count(otherOccupation, "Educator or academic"))
  
  otherOccupation_df[5, 2] <- sum(str_count(otherOccupation, "Designer or illustrator"))
  
  otherOccupation_df[6, 2] <- sum(str_count(otherOccupation, "Product manager"))
  
  otherOccupation_df[7, 2] <- sum(str_count(otherOccupation, "C-suite executive"))
  
  otherOccupation_df[8, 2] <- sum(str_count(otherOccupation, "Marketing or sales manager"))
  
  otherOccupation_df[9, 2] <- sum(str_count(otherOccupation, "Elected official"))
  
  otherOccupation_df$Percentage <- otherOccupation_df$Count / sum(otherOccupation_df$Count) * 100
  
  output$other_occupation_plot <- renderPlot({
    ggplot(otherOccupation_df, aes(x= Other_Occupations, y= Percentage))+ 
      geom_point(size=3, color = "Orange")+ 
      geom_segment(aes(x=Other_Occupations, 
                       xend=Other_Occupations, 
                       y=0, 
                       yend=Percentage), color = "Orange")+ 
      labs(title="Specific Developer Type", 
           subtitle="Other Occupation Types", 
           caption="source: stackoverflow")+ 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      theme_light()+
      coord_flip()
  })
  
  ###WORK
  
  #Job Satisfaction
  
  jobSatisfaction <- subset.data.frame(surveyResultsPublic, select = c("JobSatisfaction"))
  jobSatisfaction<- na.omit(jobSatisfaction)
  
  jobSatisfactionNew<- jobSatisfaction %>% group_by(JobSatisfaction) %>%  tally() 
  
  #Job Satisfaction plot
  
  output$job_satisfaction_plot<- renderPlot({
    ggplot(jobSatisfactionNew, aes(x=as.factor(JobSatisfaction), y = (n)/sum(n)))+
      geom_bar(stat="identity", fill="orange", width=0.4)+coord_flip()+scale_y_continuous(labels=scales::percent)+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      labs(title="How do Developers Feel about their Jobs and Careers?", 
           subtitle="Job Satisfaction (0- Not at all satisfied/ 10- completely satisfied)",
           caption="source: stackoverflow")+
      theme_light()+ xlab("Job Satisfaction") + ylab("Percentage")
  })
  
  
  
  # career Satisfaction
  careerSatisfaction <- subset.data.frame(surveyResultsPublic, select = c("CareerSatisfaction"))
  
  careerSatisfaction<- na.omit(careerSatisfaction)
  
  careerSatisfactionNew<- careerSatisfaction %>% group_by(CareerSatisfaction) %>%  tally() 
  
  #career Satisfaction plot
  output$career_satisfaction_plot<- renderPlot({
    ggplot(careerSatisfactionNew, aes(x=as.factor(CareerSatisfaction), y = (n)/sum(n)))+
      geom_bar(stat="identity", fill="orange", width=0.4)+coord_flip()+scale_y_continuous(labels=scales::percent)+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      labs(title="How do Developers Feel about their Jobs and Careers?", 
           subtitle="Career Satisfaction (0- Not at all satisfied/ 10- completely satisfied)",
           caption="source: stackoverflow")+
      theme_light()+ xlab("Career Satisfaction") + ylab("Percentage")
  })
  
  #   output$plot<-renderPlot({plot})
  
}
  
  

shinyApp(ui = ui, server = server)
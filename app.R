library(shiny)
library(ggmap)
library(maps)

library (readr)
library(tidyr)
library(plyr)
library(plotly)

ui <- fluidPage(
  theme = "bootswatch-cerulean.css",
  includeCSS("C:/Users/tonyn/Downloads/style.css"),
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
        plotOutput("student_occurences_plot"),
        plotOutput("prof_occurences_plot")
        
      ),
      fluidRow(h4(id="developer_profile","Developer Profile")),
      fluidRow(h4(id="Geography","Geography")),
     
      tabsetPanel(
        tabPanel("Survey Respondents", plotlyOutput("respond_plot")), 
        tabPanel("Professional Developers", plotlyOutput("prof_devs"))),
      
      tags$div(
        tags$p("Each month, about 50 million people visit Stack Overflow to learn, share, and build their careers. We estimate that 21 million of these people are professional developers and university-level students."),
        tags$p("Our estimate of professional developers comes from the things people read and do when they visit Stack Overflow. We collect data on user activity to help surface jobs we think you might find interesting and questions we think you can answer. You can download and clear this data at any time.")),
      
      
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
  surveyResultsPublic <- read_csv("C:/Users/tonyn/Downloads/survey_results_public.csv")
  
  #Geography - map
  country_codes<- read_csv("C:/Users/tonyn/Downloads/countries.csv")
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
  #Paul code
  
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
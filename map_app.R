library(shiny)
library(ggmap)
library(maps)

library (readr)
library(tidyr)
library(plyr)
library(plotly)


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
ggplotly(respond)


#Paul code




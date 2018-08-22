library(ggmap)
library(maps)

library (readr)
library(tidyr)
library(plyr)
library(plotly)


results <- read_csv("C:/Users/amonk/Downloads/dataset.csv")
#importing country codes Paul uploaded to github
country_codes<- read_csv("C:/Users/amonk/Downloads/countries.csv")
#cleaning up data
View(country_codes)
country_codes <- country_codes[c(-1)]
country_codes <- country_codes[c(3,1,2)]
colnames(country_codes) <- c("Country", "long", "lat") 
#update

results_pruned <- results[c(1:7)]
View(results_pruned)

#There are three different maps on the  stackoverflow website; 
#1. survey respondents - subset on country and sum of respondants
#2. monthly stack overflow visits -
#3. professional developers - subset on developer column for professional developers and sum by country

#1. survey respondents
respondents <- results_pruned[c("Respondent", "Country")]

#below I use the count function from plyr to perform count of country occurence
respondents_count <- count(respondents, "Country")

#merge respondents_count with the clean county code data using the plyr package. very clean
respondents_merged <- merge(respondents_count, country_codes, by = "Country")
respondents_merged <- na.omit(respondents_merged)
#respondents_merged <- subset(respondents_merged, freq > 5)

# Libraries
#library(tidyverse)

#percentages
respondents_merged$percentage <- (respondents_merged$freq/sum(respondents_merged$freq))*100
#2 decimal places

respondents_merged$percentage2 <- format(round(respondents_merged$percentage, 2), nsmall = 2)


# Get the world polygon and extract U

world <- map_data("world") # we already did this, but we can do it again
gg1<-ggplotly(ggplot(respondents_merged, aes()) + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey", color="lightgrey")+xlab("")+ylab("") + 
  coord_fixed(1.3) + 
  geom_point(data = respondents_merged, aes(x = lat, y = long), color = "blue", size = respondents_merged$freq/800, alpha = 0.4)+xlab("")+ylab(""))

gg1


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

# Get the world polygon 

#world <- map_data("world") # we already did this, but we can do it again
gg3<-ggplot(pro_merged, aes()) + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey", color="lightgrey")+xlab("")+ylab("") + 
  coord_fixed(1.3)



p3 <- gg3 + 
  geom_point(data = pro_merged, aes(x = lat, y = long, text = paste(Country,":", percentage2, "percentage of survey respondents")), color = "blue", size = pro_merged$freq/800, alpha = 0.4)+xlab("")+ylab("")

n3 <- ggplotly(p3)
n3
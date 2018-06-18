install.packages("ggmap")
install.packages("readr")
install.packages("tidyr")
install.packages("plotly")


library(ggmap)
library(maptools)
library(maps)

library (readr)
library(tidyr)
library(plyr)


results <- read_csv("C:/Users/tonyn/Downloads/survey_results_public.csv")
View(results)
#importing country codes Paul uploaded to github
country_codes<- read_csv("C:/Users/tonyn/Downloads/countries.csv")
#cleaning up data
country_codes <- country_codes[c(-1)]
country_codes <- country_codes[c(3,1,2)]
colnames(country_codes) <- c("Country", "long", "lat") 
#update

#View(n)
results_pruned <- results[c(1:7)]
View(results_pruned)
head(results_pruned)
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
View(respondents_merged)
#respondents_merged <- subset(respondents_merged, freq > 5)

# Libraries
library(tidyverse)

# Get the world polygon and extract U

world <- map_data("world") # we already did this, but we can do it again
gg1<-ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

gg1 + 
  geom_point(data = respondents_merged, aes(x = lat, y = long), color = "blue", size = respondents_merged$freq/1000)


#2. monthly stack overflow visits -



#3. professional developers - subset on developer column for professional developers and sum by country

professional_dev <- subset(results_pruned, Professional == "Professional developer")
#below I use the count function from plyr to perform count of country occurence

pro_count <- count(respondents, "Country")

#merge pro with the clean county code data using the plyr package. very clean
pro_merged <- merge(pro_count, country_codes, by = "Country")
pro_merged <- na.omit(pro_merged)
View(pro_merged)



# Get the world polygon 

world <- map_data("world") # we already did this, but we can do it again
gg1<-ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

gg1 + 
  geom_point(data = pro_merged, aes(x = lat, y = long), color = "blue", size = pro_merged$freq/1000)











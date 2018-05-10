install.packages("ggmap")
install.packages("readr")
install.packages("tidyr")

library(ggmap)
library(maptools)
library(maps)

library (readr)
library(tidyr)

install.packages("rworldmap")
library(rworldmap)


results <- read_csv("C:/Users/tonyn/Downloads/survey_results_public.csv")
#update
View(results)

sub <- data.frame(results$Country, results$FormalEducation)
names(sub) <- c("Country", "Foraml Education")
View(results)

#country <- c("germany", "germany", "france", "ireland", "spain")
countryr <- as.data.frame(table(results$Country))

co <- geocode(location = as.character(countryr$Var1))

bound <- cbind(countryr, country.x, country.y)
country.x <- co$lon
country.y <- co$lat

colnames(bound) <- c("country", "count","long","lat")
View(bound)

r <- prop.table(bound$count)

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

mp





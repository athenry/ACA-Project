## Title: ACA Visualizations
## Authors: Alison Henry, Christina Hwang, Katy Moore
## Last updated: 2018/10/04
## contact: ahenry@ualberta.ca

## Install and load needed packages
install.packages(c("devtools", "tidyverse", "bibliometrix", "maps", "mapdata", "mapproj","ggthemes"))
install_github("dgrtwo/gganimate")
library(tidyverse)
library(bibliometrix)
library(maps)
library(mapdata)
library(ggthemes)
library(gganimate)

## Read in prepared indices
authorList <- read.csv("Data/ACA Author Index 1997-2017 - ACA author index.csv", header=TRUE)
grantList <- read.csv("Data/ACA Grants Index 1997-2017 - ACA Grants Index.csv", header = TRUE)
pubList <- read.csv("Data/ACA Publication Index 1997-2017 - Publication index.csv")

## Publication numbers
## list desired graphics here, with corresponding figure numbers 
##

## citation_by_year <- pubList %>% 
##     group_by(Year) %>% 
##     summarise(cites = sum(as.integer(Citation.count))) 

## fig1 <- ggplot(citation_by_year, aes(x=Year, y=cites)) + geom_col() + theme_classic() + labs(title="Citations by Year of Publication")


## Gather data for grant mapping
## Create data frame of locations and assigned longitude and lattitude

locations <- as_tibble(cbind(c("Banff National Park", "CFB Suffield", "Elk Island National Park", "Foothills", "Jasper National Park", "Mountain", "Northern Boreal", "Parkland", "Prairie", "Waterton Lakes National Park", "Wood Buffalo National Park"), c("-115.92797", "-111.175004", "-112.857157", "-116.817049", "-117.954272", "-116.476799", "-116.569466", "-111.575478", "-112.336827", "-113.916624", "-112.876509"), c("51.496746", "50.277093", "53.608301", "54.187166", "52.873331", "52.37858", "57.015187", "52.990867", "50.583973", "49.083404", "59.439519")))
colnames(locations) <- c("location", "lon", "lat")

## Add longitude and latitude to each grant
## grantListGeo <- grantList %>% 
##    left_join(locations, b=c("Coded.Location" = "location")) 

grantsperLoc <- count(grantList, Coded.Location, Year.Awarded) %>%
    left_join(locations, b=c("Coded.Location" = "location"))

grantsperTopic <- count(grantList, Detailed.Thematic.Category, Year.Awarded) %>%
    left_join(locations, b=c("Coded.Location" = "location"))

grantsperPriorityArea <- count(grantList, ACARG.Priority.Areas.1, Year.Awarded) %>%
    left_join(locations, b=c("Coded.Location" = "location"))

## Create our base map
canada <- ggplot() + borders(database = "worldHires", "Canada", colour = "gray80", fill = "gray85") + theme_map()

map1 <- canada +
    geom_point(aes(x=lon, y=lat), 
               data = grantsperLoc,
               colour = 'green', alpha = .5)
    
    
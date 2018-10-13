## Title: ACA Visualizations
## Authors: Alison Henry, Christina Hwang, Katy Moore
## Last updated: 2018/10/04
## contact: ahenry@ualberta.ca

## Install and load needed packages
install.packages(c("devtools", "tidyverse", "bibliometrix", "maps", "mapdata", "mapproj","ggthemes"))
library(devtools)
install_github("dgrtwo/gganimate")
library(tidyverse)
library(RColorBrewer)
library(bibliometrix)
library(maps)
library(mapdata)
library(ggthemes)
library(gganimate)

## Read in prepared indices
authorList <- read.csv("Data/ACA Author Index 1997-2017 - ACA author index.csv", header=TRUE)
grantList <- read.csv("Data/ACA Grants Index 1997-2017 - ACA Grants Index.csv", header = TRUE)
names(grantList) <- str_remove_all(names(grantList), "[.]")
pubList <- read.csv("Data/ACA Publication Index 1997-2017 - Publication index.csv")

## Publication numbers
## (list desired graphics here, with corresponding figure numbers) 
## Fig 1: Citations by year
## Fig 2: Publication by year with citations
## Fig 3: Tweets and Retweets by Article, coloured by Topic
## Facebook interactions by publication
## Facebook interactions by topic
## News mentions by publication
## News mentions by topic
## Degree pursued by year
## degree pursued by topic?

citation_by_year <- pubList %>% 
    group_by(Year) %>% 
    summarise(cites = sum(as.integer(Citation.count))) 

fig1 <- ggplot(citation_by_year, aes(x=Year, y=cites)) + geom_col() + theme_classic() + labs(title="Citations by Year of Publication", x="Publication Year", y="Number of Citations")

publication_by_year <- pubList %>%
    group_by(Year) %>%
    summarise(articles = n())

## Note: number of citations is much higher than the number of articles. Add secondary axis so as not to lose relevance of the publication record. 

fig2 <- ggplot(publication_by_year, aes(x=Year, y=articles)) + geom_col() + theme_classic() + labs(title = "Number of Publications by Year", x="Publication Year", y="Articles") + geom_line(data = citation_by_year, aes(x=Year, y=cites/25, colour = "red"), show.legend = FALSE) + scale_y_continuous(sec.axis = sec_axis(~ . *20, breaks = waiver(), labels = waiver(), name = "Citations"))

levels(pubList$Category)[1] <- "Not assigned"

categoryPalette <- colorRampPalette(brewer.pal(7,"Dark2"))(14)

fig3 <- ggplot(pubList, aes(x=Year, y=Twitter)) + geom_point(aes(col=Category, size=Twitter)) + theme_classic() + labs(title="Tweets and ReTweets of ACA-funded Publications", y="Number of Tweets and ReTweets", x="Year of Publication") + scale_colour_manual(values = categoryPalette)


## Grant overview: mapping of location studied, types of grants (biodiversity vs research), by degree?, locations of researchers?


## Gather data for grant mapping
## Create data frame of locations and assigned longitude and lattitude

locations <- as_tibble(cbind(c("Banff National Park", "CFB Suffield", "Elk Island National Park", "Foothills", "Jasper National Park", "Mountain", "Northern Boreal", "Parkland", "Prairie", "Waterton Lakes National Park", "Wood Buffalo National Park"), c("-115.92797", "-111.175004", "-112.857157", "-116.817049", "-117.954272", "-116.476799", "-116.569466", "-111.575478", "-112.336827", "-113.916624", "-112.876509"), c("51.496746", "50.277093", "53.608301", "54.187166", "52.873331", "52.37858", "57.015187", "52.990867", "50.583973", "49.083404", "59.439519")))
colnames(locations) <- c("location", "lon", "lat")

## Add longitude and latitude to each grant
## grantListGeo <- grantList %>% 
##    left_join(locations, b=c("Coded.Location" = "location")) 

grantsperLoc <- grantList %>%
    group_by(CodedLocation, YearAwarded) %>%
    left_join(locations, b=c("CodedLocation" = "location"))

grantsperTopic <- count(grantList, Category, YearAwarded) %>%
    left_join(locations, b=c("CodedLocation" = "location"))

grantsperPriorityArea <- count(grantList, ACARGPriorityAreas1, YearAwarded) %>%
    left_join(locations, b=c("CodedLocation" = "location"))

## Create our base map
canada <- ggplot() + borders(database = "worldHires", "Canada", colour = "gray80", fill = "gray85") + theme_map()

map1 <- canada +
    geom_point(aes(x=lon, y=lat), 
               data = grantsperLoc,
               colour = 'green', alpha = .5)
    

## Bibliometric visualizations: most prolific producers, top journals, any network analysis?   
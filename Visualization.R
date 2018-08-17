## Title: ACA Visualizations
## Authors: Alison Henry, Christina Hwang, Katy Moore
## Last updated: 2018/08/17
## contact: ahenry@ualberta.ca

## Install and load needed packages
install.packages("tidyverse")
library(tidyverse)

## Read in prepared indices
authorList <- read.csv("Data/ACA Author Index 1997-2017 - ACA author index.csv", header=TRUE)
grantList <- read.csv("Data/ACA Grants Index 1997-2017 - ACA Grants Index.csv", header = TRUE)
pubList <- read.csv("Data/ACA Publication Index 1997-2017 - Publication index.csv")

## Publication numbers
## list desired graphics here, with corresponding figure numbers 
##

citation_by_year <- pubList %>% 
    group_by(Year) %>% 
    summarise(cites = sum(as.integer(Citation.count))) 

fig1 <- ggplot(citation_by_year, aes(x=Year, y=cites)) + geom_col()

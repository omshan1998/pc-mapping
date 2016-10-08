##load libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(raster)
library(ggmap)
library(RColorBrewer)
Sys.setlocale('LC_ALL','C')
source("./newFilter.R")

#############################
countries <- readOGR('./world-shapefile', layer = 'world3')
articles <- read.csv('./data/Data Scraping for Journal Articles Final 041916.csv')

#participatory <- read.csv('./data/ParticipatoryData.csv')

#removing South Sudan from my dataset
#participatory <- subset(participatory, COUNTRY!="South Sudan")

countries@data$polyorder <- 1 : dim(countries@data)[1]

#tmp <- merge(countries@data, participatory, by = "ISO3", sort = TRUE, all.x = TRUE)
#tmp <- tmp[ order(tmp$polyorder), ]
#countries@data <- tmp

#make map with color scale based on 'WORK'

#countryColor <- colorFactor(topo.colors(10), countries@data$WORK)


#Generating unique list of countries
countryList <- unique(countries@data$NAME) %>% as.character() %>% sort()
countryList2 <- unique(countries@data$ISO2.x) %>% as.character() %>% sort()

#Genreating the author list
tempAuthor <- articles$Authors %>% as.character()
AuthorList <- unique(unlist(strsplit(tempAuthor,", "))) %>% sort()

UniversityList <- unique(articles$Place.of.Publish..1st.author.) %>% as.character() %>% sort()

#Generating map Type List
MapTypeList <- c("WORK", "FIRSTPUB", "RESTPUB", "ALLPUB")

#A mapping
#nameList <- countries@data$NAME
#isoList <- countries@data$ISO2.x
#names(isoList) <- nameList

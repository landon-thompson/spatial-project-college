library(maptools)
library(sp)
library(rgeos)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

setwd('C:\\Users\\Landon\\Google Drive\\Spatial Final Project\\FinalDataSets')

dat <- readShapePoly('CountyDemographics.shp')


proj4string(dat) <- CRS('+proj=utm +zone=15')

dat <- spTransform(dat, CRS('+proj=longlat +datum=WGS84'))

## First Plot
pal <- colorBin(c('yellow','blue'), domain = dat$MeanIncome)
labels <- paste(dat$CountyOrig,': ', dat$MeanIncome, ' Mean Income', sep = '')

map <- leaflet() %>%
  addProviderTiles(providers$OpenInfraMap)

incomeMap <- map  %>%
  addPolygons(
    data = dat,  
    weight = 1,  
    opacity = 1,  # line transparency
    color = "black",  # line colour
    fillColor = ~pal(MeanIncome) , # LAD name as a hover label
    fillOpacity = 0.75,
    highlight = highlightOptions(
      weight=5,
      color = '#666',
      fillOpacity = 0,
      bringToFront = TRUE),
    label =labels
  )

## Click the "zoom" button and you can zoom in and out of the map below. 
incomeMap

saveWidget(incomeMap, file="IncomeMap.html")


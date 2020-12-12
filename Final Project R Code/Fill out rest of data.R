library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(sp)

county.data <- readShapePoly("CountyDemographics.shp")
county.data@data$id <- rownames(county.data@data)
county.points <- fortify(county.data,region="id")
county.df <- join(county.points, county.data@data, by="id")

#spplot(county.data)
spplot(county.data,"White")
#county.data@data$perc.white <- county.data@data$White / county.data@data$TotalPopul
#county.data@data$perc.black <- county.data@data$Black / county.data@data$TotalPopul
spplot(county.data,"perc.white")
#county.data@data$pop.per.house <- county.data@data$TotalPopul/county.data@data$TotalEstim

county.data@data$perc.indian <- county.data@data$Indian / county.data@data$TotalPopul
county.data@data$perc.pacif <- county.data@data$PacificIsl / county.data@data$TotalPopul
county.data@data$perc.other <- county.data@data$Other / county.data@data$TotalPopul

writePolyShape(county.data, "CountyDemographics.shp")


county.data <- readShapePoly("CountyDemographics.shp")

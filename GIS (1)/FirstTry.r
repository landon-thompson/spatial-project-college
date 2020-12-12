library(maptools)
library(dplyr)

area <- readShapePoly("C:\\Users\\Landon\\Google Drive\\MinneMUDAC\\GIS\\Political Boundary GIS\\GIS\\C2012.shp")


spplot(area, "NH_WHT", main = "NH_WHT")
spplot(area, "NH_DOJ_BLK", main="NH_DOJ_BLK")
spplot(area, "NH_DOJ_IND", main="NH_DOJ_IND")
spplot(area, "NH_DOJ_ASN", main="NH_DOJ_ASN")
spplot(area, "NH_DOJ_HWN", main= "NH_DOJ_HWN")
spplot(area, "NH_DOJ_OTH", main="NH_DOJ_OTH")
spplot(area, "NH_DOJ_OT1", main="NH_DOJ_OT1")



elecResults <- readShapePoly("C:\\Users\\Landon\\Google Drive\\MinneMUDAC\\GIS\\ElectionResults\\elec2016.shp")
summary(elecResults@data$MNSENLIB)

spplot(elecResults,"MNSENDFL", main="Democratic Votes for Senator")
spplot(elecResults,"MNSENR", main="Republican Votes for Senator")
spplot(elecResults,"MNSENIP", main="Republican Votes for Senator")

spplot(elecResults,"MNLEGDFL", main="Democratic Votes for House Seats")
spplot(elecResults,"MNLEGR", main="Republican Votes for House Seats")

new.dat <- elecResults@data %>% group_by(CONGDIST) %>% summarise(SenRepVot=sum(MNSENR),SenDemVot=sum(MNSENDFL), SenIndVot = sum(MNSENIP)
                                                                  , HouseRepVot = sum(MNLEGR), HouseDemVot = sum(MNLEGDFL))

## Senate 
area@data$SenRepVot <- new.dat$SenRepVot
area@data$SenDemVot <- new.dat$SenDemVot
area@data$SenIndVot <- new.dat$SenIndVot

## House
area@data$HouseRepVot <- new.dat$HouseRepVot
area@data$HouseDemVot <- new.dat$HouseDemVot

spplot(area, "SenRepVot", main="Senator Republican Vote")
spplot(area, "SenDemVot", main="Senator Democrat Vote")
spplot(area, "SenIndVot", main="Senator Independent Vote")

spplot(area, "HouseRepVot", main="House Republican Vote")
spplot(area, "HouseDemVot", main="House Democrat Vote")



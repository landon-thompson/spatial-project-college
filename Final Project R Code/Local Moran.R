library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(sp)
library(spatstat)
library(spdep)
library(gridExtra)

#setwd('C:\\Users\\Landon\\Google Drive\\Spatial Final Project\\Data')

county.data <- readShapePoly("CountyDemographics.shp")


#Moran's Correlograms
nlist.rook <- poly2nb(county.data, row.names=as.character(county.data@data$SP_ID), queen=F)
Mor.r <- sp.correlogram(nlist.rook, county.data@data$perc_white,order=5,method="I",style="W",randomisation = TRUE)
plot(Mor.r, main="Correlogram Percent White - Rook's Case")


nlist.queen <- poly2nb(county.data, row.names=as.character(county.data@data$SP_ID), queen=T)
Mor.q <- sp.correlogram(nlist.queen, county.data@data$perc_white+county.data@data$perc_india,order=5,method="I",style="W",randomisation = TRUE)
plot(Mor.q, main="Correlogram Percent White - Queen's Case")


Rep.q <- sp.correlogram(nlist.queen,Rep@data$percRep,order=5,method="I",style="W",randomisation = T)
Rep.cor <- plot(Rep.q)


#Style W is Row Normalized Resampling Assumptions 
W <- nb2listw(nlist.queen,style="W")



#Autocorrelation plots Based on Race, Not as useful as we thought
#percwhite <- county.data@data$perc_white
#percblack <- county.data@data$perc_black
#percwhiteImat <- localmoran(percwhite,W)
#percwhiteI <- localmoran(percwhite,W)[,1]
#percwhiteI_pval <- localmoran(percwhite,W)[,5]

#county.data@data$percwhiteI <- percwhiteI
#county.data@data$percwhiteI_pval <- percwhiteI_pval
#county.data@data$percwhiteI_pvalFlag <- ifelse(county.data@data$percwhiteI_pval < 0.05,1,0)
#spplot(county.data,"percwhiteI", main="Moran's I")
#spplot(county.data,"percwhiteI_pval", main='P-Value')
#spplot(county.data,"percwhiteI_pvalFlag", main='P-Value Indicator')

#county.data@data$percblackImat <- localmoran(percblack,W)
#county.data@data$percblackI <- county.data@data$percblackImat[,1]
#county.data@data$percblack_pval <- county.data@data$percblackImat[,5]
#county.data@data$percblack_pvalFlag <- ifelse(county.data@data$percblack_pval < 0.05,1,0)
#spplot(county.data,"percblackI", main="Moran's I")
#spplot(county.data,"percblack_pval", main='P-Value')
#spplot(county.data,"percblack_pvalFlag", main='P-Value Indicator')

#Autocorrelation based on Party Affiliation
county.data@data$percRep <- county.data@data$CongRep/county.data@data$Votes
county.data@data$percDem <- county.data@data$CongDem/county.data@data$Votes
county.data@data$percInd <- county.data@data$CongInd/county.data@data$Votes

percRep <- county.data@data$percRep
RepMoran <- as.data.frame(localmoran(percRep,W))
colnames(RepMoran) <- c("I","EI","VarI","Zi","Pval")
Rep <- county.data
Rep@data <- RepMoran
Rep@data$percRep <- county.data@data$percRep
Rep@data$PvalFlag <- ifelse(Rep@data$Pval < .05, 1,0)

#spplot(Rep,"I", main="Moran's I")
#spplot(Rep,"Pval", main="P-Value")

Repplot1 <- spplot(Rep,"PvalFlag",main="Republican Vote Share Autocorrelation")
Repplot2 <- spplot(Rep,"percRep",main="Republican Vote Share")
Repplots <- list(Repplot1,Repplot2)
do.call(grid.arrange,c(Repplots,nrow=1))


percDem <- county.data@data$percDem
DemMoran <- as.data.frame(localmoran(percDem,W))
colnames(DemMoran) <- c("I","EI","VarI","Zi","Pval")
Dem <- county.data
Dem@data <- DemMoran
Dem@data$percDem <- county.data@data$percDem
Dem@data$PvalFlag <- ifelse(Dem@data$Pval<.05,1,0)

#spplot(Dem,"I", main="Moran's I")
#spplot(Dem,"Pval", main="P-Value")
Demplot1 <- spplot(Dem,"PvalFlag",main="Democratic Vote Share Autocorrelation")
Demplot2 <- spplot(county.data,"percDem",main="Democratic Vote Share")
Demplots <- list(Demplot1,Demplot2)
do.call(grid.arrange,c(Demplots,nrow=1))



percInd <- county.data@data$percInd
IndMoran <- as.data.frame(localmoran(percInd,W))
colnames(IndMoran) <- c("I","EI","VarI","Zi","Pval")
Ind <- county.data
Ind@data <- IndMoran
Ind@data$percInd <- county.data@data$percInd
Ind@data$PvalFlag <- ifelse(Ind@data$Pval <0.05,1,0)

Indplot3 <- spplot(Ind,"I", main="Moran's I")
Indplot4 <- spplot(Ind,"Pval", main="P-Value")
Indplots2 <- list(Indplot3,Indplot4)
do.call(grid.arrange,c(Indplots2,nrow=1))

Indplot1 <- spplot(Ind,"PvalFlag",main="Independent Vote Share Autocorrelation")
Indplot2 <- spplot(Ind,"percInd",main="Independent Vote share")
Indplots <- list(Indplot1,Indplot2)
do.call(grid.arrange,c(Indplots,nrow=1))



#par(mfrow=c(1,1))



spplot(county.data,"MeanIncome", main='Mean Income by County')

county.data@data$perc_minority <- 1-county.data@data$perc_white

Raceplot1 <- spplot(county.data,"perc_minority", main='Percent of Population - Minority')
Raceplot2 <- spplot(county.data,"perc_white", main='Percent of Population - White')
Raceplots <- list(Raceplot1,Raceplot2)
do.call(grid.arrange,c(Raceplots,nrow=1))


Partyplot1 <- spplot(county.data,"percDem", main='Democratic Vote Share')
Partyplot2 <- spplot(county.data,"percRep", main='Republican Vote Share')
Partyplots <- list(Partyplot1,Partyplot2)
do.call(grid.arrange,c(Partyplots,nrow=1))


Ageplot1 <- spplot(county.data,"Age60to79", main='% Pop between Age 60 to 79')
Ageplot2 <- spplot(county.data,"Age40to59",main='% Pop between Age 40 to 59')
Ageplot3 <- spplot(county.data,"Age20to39", main='% Pop between Age 20 to 39')
Ageplots <- list(Ageplot3,Ageplot2,Ageplot1)
do.call(grid.arrange,c(Ageplots,nrow=1))

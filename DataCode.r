library(openxlsx)
library(maptools)
library(sp)
library(rgeos)
library(dplyr)
library(sqldf)

setwd('C:\\Users\\lthompson24500\\Google Drive\\Spatial Final Project\\FinalDataSets')


elecResults <- read.xlsx('2016_results.xlsx')
AgeDat <- read.csv('AgeData.csv')
Income <- read.csv('MedianIncome.csv')
Race <- read.csv('Race.csv')
shape <- readShapePoly('county2010.shp')
splits <- read.csv('DistrictSplits.csv')


## Group ElectionResults
elecCounty <- elecResults %>% 
        group_by(COUNTYNAME, COUNTYCODE) %>% 
        summarise(Votes = sum(TOTVOTING), RegisteredVoters = sum(REG7AM), CongRep = sum(USREPR),
                  CongDem = sum(USREPDFL), CongInd = sum(USREPTOTAL - USREPR - USREPDFL)) 

elecCounty <- elecCounty[elecCounty$COUNTYNAME !="TOTAL",]

## Clean Age 
AgeSub <- AgeDat %>% select(c(3,seq(4, dim(AgeDat)[2],6)))
AgeSub2 <- AgeSub %>% select(1:20)

AgeSub2$Age0to19 <- sapply(1:dim(AgeSub2)[1], function(i) sum(AgeSub[i,3:6]))
AgeSub2$Age20to39 <- sapply(1:dim(AgeSub2)[1], function(i) sum(AgeSub[i,7:10]))
AgeSub2$Age40to59 <- sapply(1:dim(AgeSub2)[1], function(i) sum(AgeSub[i,11:14]))
AgeSub2$Age60to79 <- sapply(1:dim(AgeSub2)[1], function(i) sum(AgeSub[i,15:18]))
AgeSub2$AgeOver80 <- sapply(1:dim(AgeSub2)[1], function(i) sum(AgeSub[i,19:20]))

AgeClean <- AgeSub2 %>% select(c(1,2,21:25))
colnames(AgeClean) <- c('County','TotalPopulation', colnames(AgeClean)[3:7])

AgeClean$County <- as.character(AgeClean$County)
AgeClean$County <- substr(AgeClean$County, 1, (nchar(AgeClean$County) - 18))

## Clean Race 
RaceClean <- Race %>% select(c(3,seq(6,(dim(Race)[2] - 6), 2)))
colnames(RaceClean) <- c('County','White','Black','Indian','Asian','PacificIslander','Other')

RaceClean$County <- as.character(RaceClean$County)
RaceClean$County <- substr(RaceClean$County, 1, (nchar(RaceClean$County) - 18))

## Clean Income 
IncomeClean <- Income %>% select(c(3,4,6))
colnames(IncomeClean) <- c('County','TotalEstimateAllHouseholds','MeanIncome')

IncomeClean$County <- as.character(IncomeClean$County)
IncomeClean$County <- substr(IncomeClean$County, 1, (nchar(IncomeClean$County) - 18))


## Make sure all county names are lowercase for joining
colnames(elecCounty) <- c('County','CountyCode',colnames(elecCounty)[3:7])

elecCounty$CountyOrig <- elecCounty$County

elecCounty$County <- tolower(elecCounty$County)
AgeClean$County <- tolower(AgeClean$County)
IncomeClean$County <- tolower(IncomeClean$County)
RaceClean$County <- tolower(RaceClean$County)
shape$NAME <- tolower(shape$NAME)


## Join Data together 
shapeDat <- shape@data

JoinedCounty <- sqldf('select a.NAME, b.*, c.TotalPopulation, c.Age0to19, c.Age20to39,
                        c.Age40to59, c.Age60to79, c.AgeOver80, d.White, d.Black,
                        d.Indian, d.Asian, d.PacificIslander, d.Other, e.TotalEstimateAllHouseholds, e.MeanIncome
                      from shapeDat a
                            join elecCounty b on a.NAME = b.County 
                            join AgeClean c on a.NAME = c.County 
                            join RaceClean d on a.NAME = d.County
                            join IncomeClean e on a.NAME = e.County')


shape@data <- JoinedCounty
writePolyShape(shape, 'CountyDemographics.shp')












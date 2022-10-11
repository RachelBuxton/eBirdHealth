
rm(list=ls())
gc()

library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tigris)
library(viridisLite)
library(rgdal)
library(plyr)
library(vegan)
library(ggplot2)
library(janitor)
library(lme4)

setwd("C:/Users/rbuxton/Documents/Pearson/eBird")

####Read in Geospatial data (April 2021) and eBird species diversity data
Geospatial <- read.csv("Geospatial Features/Buxton_RequestedDataset_Wave123_16April2021.csv")
Geospatial<-unite(Geospatial, Latitude, Longitude, col="latlong",sep = "_", remove = FALSE)

#DiversityIndices<-
  
##Make sure 30 points are 200 m or 1 km away from each other
Finalpoints<-ddply(DiversityIndices, c("latlong"), summarize, latitude=first(latitude), longitude=first(longitude))

#####Make ebird dataframe into spatial points
pnts_sf <- st_as_sf(Finalpoints, coords = c('longitude', 'latitude'), crs=4326)
pnts_sf_all_proj<- st_transform(pnts_sf, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

Dist<-st_distance(pnts_sf_all_proj)
# range(Dist)
Dist<-data.frame(Dist)
for(i in 1:ncol(Dist)){
  Dist[,i]<-as.numeric(Dist[,i])
}
Dist[upper.tri(Dist)]<-0

DistancefromFinalPoints<-Finalpoints
DistancefromFinalPoints$Distancetonearestpoint<-apply(Dist, 2, FUN = function(x) {min(x[x > 0])})
DistancefromFinalPoints$WhichPointisClosest<-c(DistancefromFinalPoints$latlong[unlist(apply(Dist, 2, FUN = function(x) {which(x==min(x[x > 0]))}))],0)

##################################
###MERGE EBIRD DIVERSITY AND GEOSPATIAL FEATURES
##################################

# table(DiversityIndices$year)
# table(DiversityIndices$month)

# select the necessary columns
Geospatial[,grep("buff", colnames(Geospatial))][is.na(Geospatial[,grep("buff", colnames(Geospatial))])] <- 0

cor(Geospatial[,c(7:8)]) #-0.30 - DISTANCE TO VACANT LOTS
cor(Geospatial[,c(13:14)]) #0.05 - VACANT LOTS 200 m
cor(Geospatial[,c(15:16)]) #-0.38 - VACANT LOTS 1 km
cor(Geospatial[,c(19:20)]) #0.99
cor(Geospatial[,c(21:22)]) #0.98
cor(Geospatial[,c(29:30)]) #0.99
cor(Geospatial[,c(31:32)]) #0.99

Geospatial <- Geospatial %>% select(latlong, Near_UmPark_area, Near_AllPark_area)

##Each checklist
Geospatial_checklist<-left_join(DiversityIndices, Geospatial, by="latlong")

##Summarized by site
DiversityIndexSummary<-ddply(DiversityIndices, c("latlong"), summarize, MeanShannon=mean(Shannon), MeanRichness=mean(Richness), NumberofChecklists=length(Shannon), avMonth=mean(month))

Geospatial_Site<-left_join(Geospatial, DiversityIndexSummary, by="latlong")%>%
  filter(complete.cases(MeanShannon))


# pnts_sf_checl <- st_as_sf(Geospatial_Site, coords = c('Longitude', 'Latitude'), crs = st_crs(Detroit))
# pnts_sf_checl_proj<- st_transform(pnts_sf_checl, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# 
# ggplot() +
#   geom_sf(data = Detroit_proj) +
#   geom_sf(data = pnts_sf_checl_proj, size=3, aes(colour=log(NumberofChecklists)))+
#   scale_colour_gradient2()+
#   labs(colour = "eBird Checklists (log)")+
#   theme_bw()

##################################
###MODELS
##################################

#Center scale

lmer(Shannon~(1|checklist_id),data=Geospatial_checklist)

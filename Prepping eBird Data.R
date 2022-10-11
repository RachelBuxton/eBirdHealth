
###################################################################################################################################################################
##### eBird Data downloaded for Wayne County 2017-2021 from https://ebird.org/data/download - March 31, 2021
##### Shapefile of Detroit city limits from the City of Detroit website: https://data.detroitmi.gov/datasets/city-of-detroit-boundary?geometry=-83.796%2C42.264%2C-82.402%2C42.442
##### This code clips Wayne county checklists to the Detroit city limits, then limits to spatial locations with >9 checklists
###################################################################################################################################################################

rm(list=ls())
gc()

library(auk)
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

setwd("C:/Users/rbuxton/Documents/Pearson/eBird")

####Read in eBird Data and Detroit shapefile
ebd <- read_ebd("ebd_US-MI-163_201701_202103_relFeb-2021/ebd_US-MI-163_201701_202103_relFeb-2021.txt")
Detroit<-read_sf("City_of_Detroit_Boundary-shp/72de894c-64b4-4bc1-9001-f5090ddd6c672020328-1-j6yh1z.ji6dl.shp")

# range(format(ebd$observation_date, format = "%Y"))

#####Make ebird dataframe into spatial points
pnts_sf <- st_as_sf(ebd, coords = c('longitude', 'latitude'), crs = st_crs(Detroit))
st_crs(pnts_sf) <- st_crs(Detroit)

######Subset ebird to points that fall within Detroit
ebd$Detroit<-as.integer(st_intersects(pnts_sf, Detroit))
Detroit_ebd<-subset(ebd, Detroit==1)

# Double check plots
# plot(st_geometry(Detroit))
# pnts_sf_checl <- st_as_sf(Detroit_ebd, coords = c('longitude', 'latitude'), crs = st_crs(Detroit))
# plot(pnts_sf_checl, add=TRUE)

######Do some filtering
#Complete checklists only
Detroit_ebd<-subset(Detroit_ebd, all_species_reported=="TRUE")

#From Callaghan et al 2017
# We  also  limited  checklists  used according to the following criteria: 
# (1) checklists were excluded if they had associated "group-identifiers" because these represent duplicated checklists, 
Detroit_ebd<-Detroit_ebd[is.na(Detroit_ebd$group_identifier),]

#(2) checklists were excluded if they recorded a travel distance > 10 km, 
Detroit_ebd<-subset(Detroit_ebd, effort_distance_km<=10|is.na(effort_distance_km))

#(3) checklists were included only if the recording  duration  was  between  5  and  240  minutes
Detroit_ebd<-subset(Detroit_ebd, duration_minutes>=5&duration_minutes<=240)

#(4)checklists were included only if they followed the "stationary,""travelling," or "exhaustive" protocols ( 
Detroit_ebd<-subset(Detroit_ebd, protocol_type=="Stationary"|protocol_type=="Traveling"||protocol_type=="Exhaustive")

Detroit_ebd$Coords<-paste(Detroit_ebd$latitude, Detroit_ebd$longitude, sep="_")
Detroit_ebd$Year<-format(Detroit_ebd$observation_date, format = "%Y")

###############################################################################################################################
##Plotting 

# #transform first
# Detroit_proj <- st_transform(Detroit, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# pnts_sf_checl <- st_as_sf(Detroit_ebd_summary, coords = c('longitude', 'latitude'), crs = st_crs(Detroit))
# pnts_sf_checl_proj<- st_transform(pnts_sf_checl, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# 
# #All points
# pnts_sf_allpoints <- st_as_sf(Detroit_ebd, coords = c('longitude', 'latitude'), crs = st_crs(Detroit))
# pnts_sf_all_proj<- st_transform(pnts_sf_allpoints, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# 
# #Buffers
# pnts_sf_checl_proj_buff200 = st_buffer(pnts_sf_checl_proj, dist = 200)
# pnts_sf_checl_proj_buff1000 = st_buffer(pnts_sf_checl_proj, dist = 1000)
# pnts_sf_all_buff200 = st_buffer(pnts_sf_all_proj, dist = 200)
# pnts_sf_all_proj_buff1000 = st_buffer(pnts_sf_all_proj, dist = 1000)
# 
# ggplot() +
#  geom_sf(data = Detroit_proj) +
#   geom_sf(data = pnts_sf_all_proj) +
#   geom_sf(data = pnts_sf_all_buff200,colour="darkred", fill=NA)+
#   geom_sf(data = pnts_sf_all_proj_buff1000,colour="blue", fill=NA)
# 
# ggplot() +
#   geom_sf(data = Detroit_proj) +
#   geom_sf(data = pnts_sf_all_proj) +
#   geom_sf(data = pnts_sf_checl_proj_buff1000,colour="blue", fill=NA)
# 
# ggplot() +
#   geom_sf(data = Detroit_proj) +
#   geom_sf(data = pnts_sf_checl, aes(colour=log(Checklists)))+
#   scale_colour_gradient2()
# 
# #############################################
# ##GRID
# #1 km = 19 sites
# grid <- Detroit_proj %>% 
#   st_make_grid(cellsize = 1000, what = "polygons") %>% # grid at 1 km
#   st_intersection(Detroit_proj) %>%  
#   st_sf(grid_id = 1:length(.))
# 
# ggplot() + 
#   geom_sf(data = Detroit_proj) + 
#   geom_sf(data = grid)+
#   geom_sf(data = pnts_sf_all_proj) 
# 
# # which grid square is each point in?
# GridPoints<-pnts_sf_all_proj %>% st_join(grid, join = st_intersects) %>% as.data.frame
# 
# #summarize the number of checklists
# Detroit_ebd_summary_grid<-ddply(GridPoints, c("grid_id"), summarize, Checklists=length(unique(checklist_id))) 
# Detroit_ebd_summary_grid<-subset(Detroit_ebd_summary_grid, Checklists>=9) #only 19 sites

#############################################

# Detroit_ebd_summary<-ddply(Detroit_ebd, c("Coords", "latitude", "longitude"), summarize, Checklists=length(unique(checklist_id))) 
# 
# ##Spatial clustering
# library(sp)
# library(rgdal)
# library(geosphere)
# xy <- SpatialPointsDataFrame(
#   matrix(c(Detroit_ebd_summary$longitude,Detroit_ebd_summary$latitude), ncol=2), data.frame(ID=seq(1:length(Detroit_ebd_summary$longitude))),
#   proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# mdist <- distm(xy)
# hc <- hclust(as.dist(mdist), method="complete")
# xy$clust <- cutree(hc, h=1000)
# 
# Detroit_ebd_summary$clust<-xy$clust
# Detroit_ebd_summary_summary<-ddply(Detroit_ebd_summary, c("clust"), summarize, ChecklistsTotal=sum(Checklists)) 
# Detroit_ebd_summary_summary<-subset(Detroit_ebd_summary_summary, ChecklistsTotal>=9) #only 31 sites

# library(dismo)
# library(rgeos)
# 
# # expand the extent of plotting frame
# xy@bbox[] <- as.matrix(extend(extent(xy),0.001))
# 
# # get the centroid coords for each cluster
# cent <- matrix(ncol=2, nrow=max(xy$clust))
# for (i in 1:max(xy$clust))
#   # gCentroid from the rgeos package
#   cent[i,] <- gCentroid(subset(xy, clust == i))@coords
# 
# # compute circles around the centroid coords using a 40m radius
# # from the dismo package
# ci <- circles(cent, d=1000, lonlat=T)
# 
# # plot
# plot(ci@polygons, axes=T)
# plot(xy, col=rainbow(79)[factor(xy$clust)], add=T)

##################################################################

#summarize the number of checklists
Detroit_ebd_summary<-ddply(Detroit_ebd, c("Coords", "latitude", "longitude"), summarize, Checklists=length(unique(checklist_id))) 
Detroit_ebd_summary<-subset(Detroit_ebd_summary, Checklists>=9) #only 30 sites

#Extract sites with >9 checklists
Detroit_ebd_9checklists <- Detroit_ebd[which(Detroit_ebd$Coords %in% Detroit_ebd_summary$Coords),]

###Clean up spreadsheet
Detroit_ebd_9checklists$Month<-format(Detroit_ebd_9checklists$observation_date, format = "%m")
Detroit_ebd_9checklists$Hour<-substr(Detroit_ebd_9checklists$time_observations_started, 1,2)

#Generate the final Detroit checklist dataframe
FinalDataframe<-Detroit_ebd_9checklists[,which(colnames(Detroit_ebd_9checklists)%in%c("latitude","longitude", "common_name", "observation_count", "observer_id","checklist_id","observation_date","Year","Month" ,"time_observations_started", "Hour"))]

#"X" observation_count = presence. Replace with 1
FinalDataframe[which(FinalDataframe$observation_count=="X"),]$observation_count<-"1"
FinalDataframe$observation_count<-as.numeric(FinalDataframe$observation_count)


#Species
# Species<-ddply(FinalDataframe, c("common_name"), summarize, TotalObsCount=sum(as.numeric(observation_count)))%>%
#   `colnames<-`(c("Var1", "TotalObsCount"))%>%
# left_join(data.frame(table(FinalDataframe$common_name)))
# length(which(Species$Freq==1))

##########Remove species present in fewer than 5% of checklists at a given site 
FinalDataframe<-unite(FinalDataframe, latitude, longitude, col="latlong",sep = "_", remove = FALSE)

##Reshape the dataframe - first summarize to add together duplicates in check lists, then restructure to wide form
Detroit_SpRich_restructure <- FinalDataframe %>%
  spread(common_name, observation_count)

#Make NAs into 0
Cols<-(which(colnames(Detroit_SpRich_restructure)=="Hour")+1):ncol(Detroit_SpRich_restructure)
Detroit_SpRich_restructure[,Cols][is.na(Detroit_SpRich_restructure[,Cols])] <- 0

##Loop through each site to remove species present in fewer than 5% of checklists

DiversityIndices<-lapply(1:length(unique(Detroit_SpRich_restructure$latlong)), function(i){
  Test<-Detroit_SpRich_restructure[which(Detroit_SpRich_restructure$latlong==unique(Detroit_SpRich_restructure$latlong)[i]),]
  
  Test <- clean_names(Test) #clean up column names
  SpeciesSummary<-ddply(Test, c("checklist_id"), colwise(sum, colnames(Test)[Cols]))
  
  RareSpecies<-NULL
  for(i in 2:ncol(SpeciesSummary)){
    RareSpecies[i]<-nrow(SpeciesSummary[SpeciesSummary[,i]>=1, ])
  }
  
  Test<-Test[,c(1:which(colnames(Test)=="hour"),which(colnames(Test)%in%colnames(SpeciesSummary)[(which(RareSpecies>=round(.05*nrow(SpeciesSummary))))]))]
  
 Test
})

DiversityIndices<-do.call("rbind", DiversityIndices)


#Species diversity metrics by checklist
Cols<-(which(colnames(Test)=="hour")+1):ncol(Test)
Indices<-data.frame(cbind(unname(diversity(Test[,Cols], index = "shannon")),
                          #Total number of species  
                          unname(specnumber(Test[,Cols]))))
colnames(Indices)<-c("Shannon","Richness")
Indices<-cbind(Test[,-Cols], Indices)           
#Species richness and diversity 
Indices




Observers<-data.frame(table(Detroit_ebd_17checklists$observer_id))

table(Detroit_ebd_17checklists$protocol_type)
range(na.omit(Detroit_ebd_17checklists$effort_distance_km))
table(Detroit_ebd_17checklists$group_identifier)


##Output for Wei to extract geospatial information
# Detroit_ebd_summary<-ddply(Detroit_ebd_17checklists, c("Coords"), summarize, Latitude=unique(latitude), Longitude=unique(longitude))
# Detroit_ebd_summary<-Detroit_ebd_summary[,2:3]
# write.csv(Detroit_ebd_summary, "Coordinates_of_eBirdChecklists.csv", row.names = F)

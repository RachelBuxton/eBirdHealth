

rm(list=ls())

directory<-"C:\\Users\\rbuxton\\Documents\\Pearson\\eBird\\AcousticData"
LEVEL<-"Site"

library(plyr)
library(vegan)

source('C:\\Users\\rbuxton\\Documents\\Pearson\\eBird\\functions\\Summarize Bird Species Data_02.R')
Dataframe<-SpeciesDiversity(directory, LEVEL)

GeospatialData<-read.csv("C:\\Users\\rbuxton\\Documents\\Pearson\\eBird\\Geospatial features\\Buxton_RequestedDataset_Wave123_16April2021.csv")

unique(GeospatialData$NearSite_in9)
unique(GeospatialData$NearSite_in11)

#Load up XY for each acoustic recorder
XYRecordings<-read.csv("C:\\Users\\rbuxton\\Documents\\Pearson\\eBird\\Audiomoth_in_park_XY.csv")

#Merge with Audio summary
AudioSummaryXY<-merge(Dataframe, XYRecordings, by="Site")

##Load up eBird data

#For each unique site, which eBird site is the closest

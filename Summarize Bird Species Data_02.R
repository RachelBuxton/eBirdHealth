



SpeciesDiversity<-function(directory, LEVEL){

#Reads in audio selection tables
source('C:\\Users\\rbuxton\\Documents\\Pearson\\eBird\\functions\\Read in Acoustic Data_01.R')
Dataframe<-ReadinSpreadsheets(directory)

##Remove faint and background calls
Dataframe<-Dataframe[-which(Dataframe$Species=="FaintCall"|Dataframe$Species=="FatinCall"),]
Dataframe$Count<-as.numeric(Dataframe$Count)

###################################################################################################################################################################
##########                                                 Summarize by 5 min clip
###################################################################################################################################################################

if(LEVEL=="Clip"){
Summary<-ddply(Dataframe, c("Site", "Yr","Mo","Day","Date", "Hr", "Min", "Sec", "Time", "Species"), summarize, Total=sum(Count))

##Reshape the dataframe
SpRich_restructure <- reshape(Summary, timevar = "Species", 
                              idvar = c("Site", "Yr","Mo","Day","Date", "Hr", "Min", "Sec", "Time"), direction = "wide")

##Fill NA with 0
Cols<-grep("Total", colnames(SpRich_restructure))
SpRich_restructure[,Cols][is.na(SpRich_restructure[,Cols])] <- 0

## Limit to birds only (i.e., remove other sound codes) 
SpRich_restructure<-suppressWarnings(SpRich_restructure[,which(is.na(as.numeric(gsub("Total.", "", colnames(SpRich_restructure)))))])

##Species Richness by site day time
Cols<-grep("Total.", colnames(SpRich_restructure))
Indices_Clip<-data.frame(cbind(unname(diversity(SpRich_restructure[,Cols], index = "shannon")),
                          unname(diversity(SpRich_restructure[,Cols], index = "simpson")),
                          #fisher Alpha  
                          unname(fisher.alpha(SpRich_restructure[,Cols], MARGIN = 1)),
                          #Total number of species  
                          unname(specnumber(SpRich_restructure[,Cols])),
                          #Evenness
                          unname(diversity(SpRich_restructure[,Cols]))/log(unname(specnumber(SpRich_restructure[,Cols])))))
colnames(Indices_Clip)<-c("Shannon","Simpson","Fisher","Richness", "Evenness")
Indices_Clip<-cbind(SpRich_restructure[,-Cols], Indices_Clip)            

###Insert total number of calls
Indices_Clip$Totalcalls<-rowSums(SpRich_restructure[,Cols])
return(Indices_Clip)

}else{
###################################################################################################################################################################
##########                                                 Summarize by Site and Year
###################################################################################################################################################################

Summary<-ddply(Dataframe, c("Site", "Yr", "Species"), summarize, Total=sum(Count))

##Reshape the dataframe
SpRich_restructure <- reshape(Summary, timevar = "Species", 
                              idvar = c("Site", "Yr"), direction = "wide")

##Fill NA with 0
Cols<-grep("Total", colnames(SpRich_restructure))
SpRich_restructure[,Cols][is.na(SpRich_restructure[,Cols])] <- 0

## Limit to birds only (i.e., remove other sound codes) 
SpRich_restructure<-suppressWarnings(SpRich_restructure[,which(is.na(as.numeric(gsub("Total.", "", colnames(SpRich_restructure)))))])

##Species Richness by site day time
Cols<-grep("Total.", colnames(SpRich_restructure))
Indices_SiteYr<-data.frame(cbind(unname(diversity(SpRich_restructure[,Cols], index = "shannon")),
                          unname(diversity(SpRich_restructure[,Cols], index = "simpson")),
                          #fisher Alpha  
                          unname(fisher.alpha(SpRich_restructure[,Cols], MARGIN = 1)),
                          #Total number of species  
                          unname(specnumber(SpRich_restructure[,Cols])),
                          #Evenness
                          unname(diversity(SpRich_restructure[,Cols]))/log(unname(specnumber(SpRich_restructure[,Cols])))))
colnames(Indices_SiteYr)<-c("Shannon","Simpson","Fisher","Richness", "Evenness")
Indices_SiteYr<-cbind(SpRich_restructure[,-Cols], Indices_SiteYr)            

###Insert total number of calls
Indices_SiteYr$Totalcalls<-rowSums(SpRich_restructure[,Cols])
return(Indices_SiteYr)
}

}



####################################  ####################################  ####################################  ####################################
### THIS CODE READS IN SELECTION TABLES MADE WITH RAVEN
##  CLEANS ANY ERRORS IN 4 LETTER BIRD CODES OR SOUND CODES
### AND OUTPUTS A CLEAN SPREADSHEET WITH DATES AND TIMES EXTRACTED FROM FILENAMES
####################################  ####################################  ####################################  ####################################

ReadinSpreadsheets<-function(directory){
  
  ####################################
  
  #List all the selection tables in the folder
  Allfiles<-list.files(directory,
                       pattern=".txt", recursive=TRUE, full.names = TRUE)
  
  ##Read in all files
  Files<-lapply(1:length(Allfiles), function(i){
    test<-suppressWarnings(read.table(Allfiles[i], sep = "\t", header = TRUE))#suprress error
    test
  })
  Files<-do.call("rbind", Files)
  
  #Seperate species codes from counts
  Files$Species<-sapply(strsplit(as.character(Files$NOTES), split="_"),"[", 1)
  Files$Count<-sapply(strsplit(as.character(Files$NOTES), split="_"),"[", 2)
  
  #Fix some errors
  Files$Species<-ifelse(as.character(Files$Species)=="BJLA", "BLJA", as.character(Files$Species))
  Files$Species<-ifelse(as.character(Files$Species)==" 19", "19", as.character(Files$Species))
  
  #Remove background sound
  # Files$Count<-ifelse(Files$Count=="Constant", 60, as.character(Files$Count))
  Files<-Files[-which(Files$Count=="Background"),]
  
  #Make NA = 1
  Files$Count<-ifelse(is.na(Files$Count),1,Files$Count)
  
  # #Make Site, year, month, day, and date columns from file names
  Files$Site<-sapply(strsplit(as.character(Files$Begin.Path),"\\\\"),function(x) {  x[4] })
  
  
  #File structure for 2019 and 2020 are different
  Date<-sapply(strsplit(as.character(Files$Begin.File),"_"),function(x) {  x[grep("^20", x)] })#pull out date part
  Files$Yr<- substr(Date,1,4)
  Files$Mo<- substr(Date,5,6)
  Files$Day<- substr(Date,7,8)
  Files$Date<-paste(Files$Yr, Files$Mo, Files$Day, sep="_")
  
  #Make time columns from file names
  Time<-sapply(strsplit(as.character(Files$Begin.File),"_"),function(x) {  x[grep(".wav", x, ignore.case = TRUE)] })#pull out time part
  Files$Hr<-substr(Time,1,2)
  Files$Min<-substr(Time,3,4)
  Files$Sec<-substr(Time,5,6)
  Files$Time<-paste(Files$Hr, Files$Min, Files$Sec, sep="_")
  
  #Final dataframe
  Dataframe<-Files[,which(colnames(Files)%in%c("Site", "Yr", "Mo", "Day", "Date", "Hr", "Min", "Sec", "Time", "Species", "Count"))]
  
  #Fix site names
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="BryVerm","BRY", as.character(Dataframe$Site))
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="Callahan", "CAL", as.character(Dataframe$Site))
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="HarpCo", "HAR", as.character(Dataframe$Site))
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="KeatSF", "KEA", as.character(Dataframe$Site))
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="Lawrence", "LAW", as.character(Dataframe$Site))
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="Lifsitz", "LIF", as.character(Dataframe$Site))
  Dataframe$Site<-ifelse(as.character(Dataframe$Site)=="McKMerr", "MCK", as.character(Dataframe$Site))
  
  return(Dataframe)
  }
  
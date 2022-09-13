### Install and import the following packages
library(dplyr)
library(data.table)
library(stringr)
library(lubridate)

### Import and merge files from all sites and years
# Station list from USCRN can be accessed here: https://www.ncei.noaa.gov/maps/crn/
# First step is to download daily moisture files from all sites here: https://www.ncei.noaa.gov/pub/data/uscrn/products/daily01/snapshots/
# Set the path to where data are downloaded and then read all files from the folder and sub-folders
list_of_files <- list.files(path = ".", recursive = TRUE,
                            pattern = "\\.txt$", full.names = TRUE)
data <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                  use.names = TRUE, fill=TRUE, idcol = "FileName" )
data <- data[,c(1,3,6,5,16,20:29)]
data$State = substr(data$FileName,22,23)
data$string = substring(data$FileName,first = 25)
setDT(data)[,paste0("string",1:5) := tstrsplit(string, "_")]
data1 <- data[which((is.na(data$string4))&(is.na(data$string5))),]
data2 <- data[which((!is.na(data$string4))&(is.na(data$string5))),]
data3 <- data[which((!is.na(data$string4))&(!is.na(data$string5))),]
data1$Site <- data1$string1
data2$Site <- paste(data2$string1,data2$string2)
data3$Site <- paste(data3$string1,data3$string2,data3$string3)
data_reorg <- rbind(data1,data2,data3)
data_reorg <- data_reorg[,c(23,16,2:15)]
colnames(data_reorg)[3:16] <- c("Date","Lat","Long","T","SWC_5","SWC_10","SWC_20","SWC_50","SWC_100","TS_5","TS_10","TS_20","TS_50","TS_100")

### Rearrange the data to present paired moisture and temperature records in individual rows
data_flat <- melt(setDT(data_reorg), id.vars=c("Site","State","Date","Lat","Long","T"))
data.mois <- data_flat[str_detect(data_flat$variable,"SWC"),]
data.mois$Depth <- as.numeric(substring(data.mois$variable,first = 5))
data.temp <- data_flat[str_detect(data_flat$variable,"TS"),]
data.temp$Depth <- as.numeric(substring(data.temp$variable,first = 4))
data.comb <- merge(data.mois,data.temp,by = c("Site","State","Date","Depth","Lat","Long","T")) 
data.comb <- data.comb[,c(1:7,9,11)]
colnames(data.comb)[8:9] <- c("Moisture","Temperature")

### An optional step to filter out records not within states of interests
list <- c("WA","OR","CA","NV","ID","MT","WY","UT","CO","AZ","NM","ND","SD",
          "NE","KS","OK","TX","MN","IA","MO","WI","IL","MI","IN","OH","KY")
data.f1 <- data.comb[data.comb$State %in% list,]

### Filter based on other criteria set for quality control
data.f2 <- data.f1[which(data.f1$Moisture > 0),] #Exclude low moisture contents
data.f3 <- data.f2[which(data.f2$Moisture < 0.6),] #Exclude high moisture contents
data.f4a <- data.f3[which(data.f3$Temperature > 0),] #Exclude frozen soils
data.f4b <- data.f3[which((data.f3$Temperature == -9999)&(data.f3$T > 0)),] #Exclude frozen soils
data.f4 <- rbind(data.f4a,data.f4b)
data.f4$Moisture <- data.f4$Moisture*100 #Convert moisture unit
data.f4$water_year <- round(data.f4[,3]/1e4)
data.f4$month <- round((data.f4[,3] - data.f4$water_year*1e4)/1e2) 
data.f4$day <- data.f4[,3] - data.f4$water_year*1e4 - data.f4$month*1e2
data.f4$water_day <- yday(ymd(data.f4$Date))
data.f4$date <- with(data.f4, ymd(paste(water_year, month, day, sep= ' ')))
data.f <- data.f4[which(data.f4$water_year >= 2002),] #Keep records based on study period
data.f <- data.f[which(data.f$water_year <= 2019),] #Keep records based on study period
data.final <- data.f[,c(1:2,5:6,14,4,10,13,8)]
colnames(data.final)[c(5,7,8)] <- c("Date","Water_year","Water_day")

### Export results
write.csv(data.final,"USCRN_soil_moisture.csv")
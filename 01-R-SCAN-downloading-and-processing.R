### Install and import the following packages
library(RODBC)
library(latticeExtra)
library(plyr)
library(rvest)
library(httr)
library(reshape2)
library(maps)
library(RColorBrewer)
library(sharpshootR)
library(aqp)
library(soilDB)
library(dplyr)

### Prepare site files and import
# The file can have different format but should contain site_ID and establishment year. 
# The following example uses 'ID' and 'Start_year' for exacting SCAN data (SMS for moisture and STO for temperature)
# Select stations from the USDA website https://www.nrcs.usda.gov/wps/portal/wcc/home/quicklinks/
# Start year of the stations can be accessed here: https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=scan&counttype=statelist
site <- read.table('SCAN_datasets.csv', comment.char="", quote = "\"", header=T, sep=",") # Make sure to set the correct path to read the file
sitelist <- site$ID
data <- data.frame()
data.cov <- data.frame()
for (i in 1:length(unique(site$ID))){
  siteID = sitelist[i]
  siteyear = site$Start_Year[i]
  dataall = fetchSCAN(site.code=c(siteID), year=siteyear:2019)
  data.i = dataall$SMS
  data.j = dataall$STO
  data <- rbind(data,data.i) 
  data.cov <- rbind(data.cov,data.j) 
}

### Quality control
data.f1 <- data[which(data$depth != "NA"),]
data.f2 <- data.f1[which(data.f1$value > 0),]
data.f3 <- data.f2[which(data.f2$value <= 60),]
data.comb <- merge(data.f3,data.cov,by = c("Site","Date","depth")) 
data.f <- data.comb %>%
  group_by(Site, Date, depth) %>%
  summarise_at(vars("water_year.x","water_day.x","value.x","value.y"), mean) #some data points have temperature reported from multiple sensors so averaging is needed
colnames(data.f)[4:7] <- c("Water_year","Water_day","Moisture","Temperature") #appears that some temperature is reported in F rather than C, hard to filter with that so use C=0 as the threshold just to be more conservative
data.f <- data.f[which(data.f$Temperature > 0),]

### Refine the dataset based on user-defined year and depth
data.f <- data.f[which(data.f$Water_year >= 2002),]
data.f <- data.f[which(data.f$depth <= 102),]
data.final <- merge(data.f,site,by.x = "Site",by.y = "ID")
data.final$State <- state.abb[match(data.final$State, state.name)]
data.final <- data.final[,c("Site","State","Lat","Long","Date","Depth","Water_year","Water_day","Moisture")]

### Export results
write.csv(data.final,"SCAN_soil_moisture.csv")
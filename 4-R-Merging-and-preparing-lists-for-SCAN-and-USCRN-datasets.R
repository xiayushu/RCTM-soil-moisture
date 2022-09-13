### Install and import the package
library(stringr)

### Import processed SCAN and USCRN files and combine them into one file
# Make sure to set correct pathways to read the files
SCAN <- read.table('SCAN_soil_moisture.csv', comment.char="", quote = "\"", header=T, sep=",")
USCRN <- read.table('USCRN_soil_moisture.csv', comment.char="", quote = "\"", header=T, sep=",")
SCAN$Network <- 'SCAN'
USCRN$Network <- 'USCRN'
SCAN$Water_year <- as.numeric(str_sub(SCAN$Date,-4,-1)) # Customize digits based on the format of dates; SCAN water_year is different from actual year
Data_moisture <- rbind(SCAN,USCRN)
Data_moisture <- Data_moisture[,c("X","Site","State","Lat","Long","Network","Depth","Date","Water_year","Water_day","Moisture")]
colnames(Data_moisture)[1] <- c("ID")
Data_moisture <- Data_moisture[which((Data_moisture$Water_year < 2020)
                                     &(Data_moisture$Water_year > 2001)),] #Optional step to retain data from years of interests

### Exclude sites under less common land cover types based on NLCD
list <- c("2105","Merced","Redding","Santa Barbara","Goodridge","St. Mary","Harrison","Coos Bay","Edinburg",
          "Palestine","Port Aransas","Quinault","Necedah")
`%ni%` <- Negate(`%in%`)
Final_moisture <- Data_moisture[Data_moisture$Site %ni% list,]

### Prepare lists needed for subsequent extraction of environmental covariates
# Get site-year list
Site_year <- Final_moisture[,c("Site","State","Lat","Long","Water_year")]
Site_year <- unique(Site_year)
write.csv(Site_year,"site_year.csv")
# Get site-depth list
Site_depth <- Final_moisture[,c("Site","State","Lat","Long","Depth")]
Site_depth <- unique(Site_depth)
write.csv(Site_depth,"site_depth.csv")
# Get site-date list
Site_date <- Final_moisture[,c("Site","State","Lat","Long","Water_year","Date")]
Site_date <- unique(Site_date)
write.csv(Site_date,"Site_date.csv")

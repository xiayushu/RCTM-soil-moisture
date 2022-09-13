### Import moisture and covariate files
# Make sure to set the correct path to read the files
# Import soil moisture file combined from SCAN and USCRN
data <- read.table('moisture_calibration_set.csv', comment.char="", quote = "\"", header=T, sep=",") 
# Import soil covariates extracted from GEE based on site-depth
sitedepth <- read.table('Site_depth_soils.csv', comment.char="", quote = "\"", header=T, sep=",")
# Import topographic covariates extracted from GEE based on site
site <- read.table('Site_topography.csv', comment.char="", quote = "\"", header=T, sep=",")
# Import LULC covariates extracted from GEE based on site-year
siteyear <- read.table('Site_year_LULC.csv', comment.char="", quote = "\"", header=T, sep=",")
# Import and process biotic and climate covariates extracted from Google Colab based on site-date
cov <- read.table('covariates.csv', comment.char="", quote = "\"", header=T, sep=",")
cov <- cov[,c(4:7,3,8:14)]
colnames(cov) <- c("Num","Site","State","Water_year","Date","ppt","Tavg","VPD","GPP","EVI","NDWI","LST")
# Import and process NLDAS values
NLDAS <- read.table('NLDAS.csv', comment.char="", quote = "\"", header=T, sep=",")
NLDAS <- NLDAS[,c(4:7,3,8:10)]
NLDAS$X6 <- NLDAS$X6 # Value for 0-10 cm; already in % unit
NLDAS$X7 <- NLDAS$X7/3 # Convert unit to % for 10-40 cm
NLDAS$X8 <- NLDAS$X8/6 # Convert unit to % for 40-100 cm
colnames(NLDAS) <- c("Num","Site","State","Water_year","Date","SM1","SM2","SM3")

### Optional step to filter based on NLCD and soil properties
siteyear <- siteyear[which(siteyear$LULC != 23),]
sitedepth <- sitedepth[which((sitedepth$SOC > 0)&(sitedepth$SOC <= 100)),]

### Combine covariate datasets
data <- merge(data, site, by = c("Site","Lat","Long"))
data <- merge(data, sitedepth, by = c("Site","Depth","Lat","Long"))
data <- merge(data, siteyear, by = c("Site","Water_year","Lat","Long"))
data <- unique(data[c("Site","State.x","Lat","Long","Network","Date","Water_year","Water_day",
                      "Depth","Moisture","EL","SL","AS","TWI","mcurv","hcurv","vcurv","surfrough",
                      "SOC","BD","Clay","Sand","LULC","Tree")])
colnames(data)[2] <- c("State")
rm("site","sitedepth","siteyear")
sitedate <- read.table('Site_date.csv', comment.char="", quote = "\"", header=T, sep=",")
cov <- merge(cov, sitedate, by="Num")
colnames(cov)[c(2,3,4,5)] <- c("Site","State","Water_year","Date")
cov$Date <- strptime(as.character(cov$Date), "%m/%d/%Y") # Convert date format in order to merge files
cov$Date <- format(cov$Date, "%Y-%m-%d")
cov <- cov[,c("Num","Site","State","Lat","Long","Date","ppt","Tavg","VPD","GPP","EVI","NDWI","LST")]
covset <- merge(data, cov, by = c("Site","State","Date","Lat","Long"))
rm("data","cov")
NLDAS <- merge(NLDAS, sitedate, by="Num")
colnames(NLDAS)[c(2,3,4,5)] <- c("Site","State","Water_year","Date")
NLDAS$Date <- strptime(as.character(NLDAS$Date), "%m/%d/%Y")
NLDAS$Date <- format(NLDAS$Date, "%Y-%m-%d")
NLDAS <- NLDAS[,c("Num","Site","State","Lat","Long","Date","SM1","SM2","SM3")]
covset <- merge(covset, NLDAS, by = c("Site","State","Date","Lat","Long"))
rm("NLDAS","sitedate")

### Extract NLDAS value based on depth of the record
covset5 <- covset[which(covset$Depth == 5),]
covset5$SM <- covset5$SM1
covset10 <- covset[which(covset$Depth == 10),]
covset10$SM <- covset10$SM1
covset20 <- covset[which(covset$Depth == 20),]
covset20$SM <- covset20$SM2
covset50 <- covset[which((covset$Depth == 50)|(covset$Depth == 51)),]
covset50$SM <- covset50$SM3
covset100 <- covset[which((covset$Depth == 100)|(covset$Depth == 102)),]
covset100$SM <- covset100$SM3
covset <- rbind(covset5,covset10,covset20,covset50,covset100)
drop <- c("Num.x","Num.y","SM1","SM2","SM3")
covset <- covset[,!(names(covset) %in% drop)]

### Export results
write.csv(covset,"data_with_cov.csv")
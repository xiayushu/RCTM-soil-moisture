### Install and import the following packages
# This step is best run on a virtual machine so that files in the Google Cloud bucket can be used directly
# Use gcsfuse or gcsfuse --implicit-dirs to import bucket folders
library(data.table)
library(rgdal)
library(raster)
library(caret)
library(randomForest)
library(quantregForest)

### Prepare site files and import model
# Make sure to set the correct path to read the file
# Set prediction depth, site, and time periods for prediction
depth = 7.5 
ROI = "Field" 
doy_frame <- c(as.Date("2002-01-01"), as.Date("2019-12-31")) 
dates <- seq.Date(doy_frame[1],doy_frame[2], 1)
# Import model derived from the regionalized or full quality-controlled model
#model_full <- readRDS("moisture_model_RAP_QA_15_5.rds") # Regional model
model_full <- readRDS("moisture_model_full_QA_15_5.rds") # Full model
# Define site parameters based on resolution and extents
res <- c(0.000269494585235856472, 0.000269494585235856472) # 30 meters
x <- raster(xmn = -110, xmx = -109, # Specify the spatial extent of the boundary
            ymn = 38, ymx = 39, 
            res=res, crs="+proj=longlat +datum=WGS84")
# Select files within the defined time frame
startingDate <- as.POSIXct(doy_frame[1], format="%Y-%m-%d")
endingDate <- as.POSIXct(doy_frame[2], format="%Y-%m-%d")
# Customize the pathways based on where the covariate files are stored
list_of_all_cov_files <- list.files(path = paste0("./",ROI,"/Dynamic_cov"), recursive = TRUE,pattern = "^cov.*tif", full.names = TRUE)
fileCovDates <- as.POSIXct(strptime(substr(list_of_all_cov_files,(nchar(ROI)+20),(nchar(ROI)+30)), format="%Y-%m-%d"))
list_of_selected_cov_files <- list.files(path = paste0("./",ROI,"/Dynamic_cov"), pattern = "^cov.*tif", full.names = TRUE)[fileCovDates >= startingDate & fileCovDates <= endingDate]
list_of_all_nldas_files <- list.files(path = paste0("./",ROI,"/Dynamic_cov"), recursive = TRUE, pattern = "^nldas.*tif", full.names = TRUE)
fileNldasDates <- as.POSIXct(strptime(substr(list_of_all_nldas_files,(nchar(ROI)+22),(nchar(ROI)+32)), format="%Y-%m-%d"))
list_of_selected_nldas_files <- list.files(path = paste0("./",ROI,"/Dynamic_cov"), pattern = "^nldas.*tif", full.names = TRUE)[fileNldasDates >= startingDate & fileNldasDates <= endingDate]
# Import constant covariates generated in GEE
predict.raster <- paste0("./",ROI,"/constant.tif")
raster <- new("GDALReadOnlyDataset", predict.raster)
width <- dim(raster)[2]
height <- dim(raster)[1]
imagedata <- data.frame(getRasterTable(raster))
names(imagedata) <- c('x', 'y', 'EL', 'SL', 'AS', 'TWI', 'surfrough', 'mcurv', 'hcurv', 'vcurv', 
                      'SOC5', 'BD5', 'Clay5', 'Sand5', 'SOC15', 'BD15', 'Clay15', 'Sand15', 'SOC30', 'BD30',
                      'Clay30', 'Sand30', 'SOC60', 'BD60', 'Clay60', 'Sand60', 'SOC100', 'BD100','Clay100','Sand100',
                      'landcover2001','landcover2004','landcover2006', 'landcover2008', 'landcover2011', 'landcover2013', 
                      'landcover2016', 'landcover2019', 'Depth')
imagedata$Depth <- depth
# Automatically extract covariate values corresponding to the defined predictive depth
imagedata$SOC <- imagedata$SOC5
imagedata$BD <- imagedata$BD5
imagedata$Clay <- imagedata$Clay5
imagedata$Sand <- imagedata$Sand5
if (depth > 10) {
  if (depth <= 22.5) {
    imagedata$SOC <- imagedata$SOC15
    imagedata$BD<- imagedata$BD15
    imagedata$Clay <- imagedata$Clay15
    imagedata$Sand <- imagedata$Sand15
  } 
  else if (depth <= 45) {
    imagedata$SOC <- imagedata$SOC30
    imagedata$BD<- imagedata$BD30
    imagedata$Clay <- imagedata$Clay30
    imagedata$Sand <- imagedata$Sand30
  } 
  else if (depth <= 80) {
    imagedata$SOC <- imagedata$SOC60
    imagedata$BD<- imagedata$BD60
    imagedata$Clay <- imagedata$Clay60
    imagedata$Sand <- imagedata$Sand60
  }  
  else{
    imagedata$SOC <- imagedata$SOC100
    imagedata$BD<- imagedata$BD100
    imagedata$Clay <- imagedata$Clay100
    imagedata$Sand <- imagedata$Sand100
  }
}
points <- SpatialPoints(imagedata[, c('x', 'y')], 
                        proj4string=CRS('+proj=longlat +datum=WGS84'))
pts <- spTransform(points, CRS("+proj=lcc +lat_1=60 +lat_2=25 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
pts_nldas <- spTransform(points, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

### Model run
# Use loop to automatically export predicted soil moisture for selected dates
for (i in 1:length(dates)){
  cov.raster <- stack(list_of_selected_cov_files[i])
  rasValue=extract(cov.raster, pts)
  colnames(rasValue) <- c("ppt","Tavg","VPD","GPP","EVI","NDWI","LST","Tree","TREE","AFGC","PFGC","SHR","BG","LTR")
  pts_cov <- cbind(imagedata,rasValue)
  nldas.raster <- stack(list_of_selected_nldas_files[i])
  nldasValue=extract(nldas.raster, pts_nldas)
  colnames(nldasValue) <- c("d5","d25","d70") 
  pts_all <- cbind(pts_cov,nldasValue)
  pts_all$SM <- pts_all$d5
  if (depth > 15) {
    if (depth <= 47.5) {
      pts_all$SM <- pts_all$d25/3
    }
    else{
      pts_all$SM <- pts_all$d70/6
    }
  }
  year = substr(dates[[1]],1,4)
  pts_all$LULC = pts_all$landcover2001
  if ((year == "2003")|(year == "2004")) {pts_all$LULC = pts_all$landcover2004}
  if ((year == "2005")|(year == "2006")) {pts_all$LULC = pts_all$landcover2006}
  if ((year == "2007")|(year == "2008")|(year == "2009")) {pts_all$LULC = pts_all$landcover2008}
  if ((year == "2010")|(year == "2011")) {pts_all$LULC = pts_all$landcover2011}
  if ((year == "2012")|(year == "2013")|(year == "2014")) {pts_all$LULC = pts_all$landcover2013}
  if ((year == "2015")|(year == "2016")|(year == "2017")) {pts_all$LULC = pts_all$landcover2016}
  if ((year == "2018")|(year == "2019")|(year == "2020")|(year == "2021")|(year == "2022")) {pts_all$LULC = pts_all$landcover2019}
  pts_all$LULC[pts_all$LULC == 21] <- 71
  pts_all$LULC[pts_all$LULC == 22] <- 31
  pts_all$LULC[pts_all$LULC == 23] <- 31
  pts_all$LULC[pts_all$LULC == 24] <- 31
  pts_all$LULC[pts_all$LULC == 90] <- 43
  pts_all$LULC[pts_all$LULC == 95] <- 71
  pts_all$landcover[pts_all$LULC==31] <- "Barren"
  pts_all$landcover[pts_all$LULC==41|pts_all$LULC==42|pts_all$LULC==43] <- "Forest"
  pts_all$landcover[pts_all$LULC==52] <- "Shrub"
  pts_all$landcover[pts_all$LULC==71|pts_all$LULC==72] <- "Grassland"
  pts_all$landcover[pts_all$LULC==81] <- "Pasture"
  pts_all$landcover[pts_all$LULC==82] <- "Crop"
  pts_all$SM[pts_all$SM > 60] <- 60
  pts_all$BD <- pts_all$BD/1000
  # pts_all <- pts_all[,c("x","y","landcover","EL","SL","AS","TWI","mcurv","hcurv","vcurv","surfrough",
  #                       "Clay","Sand","BD","SOC","SM","LST","GPP","EVI","NDWI","TREE","AFGC","PFGC","SHR","BG","LTR","ppt","Tavg","VPD","Depth")] # Regional model
  pts_all <- pts_all[,c("x","y","landcover","EL","SL","AS","TWI","mcurv","hcurv","vcurv","surfrough",
                        "Clay","Sand","BD","SOC","SM","LST","GPP","EVI","NDWI","Tree","ppt","Tavg","VPD","Depth")]
  pts_all <- na.omit(pts_all)
  pts_all$landcover <- as.factor(pts_all$landcover)
  levels(pts_all$landcover) <- c("Barren","Crop","Forest","Shrub","Grassland","Pasture")
  # Define model covariates
  # index_full <-names(pts_all) %in%  c("landcover", 
  #                                     "EL", "SL", "AS", "TWI", "mcurv", "hcurv", "vcurv", "surfrough",
  #                                     "Clay", "Sand", "BD", "SOC", 
  #                                     "SM",
  #                                     "LST", 
  #                                     "GPP", "EVI", "NDWI", 
  #                                     "TREE", "AFGC", "PFGC", "SHR", "BG", "LTR",
  #                                     "ppt", "Tavg", "VPD",
  #                                     "Depth") # Regional model
  index_full <-names(pts_all) %in%  c("landcover", 
                                      "EL", "SL", "AS", "TWI", "mcurv", "hcurv", "vcurv", "surfrough",
                                      "Clay", "Sand", "BD", "SOC", 
                                      "SM",
                                      "LST", 
                                      "GPP", "EVI", "NDWI", "Tree",
                                      "ppt", "Tavg", "VPD",
                                      "Depth")
  # Model prediction
  soil_pred_mean = predict(model_full, newdata = pts_all[, index_full], what = mean) 
  result <- cbind(pts_all[,c("x","y")],soil_pred_mean)
  filedate <- gsub("\\.","_",substr(names(cov.raster)[1],5,14))
  mois_map30 <- rasterize(result[, c('x', 'y')], x, result[, 'soil_pred_mean'], fun=mean, na.rm = TRUE)
  # Define pathway for eports; make sure to create the path in the Cloud Bucket first
  filepath <- paste0("./",ROI,"/",depth[[1]],"/SM_",depth[[1]],"_",filedate[[1]]) 
  writeRaster(mois_map30, filepath,format="GTiff",overwrite=TRUE)
  rm("cov.raster","nldas.raster","mois_map30","nldasValue","pts_all","pts_cov","rasValue","result","index_full","soil_pred_mean")
}
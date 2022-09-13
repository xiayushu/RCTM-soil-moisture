### Install and import the following packages
library(randomForest)
library(quantregForest)
library(caret)
library(tidyverse)

### Import and process full dataset refined with quality control flags
# Make sure to set the correct path to read the file
all <- read.table('data_with_cov_qa.csv', comment.char="", quote = "\"", header=T, sep=",")
all$landcover <- "Others"
all[all$LULC==31,]$landcover <- "Barren"
all[all$LULC==41|all$LULC==42|all$LULC==43,]$landcover <- "Forest"
all[all$LULC==52,]$landcover <- "Shrub"
all[all$LULC==71|all$LULC==72,]$landcover <- "Grassland"
all[all$LULC==81,]$landcover <- "Pasture"
all[all$LULC==82,]$landcover <- "Crop"
all$landcover <- as.factor(all$landcover)
all <- all[all$SM <=60,] 

#Carry out the spiking test on grassland sites
all <- all[which(all$landcover == "Grassland"),]
all$site_ID <- as.numeric(as.factor(with(all,paste(Site,Lat,Long))))
all$Year <- as.numeric(all$Water_year.x)
site <- unique(all[c("site_ID","Site","Network","State","Lat","Long")])

### Random 70:30 division of calibration and validation dataset based on site_ID
RanSample <- sample(all$site_ID, dim(site)[1]*.7, replace=FALSE, prob=NULL)
cali <- all[all$site_ID%in%RanSample,]
vali <- all[!all$site_ID%in%RanSample,]

### Set parameters for building the model 
fitControl = trainControl(method = "cv", number = 5)
ntree = 15

### Define covariates for modeling
index_site <-names(cali) %in%  c("EL", "SL", "AS", "TWI", "mcurv", "hcurv", "vcurv", "surfrough",
                                 "Clay", "Sand", "BD", "SOC", 
                                 "SM",
                                 "LST", 
                                 "GPP", "EVI", "NDWI","Tree",
                                 "ppt", "Tavg", "VPD",
                                 "Depth")

### Build model for different sites in a loop
# Initialize parameters
imp.cov <- data.frame()
preds.i <- data.frame()
Vali.i <- data.frame()
metrics <- data.frame()
rmse.cali.f = 0
rmse.vali.f = 0
r2.cali.f = 0
r2.vali.f = 0
mbe.cali.f = 0
mbe.vali.f = 0
# Run the main model
for (i in 1:length(unique(vali$site_ID))){
  all_i <- vali[which(vali$site_ID == unique(vali$site_ID)[i]),]
  year_i <- unique(all_i[c("Year","Site","Network","State","Lat","Long")])
  # Select the percentage of years for spiking; here an example is given for using 10% of the years for spiking/ local calibration
  RanSample_i <- sample(all_i$Year, dim(year_i)[1]*.1, replace=FALSE, prob=NULL) 
  cali_i <- rbind(cali,all_i[all_i$Year%in%RanSample_i,])
  vali_i <- all_i[!all_i$Year%in%RanSample_i,]
  qrf_site_i = caret::train(x = cali_i[,index_site],
                            y = cali_i$Moisture,
                            na.action = na.omit,
                            trControl = fitControl, 
                            metric = "RMSE",
                            ntree = ntree,
                            nodesize = 5,
                            method="qrf") 
  model_i <- qrf_site_i$finalModel
  imp <- importance(model_i)[,1]
  imp.cov <- rbind(imp.cov, imp)
  soil_cali_mean = predict(model_i, newdata = cali_i[, index_site], what = mean) 
  soil_vali_mean = predict(model_i, newdata = vali_i[, index_site], what = mean) 
  RMSE_cali_i <- sqrt(mean((soil_cali_mean - cali_i$Moisture)^2))
  r2_cali_i <- cor(soil_cali_mean, cali_i$Moisture)^2
  mbe_cali_i <- mean(soil_cali_mean - cali_i$Moisture)
  RMSE_vali_i <- sqrt(mean((vali_i[!is.na(soil_vali_mean),]$Moisture - soil_vali_mean[!is.na(soil_vali_mean)])^2, na.rm = T)) 
  r2_vali_i <- cor(vali_i[!is.na(soil_vali_mean),]$Moisture, soil_vali_mean[!is.na(soil_vali_mean)])^2
  mbe_vali_i <- mean(vali_i[!is.na(soil_vali_mean),]$Moisture - soil_vali_mean[!is.na(soil_vali_mean)])
  rmse.cali.f <- RMSE_cali_i + rmse.cali.f
  rmse.vali.f <- RMSE_vali_i + rmse.vali.f
  r2.cali.f <- r2_cali_i + r2.cali.f
  r2.vali.f <- r2_vali_i + r2.vali.f
  mbe.cali.f <- mbe_cali_i + mbe.cali.f
  mbe.vali.f <- mbe_vali_i + mbe.vali.f
  metrics <- rbind(metrics,cbind(i, r2_cali_i,RMSE_cali_i,mbe_cali_i,r2_vali_i,RMSE_vali_i,mbe_vali_i))
  mois_pred <- data.frame(soil_vali_mean)
  preds.i <- rbind(preds.i,cbind(i,mois_pred)) 
  mois_vali <- data.frame(vali_i$Moisture)
  Vali.i <- rbind(Vali.i,cbind(i,mois_vali))
  rm("all_i","model_i","cali_i","vali_i","qrf_site_i ")
}
# Check modeling results
r2.cali.f <- r2.cali.f/length(unique(vali$site_ID))
rmse.cali.f <- rmse.cali.f/length(unique(vali$site_ID))
mbe.cali.f <- mbe.cali.f/length(unique(vali$site_ID))
r2.cali.f
rmse.cali.f 
mbe.cali.f
r2.vali.f <- r2.vali.f/length(unique(vali$site_ID))
rmse.vali.f <- rmse.vali.f/length(unique(vali$site_ID))
mbe.vali.f <- mbe.vali.f/length(unique(vali$site_ID))
r2.vali.f
rmse.vali.f
mbe.vali.f
colnames(imp.cov) <- names(imp)
imp.cov <- sapply(imp.cov, mean)
data.frame(imp.cov)
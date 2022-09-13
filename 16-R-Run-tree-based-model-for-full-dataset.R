### Install and import the following packages
# This section is adapted from Huang's AHRSMM model on github: https://github.com/soilsensingmonitoring
# Citation for Huang's model: Huang, Jingyi, et al. "Retrieving Heterogeneous Surface Soil Moisture at 100 m Across the Globe via Fusion of Remote Sensing and Land Surface Parameters." Frontiers in Water (2020): 38.
# Our work differed from Huang's in terms of calibration dataset and environmental covariates
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
all <- all[all$SM <=60,] # Take out NLDAS outliers
all$site_ID <- as.numeric(as.factor(with(all,paste(Site,Lat,Long))))
obsnum <- all %>% group_by(site_ID) %>% summarise(obs_num = n())
site <- unique(all[c("site_ID","Site","Network","State","Lat","Long")])
site <- merge(site,obsnum,by="site_ID")

### Random 70:30 division of calibration and validation dataset based on site_ID
RanSample <- sample(site$site_ID, dim(site)[1]*.7, replace=FALSE, prob=NULL)
cali <- all[all$site_ID%in%RanSample,]
vali <- all[!all$site_ID%in%RanSample,]

### Set parameters for building the model 
# Test out different tree numbers to ensure efficiency and reduce over-fitting
fitControl = trainControl(method = "cv", number = 5)
ntree = 15

### Define covariates for modeling
names(cali)
index_full <-names(cali) %in%  c("landcover", 
                                 "EL", "SL", "AS", "TWI", "mcurv", "hcurv", "vcurv", "surfrough",
                                 "Clay", "Sand", "BD", "SOC", 
                                 "SM",
                                 "LST", 
                                 "GPP", "EVI", "NDWI", "Tree",
                                 "ppt", "Tavg", "VPD",
                                 "Depth")

### Build tree-based model
qrf_full = caret::train(x = cali[,index_full],
                        y = cali$Moisture,
                        na.action = na.omit,
                        trControl = fitControl, 
                        metric = "RMSE",
                        ntree = ntree,
                        nodesize = 5,
                        method="qrf")

### Generate variable importance for the calibration set
model_full <- qrf_full$finalModel
imp <- importance(model_full)[,1]
dotchart(sort(imp), xlab="Increase in node purity")

### Generate error metrics and plot model fit for the calibration set
soil_cali_mean = predict(model_full, newdata = cali[, index_full], what = mean) 
RMSE_cali <- sqrt(mean((soil_cali_mean - cali$Moisture)^2))
r2_cali <- cor(soil_cali_mean, cali$Moisture)^2
mbe_cali <- mean(soil_cali_mean - cali$Moisture)
RMSE_cali
r2_cali
mbe_cali
plot(cali$Moisture, soil_cali_mean,
     main = "Measured vs. predicted soil moisture of the calibration set", 
     cex.main = 0.95, cex = 0.5,
     xlab = "Measured moisture (%)",
     ylab = "Predicted moisture (%)",
     xlim = c(0,60),
     ylim = c(0,60))

### Generate error metrics and plot model fit for the validation set
soil_vali_mean = predict(model_full, newdata = vali[, index_full], what = mean) 
RMSE_vali <- sqrt(mean((vali[!is.na(soil_vali_mean),]$Moisture - soil_vali_mean[!is.na(soil_vali_mean)])^2, na.rm = T)) 
r2_vali <- cor(vali[!is.na(soil_vali_mean),]$Moisture, soil_vali_mean[!is.na(soil_vali_mean)])^2
mbe_vali <- mean(vali[!is.na(soil_vali_mean),]$Moisture - soil_vali_mean[!is.na(soil_vali_mean)])
RMSE_vali
r2_vali
mbe_vali
plot(vali$Moisture, soil_vali_mean,
     main = "Measured vs. predicted soil moisture of the validation set", 
     cex.main = 0.95, cex = 0.5,
     xlab = "Measured moisture (%)",
     ylab = "Predicted moisture (%)",
     xlim = c(0,60),
     ylim = c(0,60))

### Save the tree-based model for use in moisture prediction
saveRDS(model_full, "./moisture_model_full.rds")

### Save prediction results for further analysis
cali_full_results <- cbind(cali$Num,cali$Moisture, soil_cali_mean)
colnames(cali_full_results) <- c("Num","cali_obs_mois","cali_pred_mois")
write.csv(cali_full_results, "cali_full_results.csv", row.names = F) 
vali_full_results <- cbind(vali$Num,vali$Moisture, soil_vali_mean)
colnames(vali_full_results) <- c("Num","vali_obs_mois","vali_pred_mois")
write.csv(vali_full_results, "vali_full_results.csv", row.names = F) 
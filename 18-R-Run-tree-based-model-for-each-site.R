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

### Define model parameters and site-based model covariates
fitControl = trainControl(method = "cv", number = 5)
ntree = 15
index_site <-names(all) %in%  c("SM","LST","Depth","GPP","EVI","NDWI","Tree","SOC","BD","Sand","Clay","ppt","Tavg","VPD","LULC")

### Build site-based model in a loop
# Initialize the parameters
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
# Run models from different sites with a loop
for (i in 1:length(unique(all$site_ID))){
  all_i <- all[which(all$site_ID == i),]
  RanSample_i <- sample(all_i$Num, dim(all_i)[1]*.7, replace=FALSE, prob=NULL) 
  cali_i <- all_i[all_i$Num%in%RanSample_i,]
  vali_i <- all_i[!all_i$Num%in%RanSample_i,]
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
}
# Average and export modeling results
r2.cali.f <- r2.cali.f/length(unique(all$site_ID))
rmse.cali.f <- rmse.cali.f/length(unique(all$site_ID))
mbe.cali.f <- mbe.cali.f/length(unique(all$site_ID))
r2.cali.f
rmse.cali.f 
mbe.cali.f
r2.vali.f <- r2.vali.f/length(unique(all$site_ID))
rmse.vali.f <- rmse.vali.f/length(unique(all$site_ID))
mbe.vali.f <- mbe.vali.f/length(unique(all$site_ID))
r2.vali.f
rmse.vali.f
mbe.vali.f
# Make a chart for variable importance
colnames(imp.cov) <- names(imp)
imp.cov <- sapply(imp.cov, mean)
dotchart(sort(imp.cov), xlab="Increase in node purity")
metric <- merge(metrics, site, by.x = "i", by.y = "site_ID")
# Combine and plot validation results
vali.result <- cbind(preds.i,Vali.i)[c(1,2,4)]
colnames(vali.result) <- c("site_ID","Pred","Vali")
plot(vali.result$Vali, vali.result$Pred, 
     main = "Measured vs. predicted soil moisture of the validation set (all time/depth varying covariates)", cex.main = 0.95,
     xlab = "Measured moisture (%)",
     ylab = "Predicted moisture (%)",
     xlim = c(0,60),
     ylim = c(0,60),
     col = factor(vali.result$site_ID),
     cex = 0.5)
abline(coef = c(0,1))

### Export station/site based model performance
station <- unique(all[,c("Site","Lat","Long","EL","SL","AS","TWI","mcurv","hcurv","vcurv","surfrough")])
metric <- merge(metric, station, by = c("Site","Lat","Long"))
write.csv(metric, "Error_metrics_by_site.csv", row.names = F)
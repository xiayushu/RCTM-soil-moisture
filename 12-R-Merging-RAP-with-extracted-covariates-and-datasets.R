### Import and process RAP extracted for site-year combinations in GEE
# Make sure to set the correct path to read the files
rap <- read.table('RAP_site_year.csv', comment.char="", quote = "\"", header=T, sep=",")
siteyear <- read.table('Site_year.csv', comment.char="", quote = "\"", header=T, sep=",")
rap_site <- merge(rap, siteyear, by = c("Site","Num","Water_year"))
rap_site <- rap_site[,c("Site","Water_year","AFGC","BG","LTR","PFGC","SHR","TREE","Lat","Long")]

### Import already processed and merged moisture and covariate file
all <- read.table('data_with_cov.csv', comment.char="", quote = "\"", header=T, sep=",")

### Merge RAP dataset with the already processed file and take out repeated covariate (tree cover)
all <- merge(all,rap_site, by = c("Site","Water_year","Lat","Long"))
all <- all[,!(colnames(all) %in% c("Tree"))]

### Export results
write.csv(all,"data_with_RAP.csv")
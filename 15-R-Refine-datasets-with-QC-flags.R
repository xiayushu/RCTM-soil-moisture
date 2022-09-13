### Install and import the following packages
library(dplyr)

### Import and process files with quality control flags
# Make sure to set the correct path to read the files
gpp_qa <- read.table('gpp_qa_set.csv', comment.char="", quote = "\"", header=T, sep=",")
lst_qa <- read.table('lst_qa_set.csv', comment.char="", quote = "\"", header=T, sep=",")
sitedate <- read.table('Site_date.csv', comment.char="", quote = "\"", header=T, sep=",")
colnames(gpp_qa)[1:2] <- c("gpp_qa","Num")
colnames(lst_qa)[1:2] <- c("lst_qa","Num")
qa_merge1 <- merge(sitedate, gpp_qa, by = "Num", all = TRUE)
qa_merge2 <- merge(qa_merge1, lst_qa, by = "Num", all = TRUE)
qa1 <- qa_merge2 %>% 
  mutate(gpp_qa = ifelse(is.na(gpp_qa),0,1))
qa <- qa1 %>% 
  mutate(lst_qa = ifelse(is.na(lst_qa),0,1))
qa$qa <- with(qa, ifelse(gpp_qa + lst_qa >= 1, 1, 0)) 
rm("gpp_qa","lst_qa","qa_merge1","qa_merge2","qa1")

### Import the full dataset joined with environmental covariates
all <- read.table('data_with_cov.csv', comment.char="", quote = "\"", header=T, sep=",")
all <- all[all$SM <=60,] # Take out NLDAS outliers
all <- merge(all, qa, by = c("Site","State","Lat","Long","Date"))
all <- all[,!(colnames(all) %in% c("Num.y","Water_year.y"))]
subset1 <- all[which(all$qa == 0),]
rm("all","subset1")

### Import the Western US (RAP) dataset joined with environmental covariates
rap <- read.table('data_with_RAP.csv', comment.char="", quote = "\"", header=T, sep=",")
rap <- rap[rap$SM <=60,]
rap <- merge(rap, qa, by = c("Site","State","Lat","Long","Date"))
rap <- rap[,!(colnames(rap) %in% c("Num.y","Water_year.y"))]
subset2 <- rap[which(rap$qa == 0),]

### Export datasets refined with the quality control flags
write.csv(subset1,"data_with_cov_qa.csv")
write.csv(subset2,"data_with_RAP_qa.csv")
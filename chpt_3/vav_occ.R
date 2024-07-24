dev.off()
rm(list=ls())
cat("\014")

#----Wi-Fi AP and VAV Zone Association----

# Determine which Wi-Fi APs and devices are associated with each VAV zone

# Date: Wednesday 3 February 2021
# Author(s): Brodie W. Hobson

#----Load packages----

# Load required packages
if ("rstudioapi" %in% rownames(installed.packages()) == FALSE) {install.packages("rstudioapi")}
require(rstudioapi)

# Set working directory
setwd(dirname(getActiveDocumentContext()$path))

aps <- c("AP_1","AP_2","AP_3","AP_4")

for(i in aps){
  
  temp <- read.csv(file = paste0("./data/",i,".csv"))
  
  occ <- temp[,grep("occ",colnames(temp))]
  
  occ <- occ[,-1]
  
  colnames(occ) <- sub("X","",colnames(occ))
  
  nam <- i
  
  assign(nam,occ)
  
}
rm(occ,nam,i)

vav_occ <- cbind.data.frame(AP_1,AP_2,AP_3,AP_4)

vav_occ <- vav_occ[,order(names(vav_occ))]

vav_occ <- cbind.data.frame(temp[,1],vav_occ)

colnames(vav_occ)[1] <- "Timestamp"

#Specify date format used in .csv
date_format = "%Y-%m-%d %H:%M"

vav_occ[,1] <- as.POSIXct(vav_occ[,1], format = date_format, tz = "GMT")

vav_occ <- cbind.data.frame(vav_occ$Timestamp,
                                   vav_occ$`206_occ`+vav_occ$`207_occ`+vav_occ$`205_occ`,
                                   vav_occ$`201_occ`+vav_occ$`202_occ`+vav_occ$`203_occ`+vav_occ$`204_occ`,
                                   vav_occ$`112_occ`,
                                   vav_occ$`111_occ`/2,
                                   vav_occ$`111_occ`/2,
                                   vav_occ$`109_occ`,
                                   vav_occ$`107_occ`/2,
                                   vav_occ$`107_occ`/2,
                                   vav_occ$`301_occ`/3,
                                   vav_occ$`301_occ`/3,
                                   vav_occ$`301_occ`/3,
                                   vav_occ$`308_occ`+vav_occ$`309_occ`+vav_occ$`310_occ`+vav_occ$`311_occ`,
                                   vav_occ$`307_occ`,
                                   vav_occ$`303_occ`+vav_occ$`304_occ`+vav_occ$`305_occ`+vav_occ$`306_occ`,
                                   vav_occ$`101_occ`,
                                   vav_occ$`102_occ`+vav_occ$`103_occ`+vav_occ$`302_occ`,
                                   vav_occ$`104_occ`,
                                   vav_occ$`105_occ`/2,
                                   vav_occ$`105_occ`/2)

colnames(vav_occ) <- c("Timestamp",
                       "VAV_1",
                       "VAV_2",
                       "VAV_3",
                       "VAV_4",
                       "VAV_5",
                       "VAV_6",
                       "VAV_7",
                       "VAV_8",
                       "VAV_10",
                       "VAV_11",
                       "VAV_12",
                       "VAV_13",
                       "VAV_14",
                       "VAV_15",
                       "VAV_16",
                       "VAV_17",
                       "VAV_18",
                       "VAV_10",
                       "VAV_11")

#write.csv(vav_occ,"./data/vav_occ.csv",row.names = F)
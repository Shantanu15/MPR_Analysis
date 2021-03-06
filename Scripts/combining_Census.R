library(gdata)
setwd("/Users/Shantanu/Desktop/shantanu/Census/Original_Files/BIH/10-Bihar-PCA-2011")
file1000 <- read.xls("PCA1000_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1001 <- read.xls("PCA1001_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1002 <- read.xls("PCA1002_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1003 <- read.xls("PCA1003_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1004 <- read.xls("PCA1004_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1005 <- read.xls("PCA1005_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1006 <- read.xls("PCA1006_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1007 <- read.xls("PCA1007_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1008 <- read.xls("PCA1008_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1009 <- read.xls("PCA1009_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1010 <- read.xls("PCA1010_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1011 <- read.xls("PCA1011_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1012 <- read.xls("PCA1012_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1013 <- read.xls("PCA1013_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1014 <- read.xls("PCA1014_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1015 <- read.xls("PCA1015_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1016 <- read.xls("PCA1016_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1017 <- read.xls("PCA1017_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1018 <- read.xls("PCA1018_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1019 <- read.xls("PCA1019_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1020 <- read.xls("PCA1020_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1021 <- read.xls("PCA1021_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1022 <- read.xls("PCA1022_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1023 <- read.xls("PCA1023_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1024 <- read.xls("PCA1024_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1025 <- read.xls("PCA1025_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1026 <- read.xls("PCA1026_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1027 <- read.xls("PCA1027_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1028 <- read.xls("PCA1028_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1029 <- read.xls("PCA1029_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1030 <- read.xls("PCA1030_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1031 <- read.xls("PCA1031_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1032 <- read.xls("PCA1032_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1033 <- read.xls("PCA1033_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1034 <- read.xls("PCA1034_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1035 <- read.xls("PCA1035_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1036 <- read.xls("PCA1036_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1037 <- read.xls("PCA1037_2011_MDDS.xlsx", sheet=1,verbose = FALSE)
file1038 <- read.xls("PCA1038_2011_MDDS.xlsx", sheet=1,verbose = FALSE)

rbind1 <- rbind(file1038,file1037,file1036,file1035,file1034,file1033,file1032,file1031,file1030,file1029,file1028,file1027,file1026)
rbind2 <- rbind(rbind1,file1025,file1024,file1023,file1022,file1021,file1020,file1020,file1019,file1018,file1017,file1016,file1015,file1014)
rbind3 <- rbind (rbind2,file1013,file1012,file1011,file1010,file1009,file1008,file1007,file1006,file1005,file1004,file1003,file1002,file1001,file1000)
write.csv(rbind3,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Census/Bihar_Com_PCA.csv")
library(dplyr)
bihar_block <- filter(rbind3,Level == "SUB-DISTRICT", TRU == "Rural")
write.csv(bihar_block,"/Users/Shantanu/Documents/R_Experiments/MPR_Analysis_Github/Census/bihar_block.csv")

setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
source("LCV_Theme.R")
source("LCV_plotting.R")
source("LCV_Clustering.R")
source("LCV_Bayes.R")
source("SIIE2023.R")
library(forcats) # Recodificación de variables categóricas.

data <- doLoadData()
nrow(data) # 77
head(data)
colnames(data)

LCV_Orange_Theme()
LCV_histogram(data,variable="Year")

LCV_Lime_Theme()
LCV_ListOutliers(data,"s","all")
# Outliers: 867  883  1421  1111

data <- data[data$s < 1000,]
nrow(data) # 75

LCV_ListOutliers(data,"s","all")
# Outliers: 867  883  821 

LCV_Orange_Theme()
LCV_ClusterColumn(data, "s", nc = 8)
# Accuracy fit$betweenss/fit$totss=  0.9802671

LCV_Lime_Theme()
LCV_ListOutliers(data,"p","all")
# Outliers: 6

data <- data[data$p > 6,]
nrow(data) # 74


LCV_ListOutliers(data,"Grade","all")
# Outliers: 
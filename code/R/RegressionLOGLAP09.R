setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

library("NbClust")
library(forcats) # Recodificación de variables categóricas.
source("LCV_Theme.R")
source("LCV_Bayes.R")
source("LCV_plotting.R")
source("LCV_Clustering.R")
source("LCV_Regression.R")
source("SIIE2023.R")

data <- doLoadData()
data <- data[data$s < 1000,]
data <- data[data$p > 6,]
nrow(data)

LCV_Orange_Theme()
k <- LCV_ClusterColumn(data,"LOGLAP09")
k

LCV_ClusterColumn(data,"LOGLAP09",nc=5)
# Partition:  0 4.276666 7.860185 11.48007 28.98446  Accuracy fit$betweenss/fit$totss=  0.9673813

LCV_Heaven_Theme()
LCV_density(data, "LOGLAP09", showall = TRUE)

LCV_Lime_Theme()
LCV_ListOutliers(data,"LOGLAP09","all")
# Outliers: 29.45892  28.98446  32.23151
LCV_Heaven_Theme()
datawo <- data[data$LOGLAP09 < 28,]
LCV_LinearRegression(datawo,x="Grade",y="LOGLAP09",full=TRUE)

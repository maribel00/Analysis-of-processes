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
k <- LCV_ClusterColumn(data,"DAG")
k

LCV_ClusterColumn(data,"DAG",nc=5)
# 

LCV_Heaven_Theme()
LCV_density(data, "DAG", showall = TRUE)

LCV_Lime_Theme()
LCV_ListOutliers(data,"DAG","all")
# Outliers: 1.931  1.732  1.965  2.628 
# Outliers: 2.39768  2.16561  2.45609  3.28437
LCV_Heaven_Theme()
datawo <- data[data$DAG < 1.5,]
LCV_LinearRegression(datawo,x="Grade",y="DAG",full=TRUE)

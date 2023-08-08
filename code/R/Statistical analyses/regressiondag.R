setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE23master.R")

# data <- doLoadData()
# data <- data[data$s < 1000,]
# data <- data[data$p > 6,]
# nrow(data)

data <- readBaseDataset()
data <- data[data$Level == 10,]
head(data)
nrow(data)

LCV_Orange_Theme()
LCV_ClusterColumn(data,"Dag",nc=5)
# Partition:  0 4.276666 7.860185 11.48007 28.98446  Accuracy fit$betweenss/fit$totss=  0.9673813

LCV_Heaven_Theme()
LCV_density(data, "Dag", showall = TRUE)

LCV_Heaven_Theme()
datawo <- data[data$St < 28,]
LCV_LinearRegression(datawo,x="Dag",y="Grade",full=TRUE)

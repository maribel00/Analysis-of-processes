setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
library("NbClust")
library(forcats) # Recodificación de variables categóricas.
# install.packages("NbClust")
source("LCV_Theme.R")
source("LCV_plotting.R")
source("LCV_Bayes.R")
source("LCV_Clustering.R")
source("LCV_Regression.R")
source("SIIE2023.R")

data <- doLoadData()
data <- data[data$s < 1000,]
data <- data[data$p > 6,]
data$QuartileGrade <- as.character(data$QuartileGrade)

# Por clusters fijos de notas
LCV_Orange_Theme()
LCV_boxplot(data,"QuartileGrade","Grade")

LCV_histogram(data,"QuartileGrade")

bp<-LCV_boxplot(data, "QuartileGrade", "Grade")
bp_partition <- layer_data(bp)$ymin
bp_partition
# 6.99 8.23 8.95 9.60

LCV_Tomato_Theme()
LCV_densitiesFactor(data, "Grade", "QuartileGrade")

LCV_SelfValues(data, "Grade", xpartition = c(6.99,8.23,8.95,9.60))

# Por clusters dinámicos de notas
LCV_Orange_Theme()
LCV_ClusterColumn(data, "Grade")

LCV_ClusterColumn(data, "Grade", nc = 2)
# Partition:  6.99 8.79  Accuracy fit$betweenss/fit$totss=  0.6978412
LCV_ClusterColumn(data, "Grade", nc = 3)
# Partition:  6.99 8.4 9.35  Accuracy fit$betweenss/fit$totss=  0.8585982
LCV_ClusterColumn(data, "Grade", nc = 5)
# Partition:  6.46 7.34 8.1 8.79 9.44  Accuracy fit$betweenss/fit$totss=  0.9474439

# Por clusteres aproximados de rendimiento
LCV_Heaven_Theme()
data$fm <- data$p + data$fr + data$ps + data$sq + data$s

xpartition <- c(6.46, 7.34, 8.1, 8.79, 9.44)

res <- LCV_LinearRegression(data,x="Grade",y="fm",full=TRUE)
res <- res + geom_vline(xintercept = xpartition)
res

LCV_Orange_Theme()
LCV_ClusterColumn(data,"fm")
# Partition:  120.453 270.537 444.833 716.735  Accuracy fit$betweenss/fit$totss=  0.921137
# Outliers: 260.804  63.173  106.379  261.823

LCV_ClusterColumn(data,"fm",nc=7)
# Partition:  63.173 157.752 242.813 339.764 444.833 559.682 716.735  Accuracy fit$betweenss/fit$totss=  0.9772655
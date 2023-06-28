setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
library("NbClust")
library(forcats) # Recodificación de variables categóricas.
# install.packages("NbClust")
source("LCV_Theme.R")
source("LCV_plotting.R")
source("LCV_Bayes.R")
source("LCV_Clustering.R")
source("SIIE2023.R")

data <- read.delim2("SIIE23.tsv")
head(data)
data$QuartileGrade <- as.character(data$QuartileGrade)
data <- doLoadData()

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
# Partition:  6.99 8.1 9.12  Accuracy fit$betweenss/fit$totss=  0.8552877
LCV_ClusterColumn(data, "Grade", nc = 5)
# Partition:  6.46 7.34 8.1 8.79 9.44  Accuracy fit$betweenss/fit$totss=  0.9474439
setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
library("NbClust")
install.packages("NbClust")
source("LCV_Theme.R")
source("LCV_plotting.R")
source("LCV_Bayes.R")
source("LCV_Clustering.R")
source("SIIE2023.R")

data <- read.delim2("SIIE23.tsv")
head(data)
data$QuartileGrade <- as.character(data$QuartileGrade)
# data <- doLoadData()

LCV_Orange_Theme()
LCV_boxplot(data,"QuartileGrade","Grade")

LCV_histogram(data,"QuartileGrade")
data$QuartileGrade <- as.integer(data$QuartileGrade)
hist(data$QuartileGrade)

bp<-LCV_boxplot(data, "QuartileGrade", "Grade")
bp_partition <- layer_data(bp)$ymin
bp_partition

LCV_Tomato_Theme()
LCV_densitiesFactor(data, "Grade", "QuartileGrade")

LCV_SelfValues(data, "Grade", xpartition = c(6.99,8.23,8.95,9.60))
# KMEANS
LCV_ClusterColumn(data, "Grade")

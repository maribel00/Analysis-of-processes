setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("LCV_Theme.R")
source("LCV_Bayes.R")
source("SIIE2023.R")

rdataset <- doLoadData()
rdataset <- rdataset[rdataset$s < 1000,]
rdataset <- rdataset[rdataset$p > 6,]
nrow(rdataset)
head(rdataset)

LCV_Heaven_Theme()
LCV_density(rdataset, "ft", showall = TRUE)
LCV_density(rdataset, "all", showall = TRUE)
LCV_density(rdataset, "rt", showall = TRUE)
LCV_density(rdataset, "ps", showall = TRUE)
LCV_density(rdataset, "sq", showall = TRUE)
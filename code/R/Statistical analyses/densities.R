setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE2023.R")

data <- SIIE23doLoadData()
data <- data[data$s < 1000,]
data <- data[data$p > 6,]
nrow(data)
head(data)

LCV_Heaven_Theme()
LCV_density(data, "ft", showall = TRUE)
LCV_density(data, "rt", showall = TRUE)
LCV_density(data, "ps", showall = TRUE)
LCV_density(data, "all", showall = TRUE)
LCV_density(data, "sq", showall = TRUE)
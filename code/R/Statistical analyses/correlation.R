setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE23master.R")
library(patchwork)

# data <- readBaseDataset()
# data <- data[data$Level == 10,]

data <- SIIE23doLoadData()

data <- data[data$s < 1000,]
data <- data[data$p > 6,]
nrow(data)

LCV_Heaven_Theme()

nfields <- c("np","ns","ot","rt","st","ft","fr","ps","sq")
LCV_Mosaic <- LCV_LinearRegression(data,x=nfields[1],y="Grade",full=TRUE)
for (field in nfields[2:length(nfields)]){
  lplot <- LCV_LinearRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)

LCV_Mosaic <- LCV_PolynomialRegression(data,x=nfields[1],y="Grade",full=TRUE)
for (field in nfields[2:length(nfields)]){
  lplot <- LCV_PolynomialRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)

LCV_Mosaic <- LCV_ExponentialRegression(data,x=nfields[1],y="Grade",full=TRUE)
for (field in nfields[2:length(nfields)]){
  lplot <- LCV_ExponentialRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)

data <- readBaseDataset()
data <- data[data$Level == 10,]

LCV_Mosaic <- LCV_LinearRegression(data,x=topologicalmetrics()[1],y="Grade",full=TRUE)
for (field in topologicalmetrics()[2:length(topologicalmetrics())]){
  lplot <- LCV_LinearRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)

LCV_Mosaic <- LCV_PolynomialRegression(data,x=topologicalmetrics()[1],y="Grade",full=TRUE)
for (field in topologicalmetrics()[2:length(topologicalmetrics())]){
  lplot <- LCV_PolynomialRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)

LCV_Mosaic <- LCV_ExponentialRegression(data,x=topologicalmetrics()[1],y="Grade",full=TRUE)
for (field in topologicalmetrics()[2:length(topologicalmetrics())]){
  lplot <- LCV_ExponentialRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)

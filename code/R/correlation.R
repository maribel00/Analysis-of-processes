setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("LCV_Theme.R")
source("LCV_Regression.R")
source("SIIE2023.R")

data <- doLoadData()
head(data)

LCV_Heaven_Theme()

library(patchwork)
nfields <- c("np","ns","ot","rt","st","ft","fr","ps","sq")
LCV_Mosaic <- LCV_LinearRegression(data,x=nfields[1],y="Grade",full=TRUE)
for (field in nfields[2:length(nfields)]){
  lplot <- LCV_LinearRegression(data,x=field,y="Grade",full=TRUE)
  LCV_Mosaic <- LCV_Mosaic + lplot
}
LCV_Mosaic + plot_layout(nc=3)


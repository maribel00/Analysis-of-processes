library(C50)
library(forcats)
library(ggplot2)
library(caret)


source("SIIE23Scripts/LCV_Theme.R")
source("SIIE23Scripts/LCV_Bayes.R")
source("SIIE23Scripts/LCV_Clustering.R")
source("SIIE23Scripts/LCV_Graphs.R")
source("SIIE23Scripts/LCV_Hipothesis_Tests.R")
source("SIIE23Scripts/LCV_Regression.R")
source("SIIE23Scripts/LCV_plotting.R")
source("SIIE23Scripts/LCV_GraphMiner.R")
source("SIIE23Scripts/LCV_Dot.R")
source("SIIE23Scripts/SIIE2023.R")

downto=3
# Exactly thenrow(ds number of metrics to combine
minmetrics=4
# goal to learn
target= ""

# main<-function() {
#   # Levels from ground up to merge
#   downto=3
#   # Exactly thenrow(ds number of metrics to combine
#   minmetrics=4
#   # goal to learn
#   target= "BAD"
#   dsSIIE23RAW <- loadRAWDataset()
#   if (FALSE) {
#     GraphList <- GenerateAllGraphs()
#   } else {
#     GraphList <- readAllGraphs()
#   }
# 
#   if (TRUE) {
#     dsbase<-GenerateBaseDataset()
#   } else {
#     dsbase <-readBaseDataset()
#   }
# 
#   if (FALSE) {
#     GeneratePartitions(dsbase)
#   } else {
#     readPartitions()
#   }
# 
#   if (TRUE) {
#     TrainTree(target);
#   } else {
#     readTree()
#   }
#   # if (TRUE) {
#   #   applyTree(tree, level=10)
#   # }
#   if (FALSE) {
#     ByLevels(result[1,"prob"], target = target)
#   }
# 
# 
# }




loadRAWDataset<-function() {
  return (SIIE23doLoadSessions())
}

GenerateAllGraphs<-function() {

  return (data.frame())
}

readAllGraphs<-function() {
  return (SIIE23doLoadGraphs(dsSIIE23RAW))
}


GenerateBaseDataset<-function() {
  cat("Press to generate all datasets for each level from ",downto,"to",MAXPROBLEMS )
  #\\readline()
    SIIE23doGenerateDatasets(downto)
  res <- data.frame(dsList[MAXPROBLEMS])
  i<-MAXPROBLEMS-1
  while (i >= downto) {
    cat("Level ...", i,"\n");
    res <- rbind(res,data.frame(dsList[i]))
    i<-i-1;
  }
  # saveRDS(res,paste(pwd,"dsbase.RDS",sep=""))
  QuartileMatrix <- matrix(0,4,7)
  colnames(QuartileMatrix) <- unique(c(SIIE23RAW$Year))
  rownames(QuartileMatrix) <- seq(1:4)
  return (res)
}

readBaseDataset<-function() {
  return (readRDS(paste(pwd,"dsbase.RDS",sep="")))
}

GeneratePartitions<-function(data) {
  dsbase <<- SIIE23SplitDataset(data, 70);
  dstrain <<- dsbase[dsbase$split==FALSE,]
  dstest <<- dsbase[dsbase$split==TRUE,]
  saveRDS(dsbase,paste(pwd,"dsbase.RDS",sep=""))
  saveRDS(dstrain,paste(pwd,"dstrain.RDS",sep=""))
  saveRDS(dstest,paste(pwd,"dstest.RDS",sep=""))
}

readPartitions<-function() {
  dstrain <<- readRDS(paste(pwd,"dstrain.RDS",sep=""))
  dstest  <<- readRDS(paste(pwd,"dstest.RDS",sep=""))
}

TrainTree<-function(target) {
  result <<- SIIE23SearchBestFit(c(),setdiff(allmetricsF(),c("s")),target)

}

readTree<-function() {
  combo 		<<- 	readRDS(paste(pwd,"combo_",target,".RDS",sep=""))
  besttree 	<<- 	readRDS(paste(pwd,"besttree_",target,".RDS",sep=""))
  best_cm	<<- 	readRDS(aste(pwd,"bestcm_",target,".RDS",sep=""))
  bes 		<<- 	readRDS(paste(pwd,"best_",target,".RDS",sep=""))
  result 	<<-   readRDS(paste(pwd,"result_",target,".RDS",sep=""))
}

# main()











# library(C50)
# library(forcats)
# library(ggplot2)
# library(caret)
#
#
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Theme.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Bayes.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Clustering.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Graphs.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Hipothesis_Tests.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Regression.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_plotting.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_GraphMiner.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Dot.R")
# source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/SIIE2023.R")
#
#
#
# allmetrics=c( allmetricsF()) #c("ns","nt","np","ot","st","rt","ft","ps","fr","sq")
# mastermetrics=c(allmetrics)
# # mastermetrics<-allKmastermetrics
# allcombinationsmetrics =  do.call("c", lapply(seq_along(allmetrics), function(i) combn(allmetrics, i, FUN = list)))
#
# # Levels from ground up to merge
#
# dto<-3
#
# # Exactly thenrow(ds number of metrics to combine
# minmetrics=4
#
# if (TRUE) {
#   GenerateAllGraphs()
# } else {
#   readAllGraphs()
# }
#
# if (TRUE) {
#   GenerateBaseDataset()
# } else {
#   readBaseDataset()
# }
#
# if (TRUE) {
#   GeneratePartitions()
# } else {
#   readPartitions()
# }
#
#
# if (RTUE){ # file.exists(paste(pwd,"/Graphs/Ex1/dsbase95.RDS",sep=""))) {
#
#   SIIE23doInitDatasets()
#   dsbase=readRDS(paste(pwd,"/Graphs/Ex1/dsbase95.RDS",sep=""))
#   dstrain=readRDS(paste(pwd,"/Graphs/Ex1/dstrain95.RDS",sep=""))
#   dstest=readRDS(paste(pwd,"/Graphs/Ex1/dstest95.RDS",sep=""))
#
#   # dsbase = SIIE23SplitDataset(dsbase, 70);
#   # dstrain = dsbase[dsbase$split==FALSE,]
#   # dstest = dsbase[dsbase$split==TRUE,]
#
#   # dstrain=readRDS(paste(pwd,"dstrain.RDS",sep=""))
#   # dstest=readRDS(paste(pwd,"dstest.RDS",sep=""))
# }else {
#   groptions <- graphoptions("SIIE23")
#   dsSIIE23ORIGINAL=data.frame()
#   dsSIIE23FULL=data.frame()
#   dsSIIE23RAW=data.frame()
#   dsSIIE23P=data.frame()
#   C50Tree=c()
#   dsList =list()
#   dsbase= data.frame()
#   dstest= data.frame()
#   dstrain= data.frame()
#   GraphList = list()
#   SIIE23doInitDatasets()
#   SIIE23doTraining(dto)
# }
# # # metrixCombo <- c( 310,364,370,373,412,418,468,479,484 )
# # goodmetricsx4<-c( 470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,766,812,867,889,942,1021,1022,1048,1052,1054,1056,1057,1063,1071,1076,1079,1088,1096,1097,1112,1119,1122,1304,1357,1359,1397 )
# #
# # # goodmetricsx4<-c( 234,252,267,279,280,286,288,293,302,309,311,312,322,325,328,331,334,340,357,359,364,365,371,373,385,405,406,409,412,415,419,468,471,485,492,508,539 )
# # # goodmetricsx4<- c( 794,799,824,827,892,933,936,957,960,974,988,991,992,996,997,1030,1125,1126,1127,1130,1139,1141,1146,1147,1151,1166,1167,1181,1188,1193,1201,1207,1213,1215,1222,1225,1228,1230,1234,1240,1247,1251,1262,1297,1311,1320,1322,1328,1329,1336,1339,1343,1344,1345,1347,1351,1354,1360,1366,1374,1375,1379,1387,1392,1399,1403,1405,1406,1409,1413,1417,1421,1450,1459,1470,1471,1481,1484,1487,1493,1501,1522,1524,1529,1546,1549,1552,1562,1567,1579,1582,1584 )
# # goodmetricsx5<-c( 386,387,388,389,390,391,392,393,395,396,397,398,399,402,403,405,410,411,413,421,424,425,426,427,428,429,430,431,432,434,439,448,449,450,451,453,455,460,468,470,472,473,475,482,484,485,486,498,500,501,505,506,529,534,539,547,551,552,554,555,556,557,559,568,570,571,573,574,575,576,579,606,618,620,622,623,625,626,633,634,635,636 )
# # goodmetricsx6<-c( 638,639,640,641,642,643,644,645,646,647,649,650,651,652,nrow(dsbase)653,655,656,658,659,661,662,664,667,668,669,670,671,672,673,674,675,677,679,680,681,682,684,685,691,694,696,697,700,702,709,712,713,714,715,716,717,719,720,721,722,723,724,725,727,729,730,731,732,734,735,736,737,738,739,740,741,742,749,752,758,759,761,762,765,768,770,772,774,776,781,784,786,791,796,798,800,802,803,804,805,806,808,809,810,812,813,814,816,817,818,824,836,839,840,844,845 )
# # metrixCombo <- seq(1:length(allcombinationsmetrics))
# target<-"PERFORMANCE"
#
#
# result <- SIIE23SearchBestFit(c(),setdiff(allmetricsF(),c("s","p")),target)
# ByLevels(result[1,"prob"], target = target)


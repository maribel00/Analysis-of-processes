library(C50)
library(forcats)
library(ggplot2)
library(caret)

pwd<<-"."
source("SIIE23Scripts/LCV_Theme.R")
source("SIIE23Scripts/LCV_Bayes.R")
source("SIIE23Scripts/LCV_Clustering.R")
source("SIIE23Scripts/LCV_Graphs.R")
source("SIIE23Scripts/LCV_Hipothesis_Tests.R")
source("SIIE23Scripts/LCV_Regression.R")
source("SIIE23Scripts/LCV_plotting.R")
source("SIIE23Scripts/LCV_GraphMiner.R")
source("SIIE23Scripts/LCV_Dot.R")

MAXPROBLEMS<-10
METRICTIME<-FALSE
REQUIREMINCLOSURE<-FALSE

SIIE23BaseGraph <- function(name) {
  res <- dot(name,TRUE)
  columns <- c("A","p1_f","p1_s","p2_f","p2_s","p3_f","p3_s","p4_f","p4_s","p5_f","p5_s","p6_f","p6_s","p7_f","p7_s","p8_f","p8_s","p9_f","p9_s")
  l <- length(columns)
  m<-matrix(0,l,l)
  colnames(m)<-columns
  rownames(m)<-columns
  res$adjacency<-m
  return (res)
}

SIIE23AddGraph <- function(dot1,dot2) {
  res <- dot1
  for (c in colnames(dot2$adjacency)) {
    for (r in rownames(dot2$adjacency)) {
      res$adjacency[r,c] <- res$adjacency[r,c]+dot2$adjacency[r,c]
    }
  }
  return (res)
}

SIIE23AddAllGraphs <- function(name, graphnames=c(),level,glist, percentile=0.95) {
  res<-SIIE23BaseGraph(name)
  for (n in graphnames) {
    cat(n,"\n")
    res<-SIIE23AddGraph(res,glist[[n]][[level]])
  }
  v <- c(res$adjacency)
  v <- v[-which(v==0)]
  cut <- quantile(v,percentile)
  for (c in colnames(res$adjacency)) {
    for (r in rownames(res$adjacency)) {
      if (res$adjacency[r,c]>cut){
        res$adjacency[r,c] <- 0
      }
    }
  }
  return (res)
}


SIIE23doLoadSessions<-function(){
  SIIE23RAW <<- read.delim2("~/Descargas/SIIE23RAW.tsv")
}

SIIE23doLoadGraphs<-function(dataset){
  GraphList= list()
  for (g in allGroupsF(dataset)) {
    sel <- dataset[dataset$Group==g,"PERFORMANCE"]
    cat("Loading group |",g, "| grades ", sel, sep="")
    # if (length(sel)<1 & sel[1] =="LOW"){
    #   perf <- "LOW"
    # }
    # else{
    #   perf<-"GOOD"
    # }
    perf<-"*"
    GraphList[[g]] = list()
    for (i in (3:10)) {
      cat("Level ", i, " ")
      gfilename<- paste(pwd,"/Graphs/",gsub(" ","_",g),"_",i,"_LOW",".RDS", sep="")
      if (!file.exists(gfilename))
        gfilename<- paste(pwd,"/Graphs/",gsub(" ","_",g),"_",i,"_GOOD",".RDS", sep="")
      cat(gfilename, "\n")
      GraphList[[g]][[paste("L",i,sep="")]] <- readRDS(gfilename)
    }
    cat("\n")

    # #\\readline("Press")
  }
  return (GraphList)
}

SIIE23doInitDatasets<-function() {
  dsSIIE23ORIGINAL=data.frame()
  dsSIIE23FULL=data.frame()
  dsSIIE23RAW=data.frame()
  dsSIIE23P=data.frame()
  C50Tree=c()
  dsList =list()
  # allmetrics=c("ns","np","ot","st","rt","ft","ps","fr","sq")
  # mastermetrics=c("NDAG","NLAP",allmetrics)
  mastermetrics=c("ns","nt","np")
  allcombinationsmetrics =  do.call("c", lapply(seq_along(mastermetrics), function(i) combn(mastermetrics, i, FUN = list)))
}

SIIE23doImportXtraData<-function(data, fulldataset, nproblems=MAXPROBLEMS){
  slap=paste("LOGLAP0",nproblems,sep="")
  sdap=paste("DAG0",nproblems,sep="")
  data$Group <- fulldataset$Group
  data$NDAG <- (fulldataset[[sdap]]-min(fulldataset[[sdap]]))/(max(fulldataset[[sdap]])-min(fulldataset[[sdap]]))
  data$NLAP <- (fulldataset[[slap]]-min(fulldataset[[slap]]))/(max(fulldataset[[slap]])-min(fulldataset[[slap]]))
  data$KMEANS_Grade <- fulldataset$Size
  data$Level <- nproblems
  for (i in (1:nrow(data))) {
    if (data[i,"Group"] %in%  dsSIIE23FULL$Group) {
      data[i,"KMEANS_Grade"] <-dsSIIE23FULL[dsSIIE23FULL$Group == data[i,"Group"],"KMEANS_Grade"]
    } else{
      # cluster = which.min(as.numeric(dsKMEANS$medvalue)-data[i,"Grade"])
      # data[i,"KMEANS_Grade"]<-dsKMEANS[cluster,"clustername"]
      data[i,"KMEANS_Grade"]<-"XXX"
    }
  }
  data$BAD <- (data$QuartileGrade ==1)
  data$PERFORMANCE <- ifelse (data$Grade<8.1, "LOW", "GOOD")

  return (data)
}


SIIE23doLoadData<-function(){
  #\\readline("Press INTRO to LOAD DATA AND PREPROCESS")
  data <- read.delim2("~/Descargas/SIIE23.tsv")
  cat("OK loaded ",nrow(data), " rows")
  #\\readline("Press INTRO to Preprocess data (logarithmic adjustements)")

  for (sfield  in c("LAP01", "LAP02", "LAP03", "LAP04", "LAP05", "LAP06", "LAP07", "LAP08", "LAP09", "LAP10")) {
    data[paste("LOG",sfield, sep = "")] <- log(data[[sfield]])
    data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
  }

  data$m<-data$s

  benefits <- c("ns","np", "sq","ps")
  costs <- c("ot","st","rt","ft","fr")

  for (goal in benefits) {
    data$m <- data$m + data[[goal]]
  }
  for (cost in costs) {
    data$m <- data$m + (1- data[[cost]])
  }
  data$m <- data$m / (length(benefits)+length(costs))
  data$ng <- data$Grade/10
  data$all<-TRUE

  return(data)
}

SIIE23doPrepareScales<-function (data, nproblems=MAXPROBLEMS){

  #\\readline("Press INTRO to Preprocess data (logarithmic adjustements")
  if("NDAG" %in% allfieldsF()) {
    for (sfield  in c("LAP01", "LAP02", "LAP03", "LAP04", "LAP05", "LAP06", "LAP07", "LAP08", "LAP09", "LAP10")) {
      data[paste("LOG",sfield, sep = "")] <- log(data[[sfield]])
      data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
    }

    for (sfield  in c("FLAP1", "FLAP2", "FLAP3", "FLAP4", "FLAP5", "FLAP6", "FLAP7", "FLAP8", "FLAP9")) {
      data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
    }
    slap=paste("LOGLAP0",nproblems,sep="")
    data$NDAG<-(data$DAG-min(data$DAG))/(max(data$DAG)-min(data$DAG))
    data$NLAP<-(data[[slap]]-min(data[[slap]]))/(max(data[[slap]])-min(data[[slap]]))
    # data$NLAP<-(data$LOGLAP09-min(data$LOGLAP09))/(max(data$LOGLAP09)-min(data$LOGLAP09))

  }
  data$fm<-data$s
  data$fmn<-data$s

  benefits <- c("ns","np", "sq","ps")
  costs <- c("ot","st","rt","ft", "fr")

  for (goal in benefits) {
    data$fm <- data$fm + data[[goal]]
  }
  for (cost in costs) {
    data$fm <- data$fm + (1- data[[cost]])
  }

  benefits <- c("ns","np", "sq","ps")
  costs <- c("fr")

  for (goal in benefits) {
    data$fmn <- data$fmn + data[[goal]]
  }
  for (cost in costs) {
    data$fmn <- data$fmn + (1- data[[cost]])
  }

  data$fmn <- data$fmn / (length(benefits)+length(costs))
  data$ng <- data$Grade/10
  return(data)
}


getC50Error<-function(c50tree) {
  lines <- strsplit(c50tree$output, "\n")
  line <- grep("<<",lines[[1]])
  # cat("found ", lines[[1]][line])
  sfinal <- lines[[1]][line]
  return (as.numeric(strsplit(strsplit(sfinal,"\\(")[[1]][2],"\\%")[[1]][1]))
}

#
# Train
#

SIIE23doTraining <- function(downto=MAXPROBLEMS, mtraining=70,mtest=30) {
  SIIE23doInitDatasets()
  dsSIIE23RAW <<-SIIE23doLoadSessions()
  GraphList <<- SIIE23doLoadGraphs(dsSIIE23RAW)
  ## XTRA FILTERS


  cat("Press to generate all datasets for each level from ",downto,"to",MAXPROBLEMS )
  #\\readline()
  SIIE23doGenerateDatasets(downto)
  dsbase <<- data.frame(dsList[MAXPROBLEMS])
  i<-MAXPROBLEMS-1
  while (i >= downto) {
    dsbase <<- rbind(dsbase,data.frame(dsList[i]))
    i<-i-1;
  }
  # dsbase<<-dsbase[dsbase$KMEANS_Grade != "XXX",]

  dsbase <<- SIIE23SplitDataset(dsbase, 70);
  dstrain <<- dsbase[dsbase$split==FALSE,]
  dstest <<- dsbase[dsbase$split==TRUE,]


  # percenttest<-20
  # ntest <- round(nrow(dsbase)*percenttest/100,0)



  #Iterative group sampling

  # By level
  # dstrain<<- dsbase[dsbase$Level!=8,]
  # dstest<<- dsbase[dsbase$Level==8,]

  # KIntervals<-5
  # for (m in (allmetricsF())) {
  #   if (unique(dsbase[[m]]) >KIntervals){
  #   dsbase<<-LCV_ClusterColumn(dsbase,m,nc=KIntervals)
  #   } else {
  #     dsbase[[paste("KMEANS_",m,sep="")]]<- as.factor(dsbase[[m]])
  #         }
  # }
  # Remove outliers

  # #\\#\\readline("Press to generate training and tests t ")
  # dsbase$split <<- runif(nrow(dsbase),0,100)
  # dstrain<<- dsbase[dsbase$split<=percentTraining,]
  # dstest<<- dsbase[dsbase$split>percentTraining,]
  # cat("Base dataset consists of ",nrow(dsbase), "rows, ", nrow(dstrain), "for training and ", nrow(dstest), "for test\n")
  saveRDS(dsbase,paste(pwd,"dsbase.RDS",sep=""))
  saveRDS(dstrain, paste(pwd,"dstrain.RDS",sep=""))
  saveRDS(dstest,paste(pwd,"dstest.RDS",sep=""))
  return(0)
}

SIIE23SplitDataset<-function(data, percenttraining) {
  # dsbase = SIIE23SplitDatasetAlpha(dsbase, percenttraining)
  data = SIIE23SplitDatasetBeta(data, percenttraining)
  # SIIE23SplitDatasesttreetGamma(dsbase, percenttraining)
  return (data)
}



SIIE23SplitDatasetAlpha<-function(dsbase, percenttraining) {
  dsbase$split=FALSE
  # dsbase$split <<- runif(nrow(dsbase),0,100)
  dsbase$split = (runif(nrow(dsbase),0,100)>percenttraining)
  return (dsbase)
}


SIIE23SplitDatasetBeta<-function(dsbase, percenttraining) {
  dsbase$split=FALSE
  lowlevel=min(dsbase$Level)
  highlevel=max(dsbase$Level)
  mlevel=lowlevel
  ntraining <- round(nrow(dsbase)*percenttraining/100,0)
  ntest<-round(nrow(dsbase)*(100-percenttraining)/100,0)
  rows <- seq(1:nrow(dsbase))
  while (ntest>0) {
    element <- sample(rows)[1]
    while (dsbase[element,"Level"] != mlevel) {
      element <- sample(rows)[1]
    }
    dsbase[element,"split"]=TRUE
    mlevel <- mlevel+1
    ntest<- ntest-1
    rows <- setdiff(rows,c(element))
    if (mlevel >highlevel)
      mlevel <- lowlevel
  }
  return (dsbase)
}

SIIE23SplitDatasetGamma<-function(dsbase, percenttraining) {
  dsbase$split<<-FALSE
  lowlevel=min(dsbase$Level)
  highlevel=max(dsbase$Level)
  mlevel=lowlevel
  ntraining <- round(nrow(dsbase)*percenttraining/100,0)
  ntest<-round(nrow(dsbase)*(100-percenttraining)/100,0)
  group <- sample(allGroupsF(dsbase))[1]
  while (ntest>0) {
    ntest<- ntest-1
    tentativegroup <- group
    while (dsbase[dsbase$Level == mlevel & dsbase$Group==tentativegroup, "split"]== TRUE) {
      tentativegroup <- sample(allGroupsF(dsbase))[1]
    }
    dsbase[dsbase$Level == mlevel & dsbase$Group==tentativegroup, "split"]<<-TRUE
    mlevel <- mlevel+1
    if (mlevel >highlevel)
      mlevel <- lowlevel
  }
}

SIIE23doGenerateDatasets<-function(downto=MAXPROBLEMS) {
  outliers <- c()
  dstop<-data.frame()
  for (np in (MAXPROBLEMS:downto)) {
    cat("\nGenerating data frame at level ", np,":")
    # ds <- myDataset(dsSIIE23RAW, np)
    ds <- myDataset(SIIE23RAW, np)
    ds$OUTLIER <- "NO"
    if (np == MAXPROBLEMS) {
      #\\readline(paste("Removing outliers level ",np,"from ", nrow(ds),"rows \n"))
      ds[ds$s>=1000,"OUTLIER"]<-"TOO.MANY.SESSIONS"
      ds[ds$p<=6,"OUTLIER"]<-"TOO.FEW.PROBLEMS"
      outliers<- union(outliers,ds[ds$s>=1000,"Group"])
      outliers<- union(outliers,ds[ds$p<=6,"Group"])
      ds <- ds[!ds$Group  %in% outliers,]
      cat(paste("Removing outliers level ",np,"to ", nrow(ds),"rows \n"))
      #\\readline("Clustering grades to 5 groups")
      ds<-LCV_ClusterColumn(ds,"Grade",nc=5)
      dstop<-data.frame(ds)
    } else {
      ds <- ds[!ds$Group  %in% outliers,]
      for (xtrafield  in setdiff(colnames(dstop),colnames(ds))) {
        ds[[xtrafield]] <- dstop[[xtrafield]]
      }
    }
    dsList[[np]]<<- ds
    cat(" with ",nrow(dsList[[np]]), "rows")
  }
}
linesFrom<-function(compactstring,watchstring) {
  watch<-FALSE
  result <-""
  compactstring<-cat(compactstring,sep="\n")
  lines <- strsplit(compactstring,"\n")
  for (i in(1:length(lines))) {
    if (!watch)
      watch <- grep(watchstring,lines[i])
    if (watch)
      result<-paste(result, lines[i],sep="\n")
  }
  return(result)
}



SIIE23doBestC50Fit<-function(data,metrics=mastermetrics, target="KMEANS_Grade", size=3,downto=MAXPROBLEMS,seqmetrix=seq(1:length(allcombinationsmetrics))) {
  result <- data.frame(metric=character(), prob=numeric(), target=character())
  iresult<-0
  if (length(metrics)==0) {
    cat("Searching the best combination of metrics in ", nrow(data),"rows\n")
    # preping the dataset
    goodmetrics <- c()
    # newds<- newds[newds$KMEANS_Grade != "XXX",]
    # cat("From ",downto,"-",9,"=",nrow(newds),"rows\n")
    for (icomb in seqmetrix) {
      thismetric <- allcombinationsmetrics[[icomb]]
      if (length(thismetric)==size) {
        cat(icomb,",")
        if (icomb %%40 == 0)
          cat("\n");
        # cat(icomb,"_", thismetric,"\n")
        # thistree <- C5.0(x=dsSIIE23FULL[,thismetric],y = as.factor(dsSIIE23FULL[[target]]), trials = 100,rules=FALSE)
        thistree <- C5.0(x=data[,thismetric],y = as.factor(data[,target]), trials = 100,rules=FALSE)
        thiserror <- getC50Error(thistree)
        if (thiserror == 0) {
          cat("\n\nFound a valid tree for ", thismetric," with ", nrow(data),"rows and ", ncol(data),"cols\n\n")
          iresult <- iresult +1
          # cres <- SIIE23doC50Fit(dstest,thistree)
          # mcover = cres["coverage"]
          # mprob <- cres["prob"]
          mprob <- 0
          result[iresult,] <- c(prob=mprob, metric=paste(thismetric, collapse=";"),target=target)
          goodmetrics <- c(goodmetrics, icomb)
        }
        rm(thistree)
        gc()
      }
    }
  } else {
    result[1,] <- c(prob=1, metric=paste(metrics, collapse=";"),target=target)
  }

  cat("\n\nFound ",length(result), " combination of metrics\n\n")
  cat("goodmetrics<-c(",paste(goodmetrics, collapse=","),")\n")
  saveRDS(result,paste(pwd,"result.RDS",sep=""))
  show(result)
  return(result)
}

miniSIIE23_BAD<-function(mobservable,mspectral) {
  mymetrics <- c(mobservable,mspectral)
  # dsbase$BAD<<-as.factor(dsbase$Grade <8.1)
  # dstrain$BAD<<-dstrain[dstrain$Grade <8.1
  # dstest$BAD<<-as.factor(dstest$Grade <8.1)
  variables<-dstrain[,mymetrics]
  goal <<- as.factor(dstrain$BAD)
  mitree<<-C5.0(x=variables,y=goal,trials=100,rules=TRUE)
  cat("rules",mitree$size[length(mitree$size)], "error",getC50Error(mitree))
  # SIIE23doC50Fit(dstest,mitree)
  mipredict<<-predict(mitree,dstest)
  confm<<-confusionMatrix(mipredict,dstest$BAD,positive="TRUE")
  show(confm$table)
  cat("Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")
  for (level in (3:MAXPROBLEMS)){
    cat("Level ",level,"\n")
    ds<- dsbase[dsbase$Level==level,]
    mipredict<<-predict(mitree,ds)
    confm<<-confusionMatrix(mipredict,ds$BAD,positive="TRUE")
    cat("   Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")

  }
}

miniSIIE23_KGRADE<-function(mobservable,mspectral) {
  mymetrics <- c(mobservable,mspectral)
  variables<-dstrain[,mymetrics]
  goal <<- as.factor(dstrain$KMEANS_Grade)
  mitree<<-C5.0(x=variables,y=goal,trials=100,rules=TRUE)
  cat("rules",mitree$size[length(mitree$size)], "error",getC50Error(mitree),"\n")
  mipredict<<-predict(mitree,dstest)
  confm<-confusionMatrix(as.factor(mipredict),as.factor(dstest$KMEANS_Grade))
  show(confm$table)
  cat("Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")
  # show(summary(as.factor(dstest$KMEANS_Grade)))
  # show(summary(mipredict))
  # show(SIIE23doC50Fit(dstest,mitree))
  # confm<<-confusionMatrix(mipredict,dstest$BAD,positive="TRUE")
  # show(confm$table)
  # cat("Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")
  for (level in (3:MAXPROBLEMS)){
    cat("Level ",level,"\n")
    ds<- dsbase[dsbase$Level==level,]
    # show(SIIE23doC50Fit(ds,mitree, doecho = FALSE))
    mipredict<<-predict(mitree,ds)
    # confm<<-confusionMatrix(mipredict,ds$KMEANS_Grada,positive="[7.34,7.96]")
    # cat("   Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")
    # confm<<-confusionMatrix(mipredict,ds$KMEANS_Grada,positive="[6.46,6.99]")
    # cat("   Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")

  }
}


SIIE23Accuracy<-function(result) {
  # data<- do.call(cbind.data.frame, mlist)
  cat("Metric","\tError", "Cover","Prob","min Level", "max level\n",sep="\t\t")
  bestprob<-0
  best <-c()
  bestcm<-c()
  for (i in (1:nrow(result))) {
    metric <- result[i,]
    mymetrics <- c(strsplit(metric[1,"prob"],";"))
    mytarget <- metric[1,"target"]
    variables<-mymetrics[[1]]
    minLevel <- min(dstest$Level)
    maxLevel <- max(dstest$Level)
    cx<-dstrain[,variables]
    cy<-as.factor(dstrain[[mytarget]])
    mitree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
    cat("rules",mitree$size[length(mitree$size)], "error",getC50Error(mitree),"\n")
    mipredict<<-predict(mitree,dstest)
    confm<-confusionMatrix(as.factor(mipredict),as.factor(dstest[[mytarget]]))
    show(confm$table)
    cat("Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")
    mprob<-confm$overall["Accuracy"]
    if (mprob>bestprob) {
      bestprob<-mprob
      best<-variables
      bestcm <- confm
      bestm<-metric
      besttree<<-mitree
    }
  }
  saveRDS(variables,paste(pwd,"variables.RDS",sep=""))
  saveRDS(besttree ,paste(pwd,"besttree.RDS",sep=""))
  saveRDS(bestcm ,paste(pwd,"bestcm.RDS",sep=""))
  saveRDS(best ,paste(pwd,"best.RDS",sep=""))
  saveRDS(metric ,paste(pwd,"metric.RDS",sep=""))
  cat("\n\nBEST: \n",best)
  show(bestcm$table)
  cat("Accuracy ",bestcm$overall["Accuracy"],"p",bestcm$overall["AccuracyPValue"],"\n")

  return (metric)
}

ByLevels <-function(metric, target="KMEANS_Grade") {
  bestree=readRDS(paste(pwd,"besttree_",target,".RDS",sep=""))
  bestcm=readRDS(paste(pwd,"bestcm_",target,".RDS",sep=""))
  mymetrics <- c(strsplit("prob",";"))
  variables<-mymetrics[[1]]

  minLevel <- min(dsbase$Level)
  maxLevel <- max(dsbase$Level)
  cat("Deep validation by levels\n")
  cat("   Obtaining normal decision tree\n")
  cat("   Applying normal tree level by level\n")
  # cat("Metric\tLevel\tProblems\tSessions\tCover\tProb\n")
  # dds<-dsbase
  # ns = sum(dds$s)
  # np = median(dds$p)
  # mymetrics <- c(strsplit(metric,";"))
  # variables<-mymetrics[[1]]
  minLevel <- min(dsbase$Level)
  maxLevel <- max(dsbase$Level)
  # cx<-dsbase[,variables]
  # cy<-as.factor(dsbase$KMEANS_Grade)
  # mitree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
  # cat("------------------------------------------")
  # cat("rules",mitree$size[length(mitree$size)], "error",getC50Error(mitree),"\n")
  # mipredict<<-predict(mitree,dstest)
  # confm<-confusionMatrix(as.factor(mipredict),as.factor(dstest$KMEANS_Grade))
  # show(confm$table)
  # cat("Levels ",minLevel,"to",maxLevel,ns,np,"Accuracy ",cm[["overall"]]["Accuracy"],"p",cm[["overall"]]["AccuracyPValue"],"\n")
  cat("------------------------------------------")

  for (ilevel in(minLevel:maxLevel)) {
    dds<-data.frame(dsbase[dsbase$Level==ilevel,])
    ns = sum(dds[["s"]])
    np = median(dds[["p"]])
    mipredict<<-predict(besttree,dds)
    confm<-confusionMatrix(as.factor(mipredict),as.factor(dds[[target]]))
    show(confm$table)
    cat("Level",ilevel,ns,np,"Accuracy ",confm$overall["Accuracy"],"p",confm$overall["AccuracyPValue"],"\n")
  }

}


AccuracyMetrics2<-function(result,bylevels=FALSE) {
  # data<- do.call(cbind.data.frame, mlist)
  cat("Metric","\tError", "Cover","Prob","min Level", "max level\n",sep="\t\t")
  bestprob<-0
  best <-c()
  for (i in (1:nrow(result))) {
    metric <- result[i,]
    mymetrics <- c(strsplit(metric[1,"prob"],";"))
    variables<-mymetrics[[1]]
    minLevel <- min(dstest$Level)
    maxLevel <- max(dstest$Level)
    cx<-dstrain[,variables]
    cy<-as.factor(dstrain$KMEANS_Grade)
    thistree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
    thispredictor<-predict(thistree,dstest)
    thsiscfm1<-confusionMatrix(pred,dstest$KMEANS_Grade,positive="[7.34,7.96]")
    mcover = as.numeric(cres["coverage"])
    mprob <- as.numeric(cres["prob"])
    strvar<-metric[1,"prob"]
    crow<-c(strvar,thiserror,mcover,mprob,minLevel,maxLevel)
    if(mcover == 100) {
      if (mprob>bestprob) {
        bestprob<-mprob
        best<-crow
      }
      cat(crow,"\n",sep="\t\t")
    }
  }
  cat("\n\nBEST: \n",best)
  # if (bylevels) {
  #   metric <- best[1]
  #   mymetrics <- c(strsplit(metric,";"))
  #   variables<-mymetrics[[1]]
  #   minLevel <- min(dstest$Level)
  #   maxLevel <- max(dstest$Level)
  #   cat("Deep validation by levels\n")
  #   cat("   Obtaining normal decision tree\n")
  #   cx<-dstest[,variables]
  #   cy<-as.factor(dstest$KMEANS_Grade)
  #   thistree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
  #   thiserror <- getC50Error(thistree)
  #   # cat(linesFrom(summary(thistree),"classified as"))
  #   cat("   Applying normal tree level by level\n")
  #   cat("Level","p", "s","Cover","Prob","\n",sep="\t\t")
  #   for (ilevel in(minLevel:maxLevel)) {
  #     dstest<-data[data$KMEANS_Grade!="XXX" & data$Level==ilevel,]
  #     ns = sum(dstest$s)
  #     np = median(dstest$p)
  #     cres <- SIIE23doC50Fit(dstest,thistree)
  #     mcover = as.numeric(cres["coverage"])
  #     mprob <- as.numeric(cres["prob"])
  #     cat(ilevel,np,ns,mcover,mprob,"\n",sep="\t\t")
  #   }
  # }
}

FindBestTree<-function(spectral=c(), basic=c(), target="", level=MAXPROBLEMS) {
  nspectral<-length(spectral)
  nbasic=length(basic)-nspectral
  allcombinations =  do.call("c", lapply(seq_along(basic), function(i) combn(basic, i, FUN = list)))
  besterror<-1000
  besttree<-0
  bestaccuracy<-0
  bestpvalue<-0
  bestcm<-0
  bestcombo<-c()
  for (minos in (0:length(basic))) {
    change<-FALSE
    for(combo in allcombinations) {
      cat ("-->",combo,"\n")
      if (length(combo) == nbasic) {
        combo <- c(combo,spectral)
        cat("Trying ",combo,"\n")
        cx <- dstrain[,combo]
        cy<-as.factor(dstrain[[target]])
        cat("Calling C50\n")
        tree<-C5.0(cx,cy,trials=100,rules=FALSE ,
                   control = C5.0Control(
                     # subset = TRUE, bands = 0, winnow = FALSE,
                     # noGlobalPruning = FALSE, CF = .95,
                     minCases = 4,
                     # sample = 0,
                     # seed = sample.int(4096, size = 1) - 1L,
                     # earlyStopping = FALSE, label = "outcome"
                   ))
        if (getC50Error(tree)<besterror) {
          cat("Error acceptable");
          besterror <- getC50Error(tree)
          cat("   Found candidate ", combo,"\n")
          miprediction <-predict(tree,dstest)
          cm <- confusionMatrix(miprediction,as.factor(dstest[[target]]))
          if(cm$overall["Accuracy"]> bestaccuracy) {
            besttree <<- tree
            bestcm <- cm
            bestaccuracy<-cm$overall["Accuracy"]
            bestpvalue<-cm$overall["AccuracyPValue"]
            bestcombo <- combo
            change <- TRUE
          }
        }
      }
    }
  }
  cat("BEST ",bestcombo,"-->",target,"\n")
  cat("Error:",besterror,"\n")
  show(cm$table)
  cat("Accuracy ",bestaccuracy, " pvalue",bestpvalue,"\n")
  result <- data.frame(metric=character(), prob=numeric(), target=character())
  result[1,] <- c(prob=bestaccuracy, metric=paste(bestcombo, collapse=";"),target=target)

  saveRDS(bestcombo,paste(pwd,"combo_",target,".RDS",sep=""))
  saveRDS(besttree ,paste(pwd,"besttree_",target,".RDS",sep=""))
  saveRDS(bestcm ,paste(pwd,"bestcm_",target,".RDS",sep=""))
  saveRDS(best ,paste(pwd,"best_",target,".RDS",sep=""))
  saveRDS(result,paste(pwd,"result_",target,".RDS",sep=""))
  show(result)
  return(result)
}



SIIE23SearchBestFit<-function(spectral=c(), basic=c(), target="", level=MAXPROBLEMS) {
  nspectral<-length(spectral)
  nbasic=length(basic)-nspectral
  allcombinations =  do.call("c", lapply(seq_along(basic), function(i) combn(basic, i, FUN = list)))
  besterror<-1000
  besttree<-0
  bestaccuracy<-0
  bestpvalue<-0
  bestcm<-0
  bestcombo<-c()
  for (minos in (0:length(basic))) {
    change<-FALSE
    for(combo in allcombinations) {
      cat ("-->",combo,"\n")
      if (length(combo) == nbasic) {
        combo <- c(combo,spectral)
        cat("Trying ",combo,"\n")
        cx <- dstrain[,combo]
        cy<-as.factor(dstrain[[target]])
        cat("Calling C50\n")
        tree<-C5.0(cx,cy,trials=100,rules=FALSE ,
                   control = C5.0Control(
                     # subset = TRUE, bands = 0, winnow = FALSE,
                     # noGlobalPruning = FALSE, CF = .95,
                     minCases = 4,
                     # sample = 0,
                     # seed = sample.int(4096, size = 1) - 1L,
                     # earlyStopping = FALSE, label = "outcome"
                   ))
        if (getC50Error(tree)<besterror) {
          cat("Error acceptable");
          besterror <- getC50Error(tree)
          cat("   Found candidate ", combo,"\n")
          miprediction <-predict(tree,dstest)
          cm <- confusionMatrix(miprediction,as.factor(dstest[[target]]))
          if(cm$overall["Accuracy"]> bestaccuracy) {
            besttree <<- tree
            bestcm <- cm
            bestaccuracy<-cm$overall["Accuracy"]
            bestpvalue<-cm$overall["AccuracyPValue"]
            bestcombo <- combo
            change <- TRUE
          }
        }
      }
    }
  }
  cat("BEST ",bestcombo,"-->",target,"\n")
  cat("Error:",besterror,"\n")
  show(cm$table)
  cat("Accuracy ",bestaccuracy, " pvalue",bestpvalue,"\n")
  result <- data.frame(metric=character(), prob=numeric(), target=character())
  result[1,] <- c(prob=bestaccuracy, metric=paste(bestcombo, collapse=";"),target=target)

  saveRDS(bestcombo,paste(pwd,"combo_",target,".RDS",sep=""))
  saveRDS(besttree ,paste(pwd,"besttree_",target,".RDS",sep=""))
  saveRDS(bestcm ,paste(pwd,"bestcm_",target,".RDS",sep=""))
  saveRDS(best ,paste(pwd,"best_",target,".RDS",sep=""))
  saveRDS(result,paste(pwd,"result_",target,".RDS",sep=""))
  show(result)
  return(result)
}

SIIE23doC50Fit<-function(data,C50Tree, doecho=FALSE) {
  target<-"KMEANS_Grade"
  goods<- 0
  pgoods<-1
  probs <- c()
  level<-median(data$Level)
  if (doecho)  cat("Processing ",nrow(data), "rows\n")

  for (i in (1:nrow(data))) {
    # cat("Level",level,"row",i,"/",nrow(data),"\n")
    row <- data[i,]
    grade <- data[i,"Grade"]
    matrix<-predict(C50Tree,row,type="prob")
    # matrix<-localm
    ic <- which.max(matrix[1,])
    prob<-max(matrix[1,])
    probs <- c(probs, prob)
    id <- colnames(matrix)[which.max(matrix)]
    lmin <- as.numeric(strsplit(strsplit(id,",")[[1]][1], "\\[")[[1]][2])
    lmax <- as.numeric(strsplit(strsplit(id,",")[[1]][2], "\\]")[[1]][1])
    stringrow<-""
    if (lmin <= grade & grade <= lmax) {
      goods <- goods+1
      pgoods<-pgoods+prob
      pres<-"+"
    } else {
      pres<-"-"
    }
    # stringrow<- paste(stringrow,row[

    if (doecho) cat(data[i,"Group"], "with grade", grade, "has been classified within ", lmin, "-",lmax, "=")
    if (lmin <= grade & grade <= lmax) {
      if (doecho) cat(" +++++ (",prob,")")
    }
    if (doecho)    cat("\n")
    # data$KMEANS_Grade_min[i],"-",data$KMEANS_Grade_max[i]," with probability", max(matrix[1,(1:4)]),"\n")
  }
  # value<-c(coverage=round(goods/nrow(data)*100,1),prob=pgoods/goods)
  value<-c(coverage=round(goods/nrow(data)*100,3),prob=round(median(probs),3), N=nrow(data), Levels=paste(unique(data$Level),collapse=","))
  rm(data)
  return (value);
}


probProblem <- function(data, group, problem, numproblems=MAXPROBLEMS, nsessions=10000) {
  ldata<-subdataset(data,group,nproblems,nsessions)
  return (nrow(ldata[ldata$Group==group & ldata$Problem == problem,]))
}


probOutcome <- function(data, group, outcome, numproblems=MAXPROBLEMS, nsessions=10000) {
  ldata<-subdataset(data,group,nproblems,nsessions)
  return (sp(data,group,problem)/s(data,group))
}

maxS<-function(data,group, nproblems=MAXPROBLEMS) {
  year<-subdataset(data,group,nproblems)[1,"Year"]
  return(max(data[data$Year==year,"nID"]))
}

allmetricsF<-function() {
  return (c("s", "p", "np", "fr", "ps", "sq", "Cl","De", "Dm" ,"Le", "Di", "We", "Ef", "St","Dag", "WDag", "Be", "Ba"))
}

topologicalmetrics<-function() {
  return (c("s", "p", "Cl", "De", "Dm" ,"Le", "Di", "We", "Ef", "St","Dag", "WDag", "Be", "Ba"))
}

classicalmetrics<-function() {
  return (c("s", "p", "ns", "np", "ot", "st", "rt", "ft", "ps", "fr", "sq"))
}

allfieldsF<-function() {
  return (c(inheritedfieldsF(),allmetricsF(),classicalmetrics()))
}

inheritedfieldsF<-function() {
  return (c("Group",	"Year","Grade", "QuartileGrade","Size"))
}

allGroupsF<-function(data,performance="") {
  if (performance=="")
  return  (unique(data[,"Group"]))
  else if (performance == "LOW")
    return  (unique(data[data$Grade < 8.1, "Group"]))
  else if (performance == "GOOD")
    return  (unique(data[data$Grade >= 8.1, "Group"]))
}

allProblemsF<-function() {
  return (c("p1","p2","p3","p4","p5","p6","p7","p8","p9"))
}

myProblems<-function(data, group, nproblems=MAXPROBLEMS, nsessions=10000) {
  ldata<-subdataset(data,group,nproblems,nsessions)
  return  (unique(ldata[,"Problem"]))
}

mySolvedProblems<-function(data, group, nproblems=MAXPROBLEMS, nsessions=10000) {
  ldata<-subdataset(data,group,nproblems,nsessions)
  # ldata<-ldata[endsWith(ldata$Problem,"_s"),]
  ldata<-ldata[endsWith(ldata$Composite,"solved"),]
  return  (unique(ldata[,"Problem"]))
}

myLastSolvedProblem<-function(data, group, nproblems=MAXPROBLEMS, nsessions=10000) {
  sp <- mySolvedProblems(data,group,nproblems,nsessions)
  return (sp[length(sp)])
}


isSolved<-function(row) {
  return (row["OutCome"]=="solved")
}


isFailed<-function(row) {
  return (row["OutCome"]=="fail")
}

isSolvedProblem<-function(data, group, problem) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$OutCome=="solved" & ldata$Problem==problem,]
  return (nrow(ldata)>0)
}


subdataset<-function(data,group, nproblems=MAXPROBLEMS, nsessions=10000) {
  # return (data[data$Group==group & data$psolved <=nproblems & data$nID < nsessions,])
  predata <-data[data$Group==group,]
  realmax <- max(predata$psolved)
  if ( realmax >=nproblems)
    return (data[data$Group==group & data$psolved <=nproblems & data$nID < nsessions,])
  else
    return (data[data$Group==group & data$psolved <=realmax & data$nID < nsessions,])
}

sp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  # ldata<-subdataset(data,group,nproblems)
  return (nrow(data[data$Group==group & data$Problem==problem & data$psolved <=nproblems,]))
}

presp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  # ldata<-subdataset(data,group,nproblems)
  return (nrow(data[data$Group==group & data$Problem==problem & data$psolved <=nproblems &
                      data$nID >= otp(data,group,problem)  &
                      data$nID <= stp(data,group,problem)
                    ,]))
}


postsp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  # ldata<-subdataset(data,group,nproblems)
  return (nrow(data[data$Group==group & data$Problem==problem & data$psolved <=nproblems &
                      data$nID > stp(data,group,problem)
                    ,]))
}



s<-function(data,group,nproblems=MAXPROBLEMS, graphlist=c()) {
  ldata<-subdataset(data,group,nproblems)
  return (nrow(ldata))
}

ng<-function(data,group,nproblems=MAXPROBLEMS) {
  return (data[data$Group == group,][1,"Grade"]/10)
}

ns<-function(data,group,nproblems=MAXPROBLEMS, graphlist) {
  return (s(data,group,nproblems)/maxS(data,group))
}

Cx<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  ldata <- gpreProcess(ldata,groptions)
  graph <- GraphMinerCore(ldata,group,nproblems, groptions)
  saveRDS(graph,paste(pwd,"/Graphs/",gsub(" ","_",group),"_",nproblems,"_",ifelse(ldata[1,"Grade"]<8.1,"LOW","GOOD"),".RDS",sep=""))
  graph$name<-paste(graph$name,"_",ifelse(ldata[1,"Grade"]<8.1,"LOW","GOOD"),sep="")
  dotExport(graph,paste(pwd,"/Graphs/",sep=""))
  # graph<-readRDS(paste(pwd,"/Graphs/",gsub(" ","_",group),"_",nproblems,"_",ifelse(ldata[1,"Grade"]<8.1,"LOW","GOOD"),".RDS",sep=""))
  # nnodes <- ncol(dotgetMatrix(graph))
  # lastproblem<-paste(myLastSolvedProblem(ldata,group,nproblems),groptions$leafid,sep="")
  # lp <- c()
  # for (p in (mySolvedProblems(ldata,group,nproblems))) {
  #   lp <- append(lp, length(dotgetAllDepths(graph,paste(p,"_s",sep=""))))
  # }
  # allpaths<-dotgetAllDepths(graph,lastproblem)
  # return (log(mean(lp))) #/(nnodes^(nnodes-2)))
  # allFreqs<-dotgetAllFreqs(graph,lastproblem)
  return (0)
}


testColumn <- function(data,col, nproblems=9) {
  maxg<-3
  lowers <- sample(allGroupsF(data,"LOW"))
  highers <- sample(allGroupsF(data,"GOOD"))
  # df<- data.frame()
  lowy<-c()
  toppy<-c()
  allwy<-data.frame(Group=character(), Perf=character(), fun=numeric(), KMG=character())
  for (g in lowers) {
    value <- data[data$Level==nproblems, col]
    # cat ("LOW ", g, value , "\n")
    lowy <- append(lowy,c(g=value))
    # allwy[nrow(allwy)+1,] <- c(g,"LOW",as.numeric(value), dsSIIE23RAW[dsSIIE23RAW$Group==g,"KMEANS_Grade"][1])
  }
  cat("\n")
  for (g in highers) {
    value <- data[data$Level==nproblems, col]
    # cat ("GOOD ", g, value , "\n")
    toppy <- append(toppy, c(g=value))
    # allwy[nrow(allwy)+1,] <- c(g,"GOOD",as.numeric(value), dsSIIE23RAW[dsSIIE23RAW$Group==g,"KMEANS_Grade"][1])
  }
  cat ("LOWY\n")
  show(summary(lowy))
  cat ("TOPPY\n")
  show(summary(toppy))
  # allwy$Perf <- as.factor(allwy$Perf)
  # allwy$all <- 1
  # LCV_boxplot(allwy,"Perf","fun")
}



testGraphFunction <- function(data, fun) {
  maxg<-3
  lowers <- sample(allGroupsF(data,"LOW"))
  highers <- sample(allGroupsF(data,"GOOD"))
  # df<- data.frame()
  lowy<-c()
  toppy<-c()
  allwy<-data.frame(Group=character(), Perf=character(), fun=numeric(), KMG=character())
  for (g in lowers) {
    value <- do.call(fun,list(data, g, 9, GraphList))
    # cat ("LOW ", g, value , "\n")
    lowy <- append(lowy,c(g=value))
    # allwy[nrow(allwy)+1,] <- c(g,"LOW",as.numeric(value), dsSIIE23RAW[dsSIIE23RAW$Group==g,"KMEANS_Grade"][1])
  }
  cat("\n")
  for (g in highers) {
    value <- do.call(fun,list(data, g, 9, GraphList))
    # cat ("GOOD ", g, value , "\n")
    toppy <- append(toppy, c(g=value))
    # allwy[nrow(allwy)+1,] <- c(g,"GOOD",as.numeric(value), dsSIIE23RAW[dsSIIE23RAW$Group==g,"KMEANS_Grade"][1])
  }
  cat ("LOWY\n")
  show(summary(lowy))
  cat ("TOPPY\n")
  show(summary(toppy))
  # allwy$Perf <- as.factor(allwy$Perf)
  # allwy$all <- 1
  # LCV_boxplot(allwy,"Perf","fun")
}

De<-function(data, group, nproblems, graphlist) {
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (gDe(m))
  # return (We(data,group, nproblems, graphlist) *gE(m) / (nrow(m)*(nrow(m)-1)))
}

gDe<-function(m) {
  return (gE(m)/gEmax(m))
}

Dm<-function(data, group, nproblems, graphlist) {
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (gDm(m))
}

gDm<-function(m) {
  return (gE(m)/gN(m))
}



Le<-function(data, group, nproblems, graphlist) {
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
 return (log(gLe(m)))
 }


gLe<-function(m) {
  m<-gAdj(m)
  m<- gMinClosure(m)
  vm<-c(m)
  zvm<-vm[which(vm>0)]
  return (mean(zvm)/(gE(m)*(gE(m)-1)))
}

Di<-function(data, group, nproblems, graphlist) {
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (gDi(m))
}

gDi<-function(m) {
  m<- gAdj(m)
  m<- gMinClosure(m)
  vm<-c(m)
  zvm<-vm[which(vm>0)]
  return (max(zvm)/(gE(m)*(gE(m)-1)))  # return (max(m)/sum(m))
}

Co<-function(data, group, nproblems, graphlist) {
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
   return (gCo(m))  # return (gAvrgDeg(m)/gEmax(m))
}

Coi<-function(m,i) {
  m <- gAdj(m)
  return (sum(m[,i])+sum(m[i,]))
}

gCo<-function(m) {
  v<-c()
  for (i in (1:nrow(m))) {
    v <- append(v, Coi(m,i))
  }
  return (mean(v))  # return (gAvrgDeg(m)/gEmax(m))
}

Cl<-function(data, group, nproblems, graphlist) {
  if (nproblems >=3) {
    graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
    m <- dotgetMatrix(graph)
    return (gCl(m))
  }  else
    return (0);
}

gCl<-function(m) {
    v<-c()
    for (i in (1:nrow(m))) {
      k <- length(ki(m,i))
      c<- e(m,c(i,k))
      v <- append(v, 2*c/((k*(k-1))))
    }
  return (mean(v))  # return (gAvrgDeg(m)/gEmax(m))
}


ki<-function(m,i) {
  return (c(which(m[i,]>0),which(m[,i]>0),i))
}

e<-function(m,c=c()) {
  n<-0
  for (i in c) {
    n <- n +
      length( (c(intersect(which(m[i,]>0),c),intersect(c,which(m[,i]>0)))))
  }
  return (n)
}

Cli<-function(m,i) {
  k<-length(ki(m,i))
  e<-length(e(m,k))
  return (2*e/(k*(k-1)))
}

# gCl <- function(m) {
#   v <-c()
#   for (i in (1:nrow(m))) {
#     v <- append(v, Cli(m,i))
#   }
#   return (mean(v))
# }

We<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (sum(m)/maxS(data,group,nproblems))
  # return (sum(m)/maxS(data,group,nproblems))

  # return(graph_NSessions(data,group,nproblems,graphlist))
}

Ef<-function(data, group, nproblems, graphlist) {
  return(graph_NProblemsSolved(data,group,nproblems,graphlist))
  # return(graph_NProblemsSolved(data,group,nproblems,graphlist)/nproblems)
}

St<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
   return (log(gLaplacianFactor(m)))
  return (log(gLaplacianFactor(m))/(log(gE(m)^(gE(m)-1))))
  # return (log(gLaplacianFactor(m))/((n-1)/log(n)))
}

Be<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  # if (REQUIREMINCLOSURE)
  #   m<- gMinClosure(m)
  problem <-substr(colnames(m)[which.max(gBetweennes(m))],2,2)
  return (problem) #(substr(problem,2,1))
}

# De<-function(data, group, nproblems, graphlist) {
#   ldata<-subdataset(data,group,nproblems)
#   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
#   return (gDAG(graph))
# }


WDag<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  npaths <-0
  n <- 0
  for (node in (1:nrow(m))) {
    if (sum(m[node,])==0) { # Leaf
      ad <- dotgetAllDepths(graph,node)
      npaths <- npaths + sum(ad)
      n<-n+1
    }
  }
  # return(npaths)
  return(log(npaths/(sum(m)*gEmax(m))))
}


Dag<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  npaths <-0
  n <- 0
  for (node in (1:nrow(m))) {
    if (sum(m[node,])==0) { # Leaf
      ad <- dotgetAllDepths(graph,node)
      npaths <- npaths + length(ad)
      n<-n+length(ad)
    }
  }
  # return(npaths)
  return(log(npaths/(n*gEmax(m))))
}

Ba<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  sd <- sd(m[!is.na(m)])
  mean <- mean(m[!is.na(m)])
  
  return (sd/mean)
}

# -----------------
# gDe<-function(graph) {
#     m <- dotgetMatrix(graph)
#     return (We(data,group, nproblems, graphlist) *gE(m) / (nrow(m)*(nrow(m)-1)))
#   }
#
# gDm<-function(graph) {
#   m <- dotgetMatrix(graph)
#   return (gWAvrgDeg(m)/maxS(data,group,nproblems))
#   # return(graph_WAvrgDeg(data,group,nproblems,graphlist))
# }
#
#
# gLe<-function(graph) {
#   m <- dotgetMatrix(graph)
#   m<- gMinClosure(m)
#   return (mean(m)/sum(m))
# }
#
# gDi<-function(graph) {
#   m <- dotgetMatrix(graph)
#   m<- gMinClosure(m)
#   return (max(m)/sum(m))
# }
#
# gCo<-function(graph) {
#   m <- dotgetMatrix(graph)
#   return (gAvrgDeg(m)/gEmax(m))
# }
#
# We<-function(data, group, nproblems, graphlist) {
#   ldata<-subdataset(data,group,nproblems)
#   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
#   m <- dotgetMatrix(graph)
#   return (sum(m)/maxS(data,group,nproblems))
#
#   # return(graph_NSessions(data,group,nproblems,graphlist))
# }
#
# Ef<-function(data, group, nproblems, graphlist) {
#   return(graph_NProblemsSolved(data,group,nproblems,graphlist)/nproblems)
# }
#
# St<-function(data, group, nproblems, graphlist) {
#   ldata<-subdataset(data,group,nproblems)
#   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
#   m <- dotgetMatrix(graph)
#   return (log(gLaplacianFactor(m))/(log(gE(m)^(gE(m)-1))))
#   # return (log(gLaplacianFactor(m))/((n-1)/log(n)))
# }
#
# Be<-function(data, group, nproblems, graphlist) {
#   ldata<-subdataset(data,group,nproblems)
#   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
#   m <- dotgetMatrix(graph)
#   # if (REQUIREMINCLOSURE)
#   #   m<- gMinClosure(m)
#   problem <-substr(colnames(m)[which.max(gBetweennes(m))],2,2)
#   return (problem) #(substr(problem,2,1))
# }
#
# # De<-function(data, group, nproblems, graphlist) {
# #   ldata<-subdataset(data,group,nproblems)
# #   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
# #   return (gDAG(graph))
# # }
#
#
# WDag<-function(data, group, nproblems, graphlist) {
#   ldata<-subdataset(data,group,nproblems)
#   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
#   m <- dotgetMatrix(graph)
#   npaths <-0
#   n <- 0
#   for (node in (1:nrow(m))) {
#     if (sum(m[node,])==0) { # Leaf
#       ad <- dotgetAllDepths(graph,node)
#       npaths <- npaths + sum(ad)
#       n<-n+1
#     }
#   }
#   return(npaths/(sum(m)*gEmax(m)))
# }
#
#
# Dag<-function(data, group, nproblems, graphlist) {
#   ldata<-subdataset(data,group,nproblems)
#   graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
#   m <- dotgetMatrix(graph)
#   npaths <-0
#   n <- 0
#   for (node in (1:nrow(m))) {
#     if (sum(m[node,])==0) { # Leaf
#       ad <- dotgetAllDepths(graph,node)
#       npaths <- npaths + length(ad)
#       n<-n+length(ad)
#     }
#   }
#   return(npaths/(n*gEmax(m)))
# }
#
#
# -------------------

graph_LaplacianFactor<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
#  return (log(gLaplacianFactor(m))/(log(n^(n-1))))
  return (log(gLaplacianFactor(m))/((n-1)/log(n)))
}


graph_NSessions<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (sum(m)/maxS(data,group,nproblems))
}

graph_NProblemsSolved<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  v<-c()
  for (p in (allProblemsF())) {
    prob<- paste(p,"_s",sep="")
    if (prob %in% colnames(m))
      v <- append(v, ifelse(length(which(m[,prob]>0))>0,1,0))
  }
  return (sum(v))
}




graph_Betweenness<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  # if (REQUIREMINCLOSURE)
  #   m<- gMinClosure(m)
  problem <-substr(colnames(m)[which.max(gBetweennes(m))],2,1)
  return (problem) #(substr(problem,2,1))
}

# average shortest distance
graph_meanEnergy<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
    # m<- gMinClosure(m)
  return (sum(m)/gE(m))

}


#Max distance between nodes
graph_maxEnergy<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
    m<- gMinClosure(m)
  return (max(m))
}

# average shortest path length
graph_Closeness<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  m<- gMinClosure(gAdj(m))
  return (mean(m))

}

#Diameter of the network
graph_Diameter<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  m<- gMinClosure(gAdj(m))
  return (max(m))
}

graph_WAvrgDeg<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  # if (REQUIREMINCLOSURE)
  #   m<- gMinClosure(m)
  return (gWAvrgDeg(m)/maxS(data,group,nproblems))
}

graph_WDensity<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  # if (REQUIREMINCLOSURE)
    # m<- gMinClosure(m)
  return (We(data,group, nproblems, graphlist) *gE(m) / (nrow(m)*(nrow(m)-1)))
}

graph_PlanarDensity<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  if (REQUIREMINCLOSURE)
    m<- gMinClosure(m)
  return ((gE(m)-gN(m)+1)/(2*gN(m)-5))
}

graph_Connectedness<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (gAvrgDeg(m))
}



p<-function(data,group,nproblems=MAXPROBLEMS,graphlist=c()) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$OutCome=="solved",]
  return (length(myProblems(ldata,group, nproblems)))
  # pr<-unique(ldata$Problem)
  # return (length(pr))
}


np<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  return (p(ldata,group,nproblems)/nproblems)
}

ctp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$OutCome=="solved",]
  if(nrow(ldata[ldata$Problem==problem,])>0) {
    return(min(ldata[ldata$Problem==problem,"rct"]))
  } else {
    return(max(ldata[,"rct"]))
  }
}


otp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  return(min(ldata[ldata$Problem==problem,"nID"]))
}

totp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  return(min(ldata[ldata$Problem==problem,"rct"]))
}



stp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$Problem ==problem & ldata$OutCome=="solved","nID"]
  if (length(ldata)>0) {
    return(min(ldata))
  } else {
    return (0)
  }
}


tstp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$Problem ==problem & ldata$OutCome=="solved","rct"]
  if (length(ldata)>0) {
    return(min(ldata))
  } else {
    return (0)
  }
}

ftp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  return(max(ldata[ldata$Problem==problem,"nID"]))
}

tftp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  return(max(ldata[ldata$Problem==problem,"rct"]))
}




ot<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  as<-0
  v<-c()
  subproblems <- mySolvedProblems(data, group, nproblems)
  for (p in subproblems){
    if (METRICTIME){
      as <- as+ totp(ldata,group,p,nproblems)
      v<-c(v,c(p=totp(ldata,group,p,nproblems)))
    }else{
      as <- as+ otp(ldata,group,p,nproblems)
      v<-c(v,c(p=otp(ldata,group,p,nproblems)))
    }
  }
  # cat(v)
  return (as/(s(ldata,group,nproblems)*nproblems))
}

nt<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  as<-0
  subproblems <- mySolvedProblems(data, group, nproblems)
  for (p in subproblems){
    value <- ctp(ldata,group,p,nproblems)
    as <- as+ value
  }
  return (as/nproblems)
}


st<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  as<-0
  subproblems <- mySolvedProblems(data, group, nproblems)
  v<-c()
  for (p in subproblems){
    if(METRICTIME){
      as <- as+ tstp(ldata,group,p,nproblems)
      v<-c(v,c(p=tstp(ldata,group,p,nproblems)))
    } else{
      as <- as+ stp(ldata,group,p,nproblems)
      v<-c(v,c(p=stp(ldata,group,p,nproblems)))
    }
  }
  # cat(v)
  return (as/(s(ldata,group,nproblems)*nproblems))
}


rtp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$Problem ==problem ,]
  if (nrow(ldata[ldata$OutCome=="solved",])>0) {
    value<-stp(ldata,group,problem,nproblems)-otp(ldata,group,problem,nproblems)
    # cat(group,problem,value,"\n")
    return (value)
  }
  else {
    return (0)
  }
}

trtp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$Problem ==problem ,]
  if (nrow(ldata[ldata$OutCome=="solved",])>0) {
    value<-tstp(ldata,group,problem,nproblems)-totp(ldata,group,problem,nproblems)
    # cat(group,problem,value,"\n")
    return (value)
  }
  else {
    return (0)
  }
}


#   ldata<-subdataset(data,group,nproblems)
#   ldata<-ldata[#ldata$Problem==problem &
#                  ldata$nID>=otp(ldata,group,problem)
#                & ldata$nID<=stp(ldata,group,problem),]
#   return(nrow(ldata))
# }





rt<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  as<-0
  subproblems <- mySolvedProblems(ldata, group, nproblems)
  for (p in subproblems){
    if (METRICTIME)
      as <- as+ trtp(ldata,group,p,nproblems)
    else
      as <- as+ rtp(ldata,group,p,nproblems)
  }

  return (as/(s(ldata,group,nproblems)*nproblems))
}

ft<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  as<--Inf
  subproblems <- mySolvedProblems(ldata, group, nproblems)
  for (p in subproblems){
    if (METRICTIME)
      value <- tstp(ldata,group,p,nproblems)
    else
      value <- stp(ldata,group,p,nproblems)
    if (as< value)
      as <-value
  }
  return (as/(s(ldata,group,nproblems)))
}

# sqp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
#   ldata<-subdataset(data,group,nproblems, nsessions = stp(problem))
#   return (p(ldata,group,nproblems))
# }
#
# sq<-function(data,group,nproblems=MAXPROBLEMS) {
#   ldata<-subdataset(data,group,nproblems)
#   ldata<-ldata[ldata$OutCome=="solved",]
#   as <- 0
#   sq1 <- 0
#   sq2 <- 0
#   for (p in allProblems()) {
#     order <- sqp(ldata,group,p,nproblems)
#     expected<-as.numeric(substr(p,2,2))
#     cat(">",order,"-", expected,"\n")
#     as <- as + order *expected
#     sq1 <- sq1 + order*order
#     sq2 <- sq2 + expected*expected
#     cat("=",sq1,"-", sq2,"\n")
#   }
#   return (as/(sqrt(sq1)*sqrt(sq2)))
# }


frp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  return(length(ldata[ldata$nID>stp(ldata,group,problem,nproblems),"nID"]))
}

fr<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  return (nrow(ldata[ldata$OutCome=="fail",])/s(data,group,nproblems)/nproblems)
}


psp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  if (nrow(ldata[ldata$Problem==problem  &ldata$OutCome=="solved",])>0) {
    # value<-sp(ldata,group,problem,nproblemd)-st
    # value<-length(ldata[ldata$Problem==problem  & ldata$nID>stp(ldata,group,problem,nproblems),"nID"])
    value<-postsp(ldata,group,problem,nproblems)
    # cat(group,problem,value,"\n")
    return(value)
  }
  else {
    return (0)
  }
}


ps<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  as<-0
  subproblems <- mySolvedProblems(ldata, group, nproblems)
  for (p in subproblems){
    as <- as+ psp(ldata,group,p,nproblems)/s(ldata,group,nproblems)
  }
  return (as/nproblems)
}


sq<-function(data,group,nproblems=MAXPROBLEMS, GraphList=c()) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$OutCome=="solved",]
  as <- 0
  sq1 <- 0
  sq2 <- 0
  subproblems <- mySolvedProblems(ldata, group, nproblems)
  for (p in subproblems){
    order <- sqp(ldata,group,p,nproblems)
    expected<-as.numeric(substr(p,2,2))
    # cat(">",order,"-", expected,"\n")
    as <- as + order *expected
    sq1 <- sq1 + order*order
    sq2 <- sq2 + expected*expected
    # cat("=",sq1,"-", sq2,"\n")
  }
  return (as/(sqrt(sq1)*sqrt(sq2)))
}

sqp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
  ldata<-subdataset(data,group,nproblems)
  deadline <- stp(ldata,group,problem, nproblems)
  return (length(unique(ldata[ldata$nID<=deadline & ldata$OutCome =="solved","Problem"])))
}



# sqp<-function(data,group,problem,nproblems=MAXPROBLEMS) {
#   ldata<-subdataset(data,group,nproblems)
#   deadline <- stp(ldata,group,problem, nproblems)
#   return (length(unique(ldata[ldata$nID<=deadline & ldata$OutCome =="solved","Problem"])))
# }

# sq<-function(data,group,nproblems=MAXPROBLEMS) {
#   ldata<-subdataset(data,group,nproblems)
#   ldata<-ldata[ldata$OutCome=="solved",]
#   as <- 0
#   sq1 <- 0
#   sq2 <- 0
#   for (p in allProblems()) {
#     order <- sqp(ldata,group,p,nproblems)
#     expected<-as.numeric(substr(problem,2,2))
#     cat(order,"-", expected,"\n")
#     as <- as + order *expected
#     sq1 <- sq1 + order*order
#     sq2 < -sq2 + expected*expected
#   }
# }



Year<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"Year"])
}

Grade<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"Grade"])
}

Group<-function(data, group, nproblems=MAXPROBLEMS) {
  return(group)
}

QuartileGrade<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"QuartileGrade"])
}


NDAG<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"NDAG"])
}

Size<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"Size"])
}


myDataset<-function(data, nproblems=MAXPROBLEMS) {
  # dataset <<- data.frame()
  cat("Found ",length(unique(data$Group))," groups\n")
  dataset <-data.frame(matrix(nrow = length(allGroupsF(data)), ncol = 0))
  for (field in allfieldsF()) {
    dataset[field]<-as.numeric(seq(1:length(allGroupsF(data))))
  }

  for(i in (1:length(allGroupsF(data)))) {
    # for(i in (1:length(allGroups()))) {
    group <- allGroupsF(data)[i]
    # cat("processing group ",group,"\n")
    # dataset[nrow(dataset)+1,] <-NA;
    row <-c(Group=group)
    for(ifield in (1:length(allfieldsF()))) {
      field <- allfieldsF()[ifield]
      # cat(field, "\n")
      if (field %in% inheritedfieldsF()){
        # cat("   1-Processing field ", field, "\n")
        # value <-as.numeric(do.call(field, list(group)))
        value <-do.call(field, list(data, group, nproblems))
        # ldata<-subdataset(data,group,nproblems)
        # value <- ldata[[field]][1]
      } else {
        # cat("   2-Processing field ", field, "\n")
        value <-as.numeric(do.call(field, list(data, group, nproblems, GraphList)))
      }
      # row[field] <- value
      # dataset[i,field] <- value;
      dataset[i,field]<-value
    }
    # cat("Añadida fila ", i," = ",row,"\n")
  }
  dataset["Group"]<-as.character(allGroupsF(data))
  cat("Introducing extra factors \n")
  dataset$BAD <- (dataset$QuartileGrade ==1)
  dataset$PERFORMANCE <- ifelse (dataset$Grade<8.1, "LOW", "GOOD")
  dataset$Level<-nproblems
  # for (g in allGroups) {
  #   row=c();
  #   for(ifield in (1:2)) {
  #     field <- allfields()[ifield]
  #     row[field] <- do.call(field,c(data,g));
  #   }
  #   dataset[nrow(dataset)+1,] <-row
  # }
  # dataset <- SIIE23doImportXtraData(dataset,dsSIIE23ORIGINAL,nproblems)

  return (dataset)
}



######################################
######################################
######################################



# highestMetrics <- data.frame(name=character(0),pearson=numeric(0), kendall=numeric(0),spearman=numeric(0), key=numeric(0))

doSIIERegression<-function(data, objective="MAN", maxGrade=10,exponential=FALSE,   inverse=FALSE, nfields =c(
  "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
  "DAG",  "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
  "Newcomer",
  # "Mins.Starter" ,"Mins.ReactionTime",
  "Mins.EarlyBird"  ,
  "Mins.ClosingTime"    ,
  "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
  "Degree8","Degree9",
  "FDegree8","FDegree9",
  "LAP9", "Fit"  )) {
  rdataset <- data
  # rdataset<-rdataset[(rdataset$Size==1),]
  rdataset <- rdataset[(rdataset$Grade<=maxGrade),]
  highestMetrics <<- data.frame(name=character(0),fit=numeric(0),r2=numeric(0),mse=numeric(0),p=numeric(0))
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  for (xf in nfields) {
    if (xf != objective) {
      if (exponential) {
        # fit<-LCV_ExponentialRegression(rdataset,y=y, x=objective)
        # highestMetrics[nrow(highestMetrics)+1,] <<- c(y, abs(fit[1]),fit[2],fit[3],fit[4])
        # cat(paste(y,"~",objective,"\n"))
      } else {
        if (inverse) {
          fit<-LCV_LinearRegression(rdataset,x=objective,y=xf)
          highestMetrics[nrow(highestMetrics)+1,] <<- c(xf, abs(fit$pearson),fit$r2,-1,-1)
          cat(paste(xf,"~",objective,"=",fit$pearson),"\n")
        }else {
          fit<-LCV_LinearRegression(rdataset,y=objective,x=xf)
          highestMetrics[nrow(highestMetrics)+1,] <<- c(xf, abs(fit$pearson),fit$r2,-1,-1)
          cat(paste(objective,"~",xf,"=",fit$pearson),"\n")
        }
      }
    }
  }
  highestMetrics <- highestMetrics[(order(highestMetrics$fit, decreasing=TRUE)),]
  show(highestMetrics)
  for (i in seq(1:5)) {
    LCV_LinearRegression(rdataset,highestMetrics[i,"name"],objective)
  }
  highestMetrics
}


doRegression<-function(data, objective=c("MAN"), maxGrade=10) {
  # objective <- c("MAN", "N.ProblemsSolved", "Mins.EarlyBird")
  rdataset <- data
  rdataset<-rdataset[(rdataset$Size==1),]
  rdataset <- rdataset[(rdataset$Grade<=maxGrade),]
  oname <- ""
  if (length(objective) > 0) {
    rdataset$objective <- 0
    for (smix in objective) {
      rdataset$objective <- rdataset$objective + rdataset[[smix]]
      oname <- paste(oname, smix,"/")
    }
    rdataset$objective = rdataset$objective / length(objective)
  } else {
    rdataset$objective <- 0
  }
  nfields <-c(
    "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
    "DAG",
    "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
    "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
    "Mins.ClosingTime"    ,
    "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
    "Degree1","Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
    "FDegree1","FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
    "LAP1","LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9",
    "objective"
  );
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  variable<-rdataset$objective
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  highestMetrics <<- data.frame(name=character(0),pearson=numeric(0), kendall=numeric(0),spearman=numeric(0), key=numeric(0))
  for (y in nfields) {
    if (y != "objective") {
      value <- rdataset[[y]]
      rdataset.RP<- cor.test(variable, value, method=("pearson"))
      rdataset.RK<- cor.test(variable, value, method=("kendall"))
      rdataset.RS<- cor.test(variable, value, method=("spearman"))
      RP<- abs(rdataset.RP$estimate)
      RK<- abs(rdataset.RK$estimate)
      RS<- abs(rdataset.RS$estimate)
      cat("Processing x=",oname," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
      plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)
      highestMetrics[nrow(highestMetrics)+1,] <<- c(y, RP, RK, RS, (RP+RK+RS)/3)
    }
  }
  highestMetrics[(order(highestMetrics$key, decreasing=TRUE)),]
  # highestMetrics
}

doRegressionLM<-function(data, objective=c("MAN"), maxGrade=10) {
  # objective <- c("MAN", "N.ProblemsSolved", "Mins.EarlyBird")
  rdataset <- data
  rdataset<-rdataset[(rdataset$Size==1),]
  rdataset <- rdataset[(rdataset$Grade<=maxGrade),]
  oname <- ""
  if (length(objective) > 0) {
    rdataset$objective <- 0
    for (smix in objective) {
      rdataset$objective <- rdataset$objective + rdataset[[smix]]
      oname <- paste(oname, smix,"/")
    }
    rdataset$objective = rdataset$objective / length(objective)
  } else {
    rdataset$objective <- 0
  }
  nfields <-c(
    "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
    "DAG",
    "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
    "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
    "Mins.ClosingTime"    ,
    "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
    "Degree1","Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
    "FDegree1","FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
    "LAP1","LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9",
    "objective"
  );
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  variable<-rdataset$objective
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  highestMetrics <<- data.frame(name=character(0),pearson=numeric(0), kendall=numeric(0),spearman=numeric(0), key=numeric(0))
  for (y in nfields) {
    if (y != "objective") {
      value <- rdataset[[y]]
      linearmodel <- lm(value ~ variable)
      cat("Processing x=",oname," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
      plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)
      highestMetrics[nrow(highestMetrics)+1,] <<- c(y, RP, RK, RS, (RP+RK+RS)/3)
    }
  }
  highestMetrics[(order(highestMetrics$key, decreasing=TRUE)),]
  # highestMetrics
}

doC50 <- function(data,metrics) {
  c5dataset <- data
  smetrics <- sort(metrics,decreasing=TRUE)
  c5indexvars<-c(smetrics.);
  # c5indexvars<-c(seq(25:35))
  set.seed(100)
  # c5dataset<-c5dataset[(c5dataset$QGrade=='Q1'),]
  rows <- sample(nrow(c5dataset))
  c5dataset<-c5dataset[rows,]
  c5vardataset <- c5dataset[,c5indexvars]
  c5Output <- as.factor(c5dataset$QGrade)
  c50tree<- C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
  # c50tree<- C5.0(x=c5vardataset,y=c5Output, trials=100)
  c50tree
  summary(c50tree)
  plot(c50tree)

}

doRegressionSimplified<-function(data, variable) {
  x <- variable
  x2 <- "Degree8"
  x3<-"FDegree8"
  rdataset <- data
  rdataset$Mix <- rdataset[[x3]]+rdataset[[x2]]
  nfields <-c(
    variable,
    "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
    "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
    "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
    "Mins.ClosingTime"    ,
    "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day"
  );
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  x<-"objective"
  variable<-rdataset[[x]]
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  for (y in nfields) {
    value <- rdataset[[y]]
    rdataset.RP<- cor.test(variable, value, method=("pearson"))
    rdataset.RK<- cor.test(variable, value, method=("kendall"))
    rdataset.RS<- cor.test(variable, value, method=("spearman"))
    cat("Processing x=",oname," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
    plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)

  }
}



doSIIECluster2<-function(data, ids=c("Group"),metrics=c("SIM","N.ProblemsSolved","MAN","DAG","Grade","N.Sessions","N.Sessions.Before","N.Sessions.After","Mins.Sessions","Mins.Sessions.Before","Mins.Sessions.After","Mins.Day","Mins.Starter","Mins.ReactionTime","Mins.EarlyBird","Mins.ClosingTime")) {
  KM.data <<- data[,c(ids,metrics)]
  KM.nc <<- NbClust(data[,metrics],distance="euclidean",min.nc=2, max.nc = 10, method="kmeans")
  data$fKM <- as.factor(KM.nc$Best.partition)
  data$nKM <- KM.nc$Best.partition

  for (clus in unique(KM.nc$Best.partition)) {
    cat(paste("Cluster ",clus))
    show(data[(data$fKM == clus),c("Group")])
  }
  for (sm in metrics) {
    show(LCV_boxplot(data,"fKM",sm))
  }
  c5vardataset <- data[,metrics]
  c5Output <- data$fKM
  data.tree<- C5.0(x=c5vardataset,y=c5Output, trials=100, rules=FALSE)
  data.rules<- C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
  # show(c50tree)
  # show(summary(data.tree))
  # plot(data.tree)
  # show(data[,c("Group","fKM")])
  data
}



# nfields <-c(
#   "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
#   "DAG",
#   "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
#   "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
#   "Mins.ClosingTime"    ,
#   "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
#   "Degree1","Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
#   "FDegree1","FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
#   "LAP1","LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9"  );

SIIEridge <- function(data) {
  res <- data.frame(Group=character(0), x=numeric(0), y=numeric(0))
  row <- data[1]
  col<-c  ("LAP01","LAP02","LAP03","LAP04","LAP05","LAP06","LAP07","LAP08","LAP09")
  for (i in (1:9)) {
    ngroup <- c(row["Group"])
    x<-i
    col <- paste("LAP0",i, sep="")
    y<-row[col]
    newrow <- c(ngroup,x,y)
    res [res[ncol()]]
  }
  res
}

SIIE23ViewGroup <- function(data, np) {
  res <- paste(paste(str_replace_all(data," " ,"_"),"_",sep=""))
  final <- ""
  for (s in paste(np,"Problems/",res,"/",res,"*DAG*.png", sep="")) {
    final <- paste(final," ", s)
  }
  cat(paste("montage ",final," -geometry 300x300+0+1 -tile 5x", sep=""))
}

# SIIE23Init <- function() {
#   #\\readline("Press INTRO to LOAD DATA AND PREPROCESS")
#   SIIE23 <<- read.delim2("~/Descargas/SIIE23.tsv")
#   SIIE23 <<- doPrepareLogScale(SIIE23)
#   # SIIE23$QuartileGrade<- as.factor(SIIE23$QuartileGrade)
#   # SIIE23clus <- doSIIECluster1(SIIE23, id_column = c("Group"), main_kpi = "LOGLAP09", other_kpi = c("QuartileGrade","rt","ot","ns"),force_nc=-1, compare_to = c("Grade","m","np",	"rt",	"ot",	"ns"	,"ps"	,"ot"	,"est"	,"ft"	,"fr"))
#   cat("OK\n")
#   #\\readline("Press INTRO to ANALISIS OF m/mf wrt Grade")
#   for (metric in c("mf","m")) {
#     show(LCV_density(SIIE23,metric, showall = TRUE))
#     LCV_LinearRegression(SIIE23,x=metric, y="Grade", doshow = TRUE, full = TRUE)
#   }
#   #\\readline("Press INTRO to ANALISIS OF COMPONENTS wrt Grade")
#   for (metric in c("ng","ns", "np", "ot", "st", "rt", "ft", "sq", "ps", "fr")) {
#     show(LCV_density(SIIE23,metric, showall = TRUE))
#     LCV_LinearRegression(SIIE23,x=metric, y="Grade", doshow = TRUE, full = TRUE)
#   }
#   #\\readline("Press INTRO to CLUSTER ANALISIS")
#   SIIE23clus <<- doSIIECluster1(SIIE23, id_column = c("Group"), main_kpi = "LOGLAP09",
#                                 other_kpi = c("np","rt","ot","ns"),force_nc=-1, compare_to = c("np","rt","ot","ns","LOGLAP09", "QuartileGrade", "Grade"))
#
#   #\\readline("Press INTRO to RULES DISCOVERY")
#   rules<<-C5.0(x=SIIE23clus[,c("np","rt","ot","ns","LOGLAP09")],y=as.factor(SIIE23clus[,c("QuartileGrade")]),trials = 100, rules=TRUE)
#   # rules<<-C5.0(x=SIIE23clus[,c("st","ft","ps","fr","rt","ot","ns")],y=as.factor(SIIE23clus[,c("KMEANS_LOGLAP09")]),trials = 100, rules=TRUE)
#   show(summary(rules))
#   #\\readline("Press INTRO to DT DISCOVERY")
#   tree<<-C5.0(x=SIIE23clus[,c("np","rt","ot","ns","LOGLAP09")],y=as.factor(SIIE23clus[,c("QuartileGrade")]),trials = 100, rules=FALSE)
#   # tree<<-C5.0(x=SIIE23clus[,c("st","ft","ps","fr","rt","ot","ns")],y=as.factor(SIIE23clus[,c("KMEANS_LOGLAP09")]),trials = 100, rules=FALSE)
#   show(summary(tree))
#   plot(tree)
#   #\\readline("Expectations")
#   LCV_densitiesFactor(SIIE23clus, "ot","QuartileGrade", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "ot","KMEANS_LOGLAP09", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "rt","QuartileGrade", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "rt","KMEANS_LOGLAP09", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "ns","QuartileGrade", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "ns","KMEANS_LOGLAP09", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "m","QuartileGrade", showmax=TRUE)
#   LCV_densitiesFactor(SIIE23clus, "m","KMEANS_LOGLAP09", showmax=TRUE)
# }


SIIE23AnalyzeCluster<-function(data, cluster) {
  kpi=cluster;
  for (clus in c("CLUS_LOGLAP05","CLUS_LOGLAP06","CLUS_LOGLAP07","CLUS_LOGLAP08","CLUS_LOGLAP09")) {
    show(LCV_densitiesFactor(SIIE23,kpi,clus, showmax = TRUE))
  }
  for (clus in c("CLUS_LOGFLAP5","CLUS_LOGFLAP6","CLUS_LOGFLAP7","CLUS_LOGFLAP8","CLUS_LOGFLAP9" )) {
    show(LCV_densitiesFactor(SIIE23,kpi,clus, showmax = TRUE))
  }
  for (clus in
       c("QuartileGrade" )) {
    show(LCV_densitiesFactor(SIIE23,kpi,clus, showmax = TRUE))
  }
  show(LCV_histogram(data,cluster, nintervals = 10))
}



SIIE23ShowMetric <- function(data, metric, factor="PERFORMANCE", factorvalue="LOW") {
  data$all <-1
  show(LCV_boxplot(data,metric, "all"))
  dsaux <- LCV_RemoveOutliers(data,metric,"all")
  show(LCV_boxplot(dsaux,metric, "all"))
  show(LCV_densitiesFactor(dsaux,metric,factor,showmax = TRUE))
  dsaux$SEGMENT<- ifelse(dsaux[[factor]]==factorvalue,"YES","NO")
  LCV_STUDENT(dsaux,"SEGMENT",metric,)

}

# SIIE23doBestC50Fit<-function(metrics=mastermetrics, target="KMEANS_Grade", size=3,downto=MAXPROBLEMS,seqmetrix=seq(1:length(allcombinationsmetrics))) {
#   result <- list()
#   iresult<-0
#    for (icomb in seqmetrix) {
#     thismetric <- allcombinationsmetrics[[icomb]]
#     if (length(thismetric)==size) {
#       mds <- dsList[[9]]
#       cat(icomb,"/",seqmetrix[length(seqmetrix)],"\n")
#       if (icomb %%100==0)
#         cat("\n")
#       # cat(icomb,"_", thismetric,"\n")
#       thistree <- C5.0(x=mds[,thismetric],y = as.factor(mds[[target]]), trials = 100,rules=FALSE)
#       thiserror <- getC50Error(thistree)
#       if (thiserror == 0) {
#         # cat("\nTrying ",icomb,"/",length(allcombinations), "--->",thismetric,"\n")
#         allfit <- TRUE
#         allprob <- 0
#         nprob<-0
#         for (ids in (downto:9)) {
#           cat("\nLevel",ids)
#           mds <- dsList[[ids]]
#           cres <- SIIE23doC50Fit(ids,thistree)
#           mcover = cres["coverage"]
#           mprob <- cres["prob"]
#           mcover<-0
#           mprob<-0
#            # cat(   "result", mcover, mprob, "\n")
#           allfit <- allfit &  (mcover==100);
#           if (mcover == 100) {
#             allprob <- allprob+mprob
#             nprob<-nprob+1
#           } else {
#             break
#           }
#           # cat("   level ",ids,"\n");
#         }
#         if (allfit) {
#           cat("+++++",thismetric,"\n");
#           iresult<-iresult+1;
#           # cCat("Found ", length(result), "\n")
#            # cat("\nTrying ",icomb," --->",thismetric,"\n")
#           # cat ("Trying combination ", thismetric, " --> ", allfit,"\n")
#           # result <- append(result, list(metric=thismetric, coverage=100, prob=allprob/nprob))
#           # result[iresult,] <- c(metrics=list(thismetric),prob=allprob/nprob)
#           result[[iresult]] <- c(c(allprob/nprob), thismetric)
#           # result[iresult,"prob"] <-allprob/nprob
#         } else {
#           cat("----\n");
#         }
#       }
#     }
#    }
#   cat("Found ",length(result), " combination of metrics\n")
#   return(result)
# }
#


# SIIE23doBestC50Fit<-function(data,data2, metrics=c(), target="KMEANS_Grade", size=3) {
#   allcombinations <- do.call("c", lapply(seq_along(metrics), function(i) combn(metrics, i, FUN = list)))
#   result <- list()
#
#   for (icomb in (1:length(allcombinations))) {
#     thismetric <- allcombinations[[icomb]]
#     if (length(thismetric)==size) {
#       cat(thismetric,"\n")
#       thistree1 <- C5.0(x=data[,thismetric],y = as.factor(data[[target]]), trials = 100,rules=FALSE)
#       thiserror1 <- getC50Error(thistree1)
#       thistree2 <- C5.0(x=data2[,thismetric],y = as.factor(data2[[target]]), trials = 100,rules=FALSE)
#       thiserror2 <- getC50Error(thistree2)
#       cat ("Trying combination ", thismetric, " --> ", thiserror1,"/",thiserror2, "\n")
#       if (thiserror1 == 0 & thiserror2==0) {
#         result <- append(result, list(thismetric))
#       }
#     }
#   }
#   return(result)
# }



# SIIE23doBestC50Fit<-function(metrics=mastermetrics, target="KMEANS_Grade", size=3,downto=MAXPROBLEMS,seqmetrix=seq(1:length(allcombinationsmetrics))) {
#   result <- data.frame(metric=character(), prob=numeric())
#   iresult<-0
#   cat("Searching the best combination of metrics\n")
#   # preping the dataset
#   newds <-data.frame(dsList[9])
#   goodmetrics <- c()
#   i<-downto
#   while (i <9) {
#     newds <- rbind(newds,data.frame(dsList[i]))
#     i<-i+1;
#   }
#   newds<- newds[newds$KMEANS_Grade != "XXX",]
#   # cat("From ",downto,"-",9,"=",nrow(newds),"rows\n")
#   for (icomb in seqmetrix) {
#     thismetric <- allcombinationsmetrics[[icomb]]
#     if (length(thismetric)==size) {
#       cat(icomb,",")
#       if (icomb %%40 == 0)
#         cat("\n");
#       # cat(icomb,"_", thismetric,"\n")
#       # thistree <- C5.0(x=dsSIIE23FULL[,thismetric],y = as.factor(dsSIIE23FULL[[target]]), trials = 100,rules=FALSE)
#       dsC50 <- newds #data.frame(newds[newds$KMEANS_Grade != "XXX",])
#       thistree <- C5.0(x=dsC50[,thismetric],y = as.factor(dsC50[,"KMEANS_Grade"]), trials = 100,rules=FALSE)
#       thiserror <- getC50Error(thistree)
#       if (thiserror == 0) {
#         cat("\n\nFound a valid tree for ", thismetric," with ", nrow(dsC50),"rows and ", ncol(dsC50),"cols\n\n")
#         iresult <- iresult +1
#         # allfit <- TRUE
#         # allprob <- 0
#         # nprob<-0
#         # cres <- SIIE23doC50Fit(newds,thistree)
#         # mcover = cres["coverage"]
#         # mprob <- cres["prob"]
#         # if(mcover==100) {
#         mprob<-0
#         result[iresult,] <- c(prob=mprob, metric=paste(thismetric, collapse=";"))
#         # cat("\nFound ",icomb,"/",thismetric, "--->",cres,"\n")
#         # }
#         goodmetrics <- c(goodmetrics, icomb)
#       }
#       rm(thistree)
#       gc()
#     }
#   }
#   cat("\n\nFound ",length(result), " combination of metrics\n\n")
#   cat("goodmetrics<-c(",paste(goodmetrics, collapse=","),")\n")
#   show(result)
#   return(result)
# }

#
# SIIE23AccuracyMetrics<-function(ldata, result,bylevels=FALSE) {
#   # data<- do.call(cbind.data.frame, mlist)
#   cat("Metric","\tError", "Cover","Prob","min Level", "max level\n",sep="\t\t")
#   data <-data.frame(ldata)
#   dstest<-data[data$KMEANS_Grade!="XXX",]
#   bestprob<-0
#   best <-c()
#   for (i in (1:nrow(result))) {
#     metric <- result[i,]
#     mymetrics <- c(strsplit(metric[1,"prob"],";"))
#     variables<-mymetrics[[1]]
#     minLevel <- min(valid_data$Level)
#     maxLevel <- max(valid_data$Level)
#     cx<-valid_data[,variables]
#     cy<-as.factor(valid_data$KMEANS_Grade)
#     thistree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
#     thiserror <- getC50Error(thistree)
#     cres <- SIIE23doC50Fit(valid_data,thistree)
#     mcover = as.numeric(cres["coverage"])
#     mprob <- as.numeric(cres["prob"])
#     strvar<-metric[1,"prob"]
#     crow<-c(strvar,thiserror,mcover,mprob,minLevel,maxLevel)
#     if(mcover == 100) {
#       if (mprob>bestprob) {
#         bestprob<-mprob
#         best<-crow
#       }
#       cat(crow,"\n",sep="\t\t")
#     }
#   }
#   cat("\n\nBEST: \n",best)
#   if (bylevels) {
#     metric <- best[1]
#     mymetrics <- c(strsplit(metric,";"))
#     variables<-mymetrics[[1]]
#     minLevel <- min(valid_data$Level)
#     maxLevel <- max(valid_data$Level)
#     cat("Deep validation by levels\n")
#     cat("   Obtaining normal decision tree\n")
#     cx<-valid_data[,variables]
#     cy<-as.factor(valid_data$KMEANS_Grade)
#     thistree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
#     thiserror <- getC50Error(thistree)
#     # cat(linesFrom(summary(thistree),"classified as"))
#     cat("   Applying normal tree level by level\n")
#     cat("Level","p", "s","Cover","Prob","\n",sep="\t\t")
#     for (ilevel in(minLevel:maxLevel)) {
#       valid_data<-data[data$KMEANS_Grade!="XXX" & data$Level==ilevel,]
#       ns = sum(valid_data$s)
#       np = median(valid_data$p)
#       cres <- SIIE23doC50Fit(valid_data,thistree)
#       mcover = as.numeric(cres["coverage"])
#       mprob <- as.numeric(cres["prob"])
#       cat(ilevel,np,ns,mcover,mprob,"\n",sep="\t\t")
#     }
#   }
# }
#

# SIIE23AccuracyMetrics<-function(mlist, result) {
#   # data<- do.call(cbind.data.frame, mlist)
#   cat("Metric","Error", "Cover","Prob","min Level", "max level",sep="\t\t")
#   for (i in (1:nrow(result))) {
#     metric <- result[i,]
#     mymetrics <- c(strsplit(metric[i,"prob"],";"))
#     variables<-mymetrics[[1]]
#     header<-c("Group","Level","Grade","KMEANS_Grade", variables)
#     nrows<-length(mlist[[1]])
#     ncols<-length(header)
#     # data<- data.frame(matrix(NA, nrow = length(mlist[[1]]), ncol = length(header)))
#     data<- data.frame(matrix(ncol=length(header)))
#     colnames(data)<-header
#     for (r in (1:nrows)){
#       crow <- c()
#       for (c in (1:ncols)){
#         caux<-c()
#         caux[header[c]]<-mlist[[header[c]]][r]
#         crow <- c(crow, caux)
#       }
#       data[r,]<-crow
#     }
#     # ds <- data[data$KMEANS_Grade!="XXX",]
#     valid_data<-data[data$KMEANS_Grade!="XXX",]
#     minLevel <- min(valid_data$Level)
#     maxLevel <- max(valid_data$Level)
#     cx<-valid_data[,variables]
#     cy<-as.factor(valid_data$KMEANS_Grade)
#     thistree <- C5.0(x=cx,y=cy,trials=100, rules=FALSE)   #do.call(cbind.data.frame, x)y = as.factor(y), trials = 100,rules=FALSE)
#     thiserror <- getC50Error(thistree)
#     cres <- SIIE23doC50Fit(valid_data,thistree)
#     mcover = cres["coverage"]
#     mprob <- cres["prob"]
#     cat(cat(variables),thiserror,mcover,mprob,minLevel,maxLevel,sep="\t\t")
#   }
# }
#

# SIIE23doC50Fit<-function(data,C50Tree, doecho=FALSE) {
#   target<-"KMEANS_Grade"
#   goods<- 0
#   pgoods<-1
#   probs <- c()
#   if (doecho)  cat("Processing ",nrow(data), "rows\n")
#
#   for (i in (1:nrow(data))) {
#     row <- data[i,]
#     grade <- data[i,"Grade"]
#     matrix<-predict(C50Tree,row,type="prob")
#     # accuracy(C50Tree)
#     # matrix<-localm
#     ic <- which.max(matrix[1,])
#     prob<-max(matrix[1,])
#     probs <- c(probs, prob)
#     id <- colnames(matrix)[which.max(matrix)]
#     lmin <- as.numeric(strsplit(strsplit(id,",")[[1]][1], "\\[")[[1]][2])
#     lmax <- as.numeric(strsplit(strsplit(id,",")[[1]][2], "\\]")[[1]][1])
#     stringrow<-""
#     if (lmin <= grade & grade <= lmax) {
#       goods <- goods+1
#       pgoods<-pgoods+prob
#       pres<-"+"
#     } else {
#       pres<-"-"
#     }
#       # stringrow<- paste(stringrow,row[
#
#     # if (doecho) cat(data[i,"Group"], "with grade", grade, "has been classified within ", lmin, "-",lmax, "=")
#     # if (lmin <= grade & grade <= lmax) {
#     #   goods <- goods+1
#     #   pgoods<-pgoods+prob
#     #   if (doecho) cat(" +++++ (",prob,")")
#     # }
#     # if (doecho)    cat("\n")
#     # data$KMEANS_Grade_min[i],"-",data$KMEANS_Grade_max[i]," with probability", max(matrix[1,(1:4)]),"\n")
#   }
#   # value<-c(coverage=round(goods/nrow(data)*100,1),prob=pgoods/goods)
#   value<-c(coverage=round(goods/nrow(data)*100,3),prob=round(median(probs),3), N=nrow(data), Levels=paste(unique(data$Level),collapse=","))
#   rm(data)
#   return (value);
# }
# myDataset<-function(data, nproblems=MAXPROBLEMS) {
#   # dataset <<- data.frame()
#   cat("Found ",length(unique(data$Group))," groups")
#   dataset <-data.frame(Group=character(),Year=character(), Grade=numeric(),
#                        QuartileGrade=character(),DAG=numeric(),
#                        Size=numeric(),p=numeric(),s=numeric(),ns=numeric(), np=numeric(),
#                        ot=numeric(),rt=numeric(),st=numeric(),ft=numeric(),
#                        ps=numeric(),fr=numeric(), sq=numeric());
#   #,ps=numeric(),fr=numeric())
#
#   # return (c("Group",	"Year","Grade", "QuartileGrade","DAG","Size","p","s","ns","np","ot","st","rt","ft","sq","ps","fr"))
#   for(i in (1:length(allGroups()))) {
#     # for(i in (1:length(allGroups()))) {
#     group <- allGroups()[i]
#     # cat("processing group ",group,"\n")
#     # dataset[nrow(dataset)+1,] <-NA;
#     row <-c()
#     for(ifield in (1:length(allfields()))) {
#       field <- allfields()[ifield]
#       if (ifield<7){
#         # cat("   1-Processing field ", field, "\n")
#         value <-do.call(field, list(group))
#       } else {
#         # cat("   2-Processing field ", field, "\n")
#         value <-do.call(field, list(data, group, nproblems))
#       }
#       row <- c(row, field=value)
#       # row[field] <- value
#       # dataset[i,field] <- value;
#
#     }
#     # cat("Añadida fila ", i," = ",row,"\n")
#   }
#
#   # for (g in allGroups) {
#   #   row=c();
#   #   for(ifield in (1:2)) {
#   #     field <- allfields()[ifield]
#   #     row[field] <- do.call(field,c(data,g));
#   #   }
#   #   dataset[nrow(dataset)+1,] <-row
#   # }
#   return (dataset)
# }
#

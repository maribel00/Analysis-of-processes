source("LCV_Theme.R")
source("LCV_Bayes.R")
source("LCV_Clustering.R")
source("LCV_Graphs.R")
source("LCV_Hipothesis_Tests.R")
source("LCV_Regression.R")
source("LCV_plotting.R")
source("LCV_GraphMiner.R")
source("LCV_Dot.R")

MAXPROBLEMS<-9
pwd<<-"."

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

SIIE23doLoadSessions<-function() {
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

# Calcular los cuartiles
SIIE23quantiles<-function(data) {
  return (quantile(data$Grade, probs = c(0.25, 0.5, 0.75)))
}

# Función para asignar etiquetas a los cuartiles
SIIE23assignLabel <- function(value, q) {
  if (value <= q[1]) {
    return(1)
  } else if (value <= q[2]) {
    return(2)
  } else if (value <= q[3]) {
    return(3)
  } else {
    return(4)
  }
}

# Agregar una nueva columna con las etiquetas de los cuartiles
SIIE23addQuantiles<-function(data) {
  q <- SIIE23quantiles(data)
  data$QuartileGrade <- sapply(data$Grade, SIIE23assignLabel, q)
  return (data)
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

maxS<-function(data,group, nproblems=MAXPROBLEMS) {
  year<-subdataset(data,group,nproblems)[1,"Year"]
  return(max(data[data$Year==year,"nID"]))
}

allclassicmetricsF<-function() {
  return (c("ns","np","ot","st","rt","ft","ps","fr","sq"))
}

allmetricsF<-function() {
  # Modified
  return (c("s", "p", "De", "Dm" ,"Le", "Di", "Co", "We", "Ef", "St","Dag","WDag", "Be"))
}

allfieldsF<-function() {
  return (c(inheritedfieldsF(),allmetricsF()))
}

inheritedfieldsF<-function() {
  return (c("Group","Year","Grade","QuartileGrade","Size"))
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

Group<-function(data, group, nproblems=MAXPROBLEMS) {
  return(group)
}

Year<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"Year"])
}

Grade<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"Grade"])
}

QuartileGrade<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"QuartileGrade"])
}

Size<-function(data, group, nproblems=MAXPROBLEMS) {
  return(data[data$Group==group,][1,"Size"])
}

myProblems<-function(data, group, nproblems=MAXPROBLEMS, nsessions=10000) {
  ldata<-subdataset(data,group,nproblems,nsessions)
  return  (unique(ldata[,"Problem"]))
}

p<-function(data,group,nproblems=MAXPROBLEMS,graphlist=c()) {
  ldata<-subdataset(data,group,nproblems)
  ldata<-ldata[ldata$OutCome=="solved",]
  return (length(myProblems(ldata,group, nproblems)))
}

s<-function(data,group,nproblems=MAXPROBLEMS, graphlist=c()) {
  ldata<-subdataset(data,group,nproblems)
  return (nrow(ldata))
}

De<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (We(data,group, nproblems, graphlist) *gE(m) / (nrow(m)*(nrow(m)-1)))
}

We<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (sum(m)/maxS(data,group,nproblems))
}

Dm<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (gWAvrgDeg(m)/maxS(data,group,nproblems))
}

Le<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  m<- gMinClosure(m)
  return (mean(m)/sum(m))
}

Di<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  m<- gMinClosure(m)
  return (max(m)/sum(m))
}

Co<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (gAvrgDeg(m)/gEmax(m))
}

Ef<-function(data, group, nproblems, graphlist) {
  return(graph_NProblemsSolved(data,group,nproblems,graphlist)/nproblems)
}

St<-function(data, group, nproblems, graphlist) {
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  return (log(gLaplacianFactor(m))/(log(gE(m)^(gE(m)-1))))
}

Dag<-function(data, group, nproblems, graphlist) {
  # Modified -> changed mc by m
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
  return(npaths/(n*gEmax(m)))
}

WDag<-function(data, group, nproblems, graphlist) {
  # Modified -> changed mc by m
  ldata<-subdataset(data,group,nproblems)
  graph <- graphlist[[group]][[paste("L",nproblems, sep="")]]
  m <- dotgetMatrix(graph)
  m <-gMinClosure(m)
  npaths <-0
  n <- 0
  for (node in (1:nrow(m))) {
    if (sum(m[node,])==0) { # Leaf
      ad <- dotgetAllDepths(graph,node)
      npaths <- npaths + sum(ad)
      n<-n+1
    }
  }
  return(npaths/(n*sum(m)))
}

Be<-function(data, group, nproblems, graphlist) {
  return(graph_Betweenness(data,group,nproblems,graphlist))
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

SIIE23doGenerateDatasets<-function(downto=MAXPROBLEMS) {
  outliers <- c()
  dstop<-data.frame()
  for (np in (MAXPROBLEMS:downto)) {
    cat("\nGenerating data frame at level ", np,":")
    ds <- myDataset(dsSIIE23RAW, np)
    ds$OUTLIER <- "NO"
    if (np == MAXPROBLEMS) {
      #\\readline(paste("Removing outliers level ",np,"from ", nrow(ds),"rows \n"))
      ds[ds$s>=1000,"OUTLIER"]<-"TOO.MANY.SESSIONS"
      ds[ds$p<=6,"OUTLIER"]<-"TOO.FEW.PROBLEMS"
      outliers<- union(outliers,ds[ds$s>=1000,"Group"])
      outliers<- union(outliers,ds[ds$p<=6,"Group"])
      # ds <- ds[!ds$Group  %in% outliers,]
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
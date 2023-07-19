library(C50)
library(forcats)
library(ggplot2)
library(caret)

source("LCV_Dot.R")

graphpwd<-"/home/lcv/Dropbox/Research/DBA/MB/Graphs"

graphoptions<-function(name) {
  return( list(problemname=name,maxproblems=9,minproblems=9,fieldcase="Session",
               fieldagent="Group", fieldactivity="Composite",
               fieldproblem="Problem",leafsuffix=".solved",rootsuffix=".fail",
               leafid="_s", rootid="_f",
               reflexive=FALSE, cyclic=FALSE,
               engine="circo"))
}

gpreProcess<-function(data, graphoptions) {
  for (r in (1:nrow(data))) {
    data[r,graphoptions$fieldactivity]<-gsub(graphoptions$leafsuffix,graphoptions$leafid,data[r,graphoptions$fieldactivity])
    data[r,graphoptions$fieldactivity]<-gsub(graphoptions$rootsuffix,graphoptions$rootid,data[r,graphoptions$fieldactivity])
  }
  for (r in (1:nrow(data))) {
    data[r,graphoptions$fieldactivity]<-gsub(graphoptions$leafsuffix,graphoptions$leafid,data[r,graphoptions$fieldactivity])
    data[r,graphoptions$fieldactivity]<-gsub(graphoptions$rootsuffix,graphoptions$rootid,data[r,graphoptions$fieldactivity])
  }
  return (data)
}

gRightUntil<-function(data, graphoptions, level) {
  for (r in (1:nrow(data))) {
    prev <- data[data$nID <r,]
    nproblems <- length(unique(prev[endsWith(prev[[graphoptions$fieldactivity]],graphoptions$leafid),graphoptions$fieldproblem]))
    data[r,"psolved"] <- nproblems
  }
  return (data[data$psolved<level,])

}



GraphMiner<-function(data,graphoptions) {
  # readline("Preprocessing activities\n")
  # data<-gpreProcess(data,graphoptions)
  readline("Extracting groups\n")
  dsgroup <- unique(data[,"Group"])
  exit<-FALSE
  for (level in (graphoptions$minproblems:graphoptions$maxproblems)) {
    for (igroup in (1:length(dsgroup))) {
      group <- dsgroup[igroup]
      ugroup <- gsub(" ","_",group)
      readline(cat("Creating graph for group ",group," up to ",level," problems solved\n"))
      id<-paste(ugroup,"_",level,sep="")
      res<-dot(id,TRUE)
      res$title <- paste(group, level)
      dsxGroup <-data[data$Group == group, ]
      dsxGroup <- gRightUntil(dsxGroup,graphoptions, level)
      dsxGroup<-dsxGroup[order(dsxGroup$Start,decreasing = FALSE),]
      # dsSessionNames = unique(dsxGroup[,"Session"])
      dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
      # Add nodes
      readline(cat("Adding nodes ",group,"\n"))
      res<-dotAddNode(res,"A","square")
      for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
        if (endsWith(n,graphoptions$leafid))
          res<-dotAddNode(res,n,"doublecircle")
        else
          res<-dotAddNode(res,n,"circle")
      }
      # Add arcs
      readline(cat("Adding ",nrow(dsxGroupxP), " edges\n"))
      nprev<-"A"
      first <-dsxGroupxP[1,graphoptions$fieldactivity]
      for (r in (1:nrow(dsxGroupxP))) {
        nnext<-dsxGroupxP[r,graphoptions$fieldactivity]
        potential<-c(nprev)
        visited<-c()
        nprev<-potential[1]
        # cat(r,") TRY ",nprev,"-->",nnext,"(",dotgetAllAncestors(res,nprev),")\n")
        if (!graphoptions$cyclic & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext)) {
          if (nnext == first)
            top <- "A"
          else
            top <-first
          while (length(potential)>0 & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext) & nprev!=top ) {
            # cat("VISITED", visited,"\nPOTENTIAL", potential,"\n")
            potential <- potential[-1]
            visited<-append(visited,nprev)
            potential<- append(potential,setdiff(dotgetDirectAncestors(res, nprev),visited))
            nprev<-potential[1]
          }
          # cat("\tFINALLY",nprev,"-->",nnext,"\n")
        }
        res<-dotAddEdge(res,nprev,nnext[1])
        nprev <- nnext[1]
      }
      readline(cat("Saving graph\n"))
      dotExport(res,graphpwd)
      dotShow(res,graphpwd)
      saveRDS(res,paste(graphpwd,"/",id,".RDS",sep=""))
      ans<-readline("Press to next group (Y/N)")
      # if (ans != "Y")
      #   exit<-TRUE
      if (exit)
        break
    }
    if (exit)
      break
  }
  return (res)
}

GraphMinerCore<-function(data,group, level, graphoptions) {
  # readline("Preprocessing activities\n")
  # data<-gpreProcess(data,graphoptions)
  ugroup <- gsub(" ","_",group)
  cat("Creating graph for group ",group," and  ",level," problems solved\n")
  id<-paste(ugroup,"_",level,sep="")
  res<-dot(id,TRUE)
  res$engine <- graphoptions$engine
  res$title <- paste(group, level)
  dsxGroup <-data[data$Group == group, ]
  dsxGroup <- gRightUntil(dsxGroup,graphoptions, level)
  dsxGroup<-dsxGroup[order(dsxGroup$Start,decreasing = FALSE),]
  dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
  # Add nodes
  cat("Adding nodes ",group,"\n")
  res<-dotAddNode(res,"A","square")
  for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
    if (endsWith(n,graphoptions$leafid))
      res<-dotAddNode(res,n,"doublecircle")
    else
      res<-dotAddNode(res,n,"circle")
  }
  # Add arcs
  cat("Adding ",nrow(dsxGroupxP), " edges\n")
  nprev<-"A"
  first <-dsxGroupxP[1,graphoptions$fieldactivity]
  for (r in (1:nrow(dsxGroupxP))) {
    nnext<-dsxGroupxP[r,graphoptions$fieldactivity]
    potential<-c(nprev)
    visited<-c()
    nprev<-potential[1]
    # cat(r,") TRY ",nprev,"-->",nnext,"(",dotgetAllAncestors(res,nprev),")\n")
    if (!graphoptions$cyclic){
      if (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext) {
        if (nnext == first)
          top <- "A"
        else
          top <-first
        while (length(potential)>0 & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext) & nprev!=top ) {
          # cat("VISITED", visited,"\nPOTENTIAL", potential,"\n")
          potential <- potential[-1]
          visited<-append(visited,nprev)
          potential<- append(potential,setdiff(dotgetDirectAncestors(res, nprev),visited))
          nprev<-potential[1]
        }
      }
      # cat("\tFINALLY",nprev,"-->",nnext,"\n")
    }
    res<-dotAddEdge(res,nprev,nnext[1])
    nprev <- nnext[1]
  }
  return (res)
}


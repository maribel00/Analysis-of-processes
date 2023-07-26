graphoptions<-function(name,fieldactivity="Composite",minproblems=9,maxproblems=9) {
  return( list(problemname=name,maxproblems=maxproblems,minproblems=minproblems,fieldcase="Session",
               fieldagent="Group", fieldactivity=fieldactivity,
               fieldproblem="Problem",leafsuffix=".solved",rootsuffix=".fail",
               leafid=" OK", rootid=" FAIL",
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
  data$nID <- row.names(data)
  for (r in (1:nrow(data))) {
    prev <- data[data$nID <r,]
    nproblems <- length(unique(prev[endsWith(prev[[graphoptions$fieldactivity]],graphoptions$leafid),graphoptions$fieldproblem]))
    data[r,"psolved"] <- nproblems
  }
  return (data[data$psolved<=level,])
}

GraphMiner<-function(data,graphoptions,graphpwd) {
  #readline("Extracting groups\n")
  cat("Extracting groups\n")
  dsgroup <- unique(data[,"Group"])
  exit<-FALSE
  for (level in (graphoptions$minproblems:graphoptions$maxproblems)) {
    for (igroup in (1:length(dsgroup))) {
      group <- dsgroup[igroup]
      ugroup <- gsub(" ","_",group)
      #readline(cat("Creating graph for group ",group," up to ",level," problems solved\n"))
      cat("Creating graph for group ",group," up to ",level," problems solved\n")
      id<-paste(ugroup,"_",level,sep="")
      engine <- "None"
      if (graphoptions$fieldactivity == 'State')
        engine <- "circo"
      res<-dot(id,TRUE,engine)
      res$title <- paste(group, level)
      dsxGroup <-data[data$Group == group, ]
      dsxGroup <- gRightUntil(dsxGroup,graphoptions, level)
      dsxGroup<-dsxGroup[order(dsxGroup$Time,decreasing = FALSE),]
      # dsxGroup<-dsxGroup[order(dsxGroup$Start,decreasing = FALSE),]
      dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
      # Add nodes
      #readline(cat("Adding nodes ",group,"\n"))
      cat("Adding nodes ",group,"\n")
      res<-dotAddNode(res,"START","square")
      for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
        if (endsWith(n,graphoptions$leafid))
          res<-dotAddNode(res,n,"doublecircle")
        else
          res<-dotAddNode(res,n,"circle")
      }
      # Add arcs
      #readline(cat("Adding ",nrow(dsxGroupxP), " edges\n"))
      cat("Adding ",nrow(dsxGroupxP), " edges\n")
      nprev<-"START"
      first <-dsxGroupxP[1,graphoptions$fieldactivity]
      for (r in (1:nrow(dsxGroupxP))) {
        nnext<-dsxGroupxP[r,graphoptions$fieldactivity]
        potential<-c(nprev)
        visited<-c()
        nprev<-potential[1]
        if (!graphoptions$cyclic & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext)) {
          if (nnext == first)
            top <- "START"
          else
            top <-first
          while (length(potential)>0 & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext) & nprev!=top ) {
            potential <- potential[-1]
            visited<-append(visited,nprev)
            potential<- append(potential,setdiff(dotgetDirectAncestors(res, nprev),visited))
            nprev<-potential[1]
          }
        }
        res<-dotAddEdge(res,nprev,nnext[1])
        nprev <- nnext[1]
      }
      if (graphoptions$fieldactivity == 'State'){
        res<-dotAddNode(res,"END","square")
        res <- dotAddEdge(res,nprev,"END")
      }
      #readline(cat("Saving graph\n"))
      cat("Saving graph\n")
      if (graphoptions$fieldactivity == 'Composite')
        dotExportDISCO(res,graphpwd)
      else
        dotExportProblem(res,graphpwd)
      # dotShow(res,graphpwd)
      saveRDS(res,paste(graphpwd,"/",id,".RDS",sep=""))
      saveRDS(res,paste(pwd,"/Graphs/",gsub(" ","_",group),"_",level,"_",ifelse(data[1,"Grade"]<8.1,"LOW","GOOD"),".RDS",sep=""))
    }
  }
  
  return (res)
}

GraphMinerProblems<-function(data,graphoptions,graphpwd) {
  #readline("Extracting groups\n")
  cat("Extracting groups\n")
  dsgroup <- unique(data[,"Group"])
  exit<-FALSE
  for (level in (graphoptions$minproblems:graphoptions$maxproblems)) {
    for (igroup in (1:length(dsgroup))) {
      group <- dsgroup[igroup]
      ugroup <- gsub(" ","_",group)
      #readline(cat("Creating graph for group ",group," up to ",level," problems solved\n"))
      cat("Creating graph for group ",group," up to ",level," problems solved\n")
      id<-paste(ugroup,"_",level,sep="")
      engine <- "None"
      if (graphoptions$fieldactivity == 'State')
        engine <- "circo"
      res<-dot(id,TRUE,engine)
      res$title <- paste(group, level)
      dsxGroup <-data[data$Group == group, ]
      dsxGroup <- gRightUntil(dsxGroup,graphoptions, level)
      dsxGroup<-dsxGroup[order(dsxGroup$Time,decreasing = FALSE),]
      # dsxGroup<-dsxGroup[order(dsxGroup$Start,decreasing = FALSE),]
      dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
      # Add nodes
      #readline(cat("Adding nodes ",group,"\n"))
      cat("Adding nodes ",group,"\n")
      res<-dotAddNode(res,"START","square")
      for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
        if (endsWith(n,graphoptions$leafid))
          res<-dotAddNode(res,n,"doublecircle")
        else
          res<-dotAddNode(res,n,"circle")
      }
      res<-dotAddNode(res,"END","square")
      # Add arcs
      #readline(cat("Adding ",nrow(dsxGroupxP), " edges\n"))
      cat("Adding ",nrow(dsxGroupxP), " edges\n")
      nprev<-"START"
      first <-dsxGroupxP[1,graphoptions$fieldactivity]
      for (r in (1:nrow(dsxGroupxP))) {
        nnext<-dsxGroupxP[r,graphoptions$fieldactivity]
        potential<-c(nprev)
        visited<-c()
        nprev<-potential[1]
        res<-dotAddEdge(res,nprev,nnext[1])
        nprev <- nnext[1]
      }
      res <- dotAddEdge(res,nprev,"END")
      #readline(cat("Saving graph\n"))
      cat("Saving graph\n")
      if (graphoptions$fieldactivity == 'Composite')
        dotExportDISCO(res,graphpwd)
      else
        dotExportProblem(res,graphpwd)
      # dotShow(res,graphpwd)
      saveRDS(res,paste(graphpwd,"/",id,".RDS",sep=""))
    }
  }
  
  return (res)
}
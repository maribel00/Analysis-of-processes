GraphMinerSummary<-function(data,graphoptions,graphpwd) {
  cat("Extracting groups\n")
  dsgroup <- unique(data[,"Group"])
  exit<-FALSE
  level<-10
  for (igroup in (1:length(dsgroup))) {
    group <- dsgroup[igroup]
    ugroup <- gsub(" ","_",group)
    cat("Creating graph for group ",group," up to ",level," problems solved\n")
    id<-paste(ugroup,"_",level,sep="")
    res<-dot(id,TRUE)
    res$title <- paste(group, level)
    dsxGroupxP <-data[data$Group == group, ]
    # dsxGroup<-dsxGroup[order(dsxGroup$sTime,decreasing = FALSE),]
    # dsSessionNames = unique(dsxGroup[,"Session"])
    # dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
    # Add nodes
    cat("Adding nodes ",group,"\n")
    res<-dotAddNode(res,"START","square")
    for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
      if (endsWith(n,graphoptions$leafid))
        res<-dotAddNode(res,n,"doublecircle")
      else
        res<-dotAddNode(res,n,"circle")
    }
    # Add arcs
    cat("Adding ",nrow(dsxGroupxP), " edges\n")
    nprev<-"START"
    first <-dsxGroupxP[1,graphoptions$fieldactivity]
    for (r in (1:nrow(dsxGroupxP))) {
      nnext<-dsxGroupxP[r,graphoptions$fieldactivity]
      potential<-c(nprev)
      visited<-c()
      nprev<-potential[1]
      # cat(r,") TRY ",nprev,"-->",nnext,"(",dotgetAllAncestors(res,nprev),")\n")
      if (!graphoptions$cyclic & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext)) {
        if (nnext == first)
          top <- "START"
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
    cat("Saving graph\n")
    dotExportDISCO(res,graphpwd)
    # dotShow(res,graphpwd)
    saveRDS(res,paste(graphpwd,"/",id,".RDS",sep=""))
  }
  return (res)
}

# GraphMinerSummary<-function(data,graphoptions,graphpwd) {
#   cat("Extracting groups\n")
#   dsgroup <- unique(data[,"Group"])
#   level <- 10
#   for (igroup in (1:length(dsgroup))) {
#     group <- dsgroup[igroup]
#     ugroup <- gsub(" ","_",group)
#     cat("Creating graph for group ",group," up to ",level," problems solved\n")
#     id<-paste(ugroup,"_",level,sep="")
#     engine <- "None"
#     res<-dot(id,TRUE)
#     res$title <- paste(group, level)
#     dsxGroup <-data[data$Group == group, ]
#     dsxGroup<-dsxGroup[order(dsxGroup$sTime,decreasing = FALSE),]
#     dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
#     # Add nodes
#     cat("Adding nodes ",group,"\n")
#     res<-dotAddNode(res,"START","square")
#     for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
#       if (endsWith(n,graphoptions$leafid))
#         res<-dotAddNode(res,n,"doublecircle")
#       else
#         res<-dotAddNode(res,n,"circle")
#     }
#     # Add arcs
#     cat("Adding ",nrow(dsxGroupxP), " edges\n")
#     nprev<-"START"
#     first <-dsxGroupxP[1,graphoptions$fieldactivity]
#     for (r in (1:nrow(dsxGroupxP))) {
#       nnext<-dsxGroupxP[r,graphoptions$fieldactivity]
#       potential<-c(nprev)
#       visited<-c()
#       nprev<-potential[1]
#       if (!is.na(nnext) & !graphoptions$cyclic & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext)) {
#         if (nnext == first)
#           top <- "START"
#         else
#           top <-first
#         while (length(potential)>0 & (nnext %in% dotgetAllAncestors(res,nprev) | nprev == nnext) & nprev!=top ) {
#           potential <- potential[-1]
#           visited<-append(visited,nprev)
#           potential<- append(potential,setdiff(dotgetDirectAncestors(res, nprev),visited))
#           nprev<-potential[1]
#         }
#       }
#       res<-dotAddEdge(res,nprev,nnext[1])
#       nprev <- nnext[1]
#     }
#     cat("Saving graph\n")
#     dotExportDISCO(res,graphpwd)
#     # dotShow(res,graphpwd)
#     saveRDS(res,paste(graphpwd,"/",id,".RDS",sep=""))
#   }
# 
#   return (res)
# }

# Añadir un summary con dotexport problem

GraphMinerProblems<-function(data,graphoptions,graphpwd) {
  cat("Extracting groups\n")
  dsgroup <- unique(data[,"Group"])
  for (level in (graphoptions$minproblems:graphoptions$maxproblems)) {
    for (igroup in (1:length(dsgroup))) {
      group <- dsgroup[igroup]
      ugroup <- gsub(" ","_",group)
      cat("Creating graph for group ",group," up to ",level," problems solved\n")
      id<-paste(ugroup,"_",level,sep="")
      engine <- "None"
      if (graphoptions$fieldactivity == 'Composite')
        engine <- "circo"
      res<-dot(id,TRUE,engine)
      res$title <- paste(group, level)
      dsxGroup <-data[data$Group == group, ]
      dsxGroup <- gRightUntil(dsxGroup,graphoptions,level)
      dsxGroup<-dsxGroup[order(dsxGroup$Start,decreasing = FALSE),]
      dsxGroupxP<-dsxGroup[dsxGroup$psolved<=graphoptions$maxproblems, ]
      # Add nodes
      cat("Adding nodes ",group,"\n")
      res<-dotAddNode(res,"START","square")
      for (n in unique(dsxGroupxP[[graphoptions$fieldactivity]])) {
        if (endsWith(n,graphoptions$leafid))
          res<-dotAddNode(res,n,"circle")
        else
          res<-dotAddNode(res,n,"circle")
      }
      res<-dotAddNode(res,"END","square")
      # Add arcs
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
      cat("Saving graph\n")
      if (graphoptions$fieldactivity == 'Composition')
        dotExportDISCO(res,graphpwd)
      else
        dotExportProblem(res,graphpwd)
      # dotShow(res,graphpwd)
      saveRDS(res,paste(graphpwd,"/",id,".RDS",sep=""))
    }
  }
  
  return (res)
}
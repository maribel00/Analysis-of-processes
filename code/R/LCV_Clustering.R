LCV_ListOutliers<-function(data, variable, value) {
  bp <- LCV_boxplot(data,variable, value)
  show(bp)
  cat("\nOutliers: ")
  outl<-layer_data(bp)$outliers
  if (length(outl)>0){
    for (i in (1:length(outl))) {
      if (length(outl[[i]])>0){
        for (iout in (1:length(outl[[i]]))) {
          cat(outl[[i]][iout]," ")
        }
      }
    }
  }
}


LCV_RemoveOutliers<-function(data, bplot) {
  cat("\nRemoving outliers")
  outl<-layer_data(bp)$outliers
  if (length(outl)>0){
    for (i in (1:length(outl))) {
      if (length(outl[[i]])>0){
        for (iout in (1:length(outl[[i]]))) {
          cat("Removing outlier ",outl[[i]][iout]," ")
          data<-data[data[[col]]!=outl[[i]][iout],]
        }
      }
    }
  }
  return(data)
}


LCV_AlphaCutClusters <- function(data, cutx, cuty, clustering="red", radius="", xpartition=c()) {
  # Build its own data frame based on rx and ry to gain independence from original dataset
  cteradius=10
  rvariable<- data[[cutx]]
  rvalue <- data[[cuty]]
  rcaption<-""
  if (clustering %in% colnames(data)) {
    cluster <- factor(data[[clustering]])
    rcaption <- paste(paste("Color by",clustering))
  }else{
    cluster <- clustering
  }
  if (radius %in% colnames(data)) {
    crmax <-max(as.numeric(data[[radius]]))
    cradius <- 5*as.numeric(data[[radius]])/crmax
    rcaption <- paste(rcaption,"\n",paste("Radius by",radius))
  }else{
    cluster <- clustering
    cradius=1
  }
  rplot <-ggplot(data,mapping=aes(x=rvariable,y=rvalue, colour=data[[cluster]]))+
    geom_point(aes(size=cradius),alpha=0.5)+
    scale_color_manual(values=c("red","blue","green","purple","orange","brown"))+
    labs(caption=rcaption)+
    xlab(cutx)+
    ylab(cuty)

  if (length(xpartition)>0) {
    rplot <- rplot+
      geom_vline(xintercept = xpartition)
  }

  rplot <- rplot+
    LCV_theme2
  rplot
}


LCV_KMeans<-function(data, id_column=c("Group"),main_kpi="np", other_kpi=c(), compare_to=c(), force_nc=-1, sort_clusters=FALSE) {
  idclus <- paste("KMEANS_",main_kpi,sep="")
  idclusord <-paste(idclus,"_ord",sep="")
  # KM.data <<- data[,c(ids,metrics)]
  # NbClust(data[,metrics],distance="euclidean",min.nc=2, max.nc = 10, method="kmeans")
  all_metrics <-c(main_kpi, other_kpi)
  data_clust<-data.frame(data[,all_metrics])
  if (force_nc<0) {
    nbc<-NbClust(data_clust,distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
    fnc<-length(unique(nbc$Best.partition))
    # summary(nbc)
    LCV_NBclust <<- nbc
  } else {
    fnc <- force_nc
    LCV_NBclust <<- c()
  }
  LCV_Kmeans <<- kmeans(data_clust,fnc,nstart=fnc)
  nc <-LCV_Kmeans$cluster
  nclist <- unique(nc)
  # Sort clusters by main_kpi
  pdf <- data.frame(id=data[[id_column]],idvalues=data_clust[,1], clustid=as.factor(nc))
  show(pdf)
  cldf<-data.frame(clustername=character(0),clusterid=numeric(0), minvalue=numeric(0))
  for (ic in (nclist)) {
    dclus <- pdf[pdf$clustid==ic,]
    cmin <- round(min(dclus$idvalues),2)
    cmax <- round(max(dclus$idvalues),2)
    cmed<- round(median(dclus$idvalues),2)
    cldf[nrow(cldf)+1,]<-c(clustername=paste("[",cmin,",",cmax,"]",sep=""), clusterid=ic,minvalue=cmin)
  }
  cldf <- cldf[order(cldf$minvalue),]
  show(cldf)
  ordernc <- c()
  orderncorder<-c()
  for (i in (nc)) {
    ordernc <- append(ordernc,cldf[cldf$clusterid==i,"clustername"])
    orderncorder<- append(orderncorder,cldf[cldf$clusterid==i,"minvalue"])
  }
  show(ordernc)
  data[[idclus]] <- ordernc
  data[[idclusord]] <- orderncorder
  cat("SUMMARY: ", fnc, " partitions, Accuracy fit$betweenss/fit$totss= ",LCV_Kmeans$betweenss/LCV_Kmeans$totss)

  # data[["nKM"]] <- 0
  # for (row in (1:nrow(data))) {
  #   data[row,"nKM"]<- nc[row]
  # }

  for (clus in unique(ordernc)) {
    cat(paste("\n\nCluster found ",clus,"\n"))
    cat(data[(data[[idclus]] == clus),c(id_column)], sep=", ")
  }
  return(data)
}

############################################

doSIIEClusterQuartile<-function(data, id_column=c("Group"),main_kpi="np", other_kpi=c(), compare_to=c("Grade","m","nnp",	"rt",	"ot",	"ns"	,"nns"	,"ps"	,"es"	,"eo"	,"ef"	,"fr")) {
  idclus <- paste("CLUS_",main_kpi,sep="")
  # KM.data <<- data[,c(ids,metrics)]
  # NbClust(data[,metrics],distance="euclidean",min.nc=2, max.nc = 10, method="kmeans")
  all_metrics <-c(main_kpi, other_kpi)
  QM <- quantile(data[[main_kpi]], 0,0.25,0.5, 0.75, 1)
  data_clust<-data[,all_metrics]
  if (force_nc<0) {
    nbc<-NbClust(data_clust,distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
    fnc<-length(unique(nbc$Best.partition))
    data[[idclus]] <- as.factor(nbc$Best.partition)
  } else {
    fnc <- force_nc
    nc <-kmeans(data_clust,fnc,nstart=fnc)$cluster
    data[[idclus]] <- as.factor(nc)
  }
  # data[["nKM"]] <- 0
  # for (row in (1:nrow(data))) {
  #   data[row,"nKM"]<- nc[row]
  # }

  for (clus in unique(nc)) {
    cat(paste("\n\nCluster found ",clus))
    show(data[(data[[idclus]] == clus),c(id_column)])
  }
  LCV_Tomato_Theme()
  for (sm in all_metrics) {
    show(LCV_boxplot(data,idclus,sm, order=TRUE, orderby = main_kpi))
  }
  LCV_Apple_Theme()
  for (sm in compare_to) {
    show(LCV_boxplot(data,idclus,sm, order=TRUE, orderby = main_kpi))
  }
  c5vardataset <- data[,c(compare_to)]
  c5Output <- data[[idclus]]
  tree<<-C5.0(x=c5vardataset,y=c5Output, trials=100, rules=FALSE)
  rules <<-C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
  # show(rules$rules)
  # show(tree$tree)

  # show(data[,c("Group","fKM")])
  data
}


LCV_ClusterColumn<- function(data, col, nc=-1, remove_outliers=FALSE) {
  readline("Pulse para ver Sesiones originales")
  show(LCV_SelfValues(data, col))
  data <- LCV_KMeans(data, main_kpi = col, force_nc = nc)
  newcol<-paste("KMEANS_",col, sep="")
  show(LCV_histogram(data,paste(newcol,sep=""),orderby=paste(newcol,"_ord",sep="")))
  bp <- LCV_boxplot(data, newcol, col, order=TRUE)
  if (remove_outliers) {
    cat("\nRemoving outliers")
    outl<-layer_data(bp)$outliers
    if (length(outl)>0){
      for (i in (1:length(outl))) {
        if (length(outl[[i]])>0){
          for (iout in (1:length(outl[[i]]))) {
            cat("Removing outlier ",outl[[i]][iout]," ")
            data<-data[data[[col]]!=outl[[i]][iout],]
          }
        }
      }
    }
    data <- LCV_KMeans(data, main_kpi = col, force_nc = nc)
    show(LCV_histogram(data,paste(newcol,sep=""),orderby=paste(newcol,"_ord",sep="")))
    bp <- LCV_boxplot(data, newcol, col, order=TRUE)
  }
  xpartition_s <- layer_data(bp)$ymin
  show(LCV_SelfValues(data, col, xpartition = xpartition_s))
  cat("\n\nPartition: ",xpartition_s," Accuracy fit$betweenss/fit$totss= ",LCV_Kmeans$betweenss/LCV_Kmeans$totss)
  cat("\nOutliers: ")
  outl<-layer_data(bp)$outliers
  if (length(outl)>0){
    for (i in (1:length(outl))) {
      if (length(outl[[i]])>0){
        for (iout in (1:length(outl[[i]]))) {
          cat(outl[[i]][iout]," ")
        }
      }
    }
  }
  return (data)
}


# LCV_Tomato_Theme()
# for (sm in all_metrics) {
#   show(LCV_boxplot(data,idclus,sm, order=TRUE, orderby = main_kpi))
# }
# LCV_Apple_Theme()
# for (sm in compare_to) {
#   show(LCV_boxplot(data,idclus,sm, order=TRUE, orderby = main_kpi))
# }
# c5vardataset <- data[,c(compare_to)]
# c5Output <- data[[idclus]]
# tree<<-C5.0(x=c5vardataset,y=c5Output, trials=100, rules=FALSE)
# rules <<-C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
# show(rules$rules)
# show(tree$tree)

# show(data[,c("Group","fKM")])

# LCV_KMeans<-function(data, id_column=c("Group"),main_kpi="np", other_kpi=c(), compare_to=c(), force_nc=-1) {
#   idclus <- paste("KMEANS_",main_kpi,sep="")
#   # KM.data <<- data[,c(ids,metrics)]
#   # NbClust(data[,metrics],distance="euclidean",min.nc=2, max.nc = 10, method="kmeans")
#   all_metrics <-c(main_kpi, other_kpi)
#   data_clust<-data[,all_metrics]
#   if (force_nc<0) {
#     nbc<-NbClust(data_clust,distance="euclidean", min.nc=2, max.nc=10, method="kmeans")
#     fnc<-length(unique(nbc$Best.partition))
#     data[[idclus]] <- as.factor(nbc$Best.partition)
#     summary(nbc)
#     LCV_Kmeans <<- kmeans(data_clust,fnc,nstart=fnc)
#     nc <-LCV_Kmeans$cluster
#     cat("LCV Accuracy fit$betweenss/fit$totss= ",LCV_Kmeans$betweenss/LCV_Kmeans$totss)
#     LCV_NBclust <<- nbc
#   } else {
#     fnc <- force_nc
#     LCV_Kmeans <<- kmeans(data_clust,fnc,nstart=fnc)
#     cat("LCV Accuracy fit$betweenss/fit$totss= ",LCV_Kmeans$betweenss/LCV_Kmeans$totss)
#     nc <-LCV_Kmeans$cluster
#     data[[idclus]] <- as.factor(nc)
#     LCV_NBclust <<- c()
#   }
#
#   # data[["nKM"]] <- 0
#   # for (row in (1:nrow(data))) {
#   #   data[row,"nKM"]<- nc[row]
#   # }
#
#   for (clus in unique(nc)) {
#     cat(paste("\n\nCluster found ",clus))
#     show(data[(data[[idclus]] == clus),c(id_column)])
#   }
#   LCV_Tomato_Theme()
#   for (sm in all_metrics) {
#     show(LCV_boxplot(data,idclus,sm, order=TRUE, orderby = main_kpi))
#   }
#   LCV_Apple_Theme()
#   for (sm in compare_to) {
#     show(LCV_boxplot(data,idclus,sm, order=TRUE, orderby = main_kpi))
#   }
#   # c5vardataset <- data[,c(compare_to)]
#   # c5Output <- data[[idclus]]
#   # tree<<-C5.0(x=c5vardataset,y=c5Output, trials=100, rules=FALSE)
#   # rules <<-C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
#   # show(rules$rules)
#   # show(tree$tree)
#
#   # show(data[,c("Group","fKM")])
#   return(data)
# }




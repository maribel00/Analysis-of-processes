
LCV_density <- function (data, variable, nintervals=0, showextremes=FALSE, showmax=FALSE, showmean=FALSE, showall=FALSE) {
  ddataset <- data
  dvariable <- ddataset[[variable]]
  d<-density(dvariable)
  fd <- approxfun(d)
  besty <- which.max(d$y)
  bestx <- d$x[besty]
  maxVar <-max(dvariable)
  minVar <-min(dvariable)
  sumy <- sum(d$y)
  sumx <- 0
  for (i in (1:length(d$x))) {
    sumx <- sumx + d$x[i]*d$y[i]
  }
  cogx <- sumx/sumy
  if (nintervals == 0 ) {
    fvariable <- as.factor(dvariable)
    ddata <- data.frame(fvariable)
    res<-ggplot(ddataset,aes(x=dvariable))+
      geom_density(color=textcolor,fill=fillcolor,alpha=alphafill)
    if (showall  | showmax) {
      res<- res+geom_segment(aes(x=bestx, xend=bestx, y=0, yend=fd(bestx)), linetype="dashed", lineend = "round", color ="black")
      tmax<- paste("\nMAX prob: ",bestx)
    }else{
      tmax<-""
    }
    if (showall  | showmean) {
      res <- res+geom_segment(aes(x=cogx, xend=cogx, y=0, yend=fd(cogx)), linetype="solid", lineend = "round", color ="black")
      tmean<- paste("\nCOM: ",cogx)
    }else{
      tmean<-""
    }
    res <- res+
      labs(caption=paste(tmax,tmean))+
      xlab(variable)+
      ylab("Density")+
      LCV_theme2
    # ddata <- data.frame(dvariable)
    # res <- ggplot(ddataset,aes(x=dvariable))+
    #   geom_density(color=textcolor,fill=fillcolor,alpha=alphafill)+
    #   xlab(variable)+
    #   ylab("Density")+
    #   LCV_theme
  } else {
    dhisto <- hist(dvariable, breaks=nintervals)
    maxVal <- max(dhisto$counts)
    minVal <-min(dhisto$counts)
    fvariable <- as.factor(dvariable)
    ddata <- data.frame(fvariable)
    nticks<-nintervals
    res<-ggplot(ddataset,aes(x=dvariable))+
      geom_histogram(binwidth=(maxVar-minVar)/nintervals,aes(y=after_stat(count)))+
      geom_density(color=textcolor,fill=fillcolor,alpha=alphafill, aes(y=after_stat(maxVal*count/max(count))))+
      geom_vline(aes(xintercept = bestx, y=d$y[bestx]), linetype="dashed")+
      # geom_vline(xintercept = cogx, color="red")+
      geom_text(mapping = aes(x=cogx, y=0),label="X")+
      geom_text(mapping = aes(x=bestx, y=0),label="^")+
      labs(caption=paste("^ MAX = ",round(bestx,2),"\nx COG=",round(cogx,2)))+
      xlab(variable)+
      scale_x_continuous(n.breaks=nticks)+
      ylab("Density")+
      LCV_theme2
  }
  res
}

# LCV_histogram <- function (data, variable) {
LCV_densities <- function (data, variables=c(), showmax=TRUE) {
  ddataset <- data.frame(variable=character(0),value=numeric(0))
  for (v in variables){
    dfaux <-data.frame(variable=v,value=data[[v]])
    ddataset<- rbind(ddataset, dfaux)
  }
  res<-ggplot(data=ddataset,aes(x=value,fill=variable))+geom_density(alpha=0.25)+LCV_theme2
  if (showmax) {
    for (v in variables){
      dvariable <- ddataset[[v]]
      d<-density(dvariable)
      besty <- which.max(d$y)
      bestx <- d$x[besty]
      res<- res+geom_segment(aes(x=bestx, xend=bestx, y=0, yend=fd(bestx)), linetype="dashed", lineend = "round", color =v)
    }

  }
  res
}

LCV_MosaicDensitiesFactor<-function (d,v=c(),f) {
  library(patchwork)
  LCV_Mosaic<-LCV_densitiesFactor(d,v[1],f)
  for (iv in v[2:length(v)]) {
    lplot <- LCV_densitiesFactor(d,iv,f)
    LCV_Mosaic <- LCV_Mosaic + lplot
  }
  LCV_Mosaic+plot_layout(nc=3)
}

LCV_MosaicDensities<-function (d,v=c()) {
  library(patchwork)
  LCV_Mosaic<-LCV_density(d,v[1],showmax = TRUE)
  for (iv in v[2:length(v)]) {
    lplot <- LCV_density(d,iv,showmax = TRUE)
    LCV_Mosaic <- LCV_Mosaic + lplot
  }
  LCV_Mosaic+plot_layout(nc=4)
}


LCV_MosaicBoxPlots<-function (d,v=c(),f) {
  LCV_Mosaic<<-LCV_boxplot(d,f,v[1])
  for (iv in v[2:length(v)]) {
    LCV_Mosaic <<- LCV_Mosaic + LCV_boxplot(d,f,iv)
  }
  LCV_Mosaic+plot_layout(nc=3)
}


LCV_MosaicLRegression<-function (d,v=c(),fa) {
  library(patchwork)
  LCV_Mosaic<-LCV_LinearRegression(d,x=v[1],y=fa, full = TRUE)
  for (iv in v[2:length(v)]) {
    lplot <- LCV_LinearRegression(d,iv,fa, full = TRUE)
    LCV_Mosaic <- LCV_Mosaic + lplot
  }
  LCV_Mosaic+plot_layout(nc=3)
}


LCV_densitiesFactor <- function (data, variable, factor, showmax=FALSE, annotationsx=c(), annotationstxt=c(), annotationsy=c(),xpartition=c()) {
  df<- data.frame()
  df <- data.frame(rbind(cvariable=data[[variable]], cfactor=as.factor(data[[factor]])))
  uniques <- data[0,factor];
  for (i in (1:nrow(data))) {
    # if (nrow(data[i,factor])>1) {
    if (! data[i,factor] %in% uniques){
      uniques <- append(uniques, data[i,factor])
    }
    # }
  }
  v <- data[[variable]]
  cluster <- as.factor(data[[factor]])

  res<-ggplot(data, aes(x=v,fill=cluster,..scaled..))+
    geom_density(alpha=0.25)+
    xlab(factor)+
    ylab(variable)
  if (length(xpartition)>0) {
    res <- res+
      geom_vline(xintercept = xpartition)
  }

  if (showmax) {
    for (ic in (1:length(uniques))) {
      c = uniques[ic]
      fcolor<- LCV_colors[ic]
      ss <- data[data[[factor]]==c,]
      dvariable <- ss[[variable]]
      if (length(dvariable)>1){
        cat("processing factor", c, paste(dvariable, sep="-"), "\n")
        d<-density(dvariable)
        fd <- approxfun(d)
        besty <- which.max(d$y)
        bestx <- d$x[besty]
        maxVar <-max(dvariable)
        minVar <-min(dvariable)
        sumy <- sum(d$y)
        sumx <- 0
        for (i in (1:length(d$x))) {
          sumx <- sumx + d$x[i]*d$y[i]
        }
        cogx <- sumx/sumy
        fvariable <- as.factor(dvariable)
        ddata <- data.frame(fvariable)
        res<-res+geom_vline(xintercept = bestx,linetype="dashed")
        res <- res + annotate("text",x=bestx,y=-0.02*ic,fontface=2,label=paste("[",ic,"]\n",round(bestx,2),sep = ""))
      } else {
        bestx<-dvariable[1]
        res<-res+geom_vline(xintercept = bestx,linetype="dashed")
        if (ic%%2==0) {
          # res <- res + geom_text(mapping=aes(x=bestx,y=0),label=paste("[",ic,"]=",bestx,sep = ""))
        }else{
          # res <- res + geom_text(mapping=aes(x=bestx,y=1),label=paste("[",ic,"]=",bestx,sep=""))
        }
      }
    }
  }
  for (i in (1:length(annotationstxt))) {
    as <- annotationstxt[i]
    ax <- annotationsx[i]
    ay <- 0  #annotationsy[i]
    res<-res+geom_vline(xintercept = ax,linetype="dotted" ,color="red")
    res <- res + annotate("text",x=ax,y=ay,fontface=2,label=as)
  }
  res<-res+LCV_theme2
  res
}
LCV_SelfValues <- function(data, variable, xpartition=c(), clustering="red") {
  v <- data[[variable]]
  if (clustering %in% colnames(data)) {
    res<-ggplot()+geom_point(mapping=aes(x=v, y=0),size=3,colour=as.factor(data[[clustering]]), alpha=0.25)+
      # scale_color_manual(values=c("red","blue","green","purple","orange","brown", "gray"))+
      xlab(variable)
  }else{
    res<-ggplot()+geom_point(mapping=aes(x=v, y=0),size=3,colour="red", alpha=0.25)+
      xlab(variable)
  }
  if (length(xpartition)>0) {
    res <- res+
      geom_vline(xintercept = xpartition)
  }
  res<-res+
    LCV_theme2
  res
}
# LCV_SelfValues <- function(data, variable, xpartition=c()) {
#   v <- data[[variable]]
#   res<-ggplot()+
#    geom_point(mapping=aes(x=v, y=0),size=3,color="red", alpha=0.25)+
#     xlab(variable)
#   if (length(xpartition)>0) {
#     res <- res+
#       geom_vline(xintercept = xpartition)
#   }
#   res<-res+
#     LCV_theme2
#   res
# }

LCV_getFDensity<-function(data, variable){
  ddataset <- data
  dvariable <- ddataset[[variable]]
  d<-density(dvariable)
  fd <- approxfun(d)
  fd
}

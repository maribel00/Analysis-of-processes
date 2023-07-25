source("LCV_Theme.R")
library(dplyr)
library(scales)
LCV_histogram <- function (data, variable, nintervals=0, mybreaks=c(), columnlabels=c(),force_factor=FALSE, orderby="") {
  if (orderby != "") {
    dorder<-data[[orderby]]
    dvariable <- data[[variable]]
  }else {
    dorder<-dvariable
    dvariable <- data[[variable]]
  }
  if (force_factor) {
    dvariable <- as.factor(dvariable)
  }
  if (is.factor(dvariable) || nintervals ==  0)  {
    if (length(mybreaks)==0) {
      res<-ggplot(data)+
        geom_bar(stat="count",aes(x=dvariable),fill=fillcolor, color=textcolor,alpha=alphafill)+
        geom_text(stat="count", aes(x=dvariable,label=..count..),color=textcolor,vjust=1.75,size=3 )+
        xlab(variable)+
        LCV_theme2
          LCV_theme2
    }  else {
      # res<-ggplot(data, mapping=aes(x=dvariable))+
      #   geom_text(stat="count", aes(label=..count..),color=textcolor,vjust=-0.5)+
      #   xlab(variable)+
      #   geom_bar(fill=fillcolor, color=textcolor,alpha=alphafill)+
      #   scale_x_continuous(breaks=mybreaks)+
      #   LCV_theme2
    }
  } else {
    maxVar <-max(dvariable)
    minVar <-min(dvariable)
    fvariable <- as.factor(dvariable)
    dhisto <- hist(dvariable, breaks=nintervals)
    maxVal <- max(dhisto$counts)
    minVal <-min(dhisto$counts)
    nticks<-nintervals
    res <- ggplot(ddataset,aes(x=dvariable))+
      geom_histogram(binwidth=(maxVar-minVar)/nintervals,aes(y=after_stat(count)),fill = fillcolor, color=textcolor,alpha=alphafill)+
      xlab(variable)+
      scale_x_continuous(n.breaks=nticks)+
      ylab("Frequency")+
      LCV_theme2
  }
  # theme_classic()
  res
}

LCV_boxplot <- function(data, variable, value, group="", log=FALSE,
                        order=FALSE, orderby=value, summary=FALSE) {
  dcaption <-"  "
  if (nchar(group)>0) {
    dataset <- subset(data,select = c(group,variable, value, orderby))
    dvariable <- dataset[[variable]]
    dvalue <- dataset[[value]]
    cat("Before: ", dvariable)
    if (order) {
      dvariable <- fct_reorder(dvariable, dataset[[orderby]])
      dcaption <- paste("Sorted asc by ",orderby)
    }
    cat("After: ", dvariable)
    res <- ggplot(dataset,mapping=aes(x=dvariable,y=dvalue,colour=group))+
      geom_boxplot(fill = fillcolor, alpha=alphafill)+
      xlab(variable)+
      ylab(value)+
      labs(caption=dcaption)+
      LCV_theme2

  } else {
    # dataset <- subset(data,select = c(group,variable, value, orderby))
    dataset <- data[,c(variable, value, orderby)]
    dvariable <- dataset[[variable]]
    dvalue <- dataset[[value]]
    if (order) {
      dvariable <- fct_reorder(dvariable, dataset[[orderby]])
      dcaption <- paste("Sorted asc by ",orderby)
    }
    if (log) {
      res <- ggplot(dataset,mapping=aes(x=dvariable,y=dvalue))+
        geom_boxplot(fill = fillcolor, color=textcolor,alpha=alphafill)+
        xlab(variable)+
        ylab(value)+
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) +
        labs(caption=dcaption)+
        LCV_theme2
    } else {
      res <- ggplot(dataset,mapping=aes(x=dvariable,y=dvalue))+
        geom_boxplot(fill = fillcolor, color=textcolor,alpha=alphafill)+
        xlab(variable)+
        ylab(value)+
        labs(caption=dcaption)+
        LCV_theme2
    }
  }
  show(res)
  if (summary) {
    summary(res)
    layer_data(res)
  }
  res
}

# LCV_histogram <- function (data, variable, nintervals=0, mybreaks=c(), columnlabels=c(),force_factor=FALSE) {
#   ddataset <- data
#   dvariable <- ddataset[[variable]]
#   if (force_factor) {
#     dvariable <- as.factor(dvariable)
#   }
#   ddata <- data.frame(fvariable)
#   mybreaks<-c()
#   if (is.factor(dvariable) || nintervals ==  0)  {
#     if (length(mybreaks)==0) {
#       res<-ggplot(data=ddata, mapping=aes(x=fvariable))+
#         geom_text(stat="count", aes(label=..count..),color=textcolor,vjust=-0.75,size=3 )+
#         xlab(variable)+
#         geom_bar(fill=fillcolor, color=textcolor,alpha=alphafill)+
#         LCV_theme2
#     }  else {
#       res<-ggplot(data=ddata, mapping=aes(x=fvariable))+
#         geom_text(stat="count", aes(label=..count..),color=textcolor,vjust=-0.5)+
#         xlab(variable)+
#         geom_bar(fill=fillcolor, color=textcolor,alpha=alphafill)+
#         scale_x_continuous(breaks=mybreaks)+
#         LCV_theme2
#     }
#     res
#   } else {
#     maxVar <-max(dvariable)
#     minVar <-min(dvariable)
#     fvariable <- as.factor(dvariable)
#     dhisto <- hist(dvariable, breaks=nintervals)
#     maxVal <- max(dhisto$counts)
#     minVal <-min(dhisto$counts)
#     nticks<-nintervals
#     res <- ggplot(ddataset,aes(x=dvariable))+
#       geom_histogram(binwidth=(maxVar-minVar)/nintervals,aes(y=after_stat(count)),fill = fillcolor, color=textcolor,alpha=alphafill)+
#       xlab(variable)+
#       scale_x_continuous(n.breaks=nticks)+
#       ylab("Frequency")+
#       LCV_theme2
#   }
#   # theme_classic()
#   res
# }
#

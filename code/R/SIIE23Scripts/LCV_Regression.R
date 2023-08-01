source("SIIE23Scripts/LCV_Theme.R")

# LCV_MultipleLinearRegression <- function(dataset, y, x=c(), xdoshow=TRUE, numpoints=500) {
#   # Build its own data frame based on rx and ry to gain independence from original dataset
#   # rvariable<- data[[x]]
#   # rvalue <- data[[y]]
#   data <- dataset
#   data$ly<-data[[y]]
#   data$lx<-data[[x[1]]]
#   # rvariable <- ""
#   # data$lx <- data[[x[1]]]
#   tit<-x[1]
#   for (m in x[2:length(x)]) {
#     data$lx <- data$lx + data[[m]]
#     tit<-paste(tit,"+",m)
#   }
#   # define the regression model and find its main parameters
#   rmodel <- lm(data$ly~data$lx)
#   rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
#   rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
#   rcx <- rmodel$coefficients[2] # regression coefficients
#   ri <- rmodel$coefficients[1]
#   rmse <- mean(rmodel$residuals^2) # Mean Square Error
#   rr2<-summary(rmodel)$r.squared # R2
#   rtext<-paste("y=",round(rcx,2),"x","+",round(ri,2)) # textual formula
#   # Calculates the new points of the regressoin
#   # rdf <- data.frame(data)
#   mn<- min(data$lx)
#   mx<-max(data$lx)
#   newx <- seq(mn,mx,(mx-mn)/numpoints)
#   newy<-predict(rmodel,newdata=data.frame(x=seq(mn,mx,(mx-mn)/numpoints)))
#   pdf<-data.frame(px=newx,py=newy)
#   # Since it is lineal, also calculates Pearson's R and its pvalue
#   # pearson <-cor.test(x=rdf$rx, y=rdf$ry, method=("pearson"))
#   # rpearson<-pearson$estimate
#   # ppearsonpvalue <- pearson$p.value
#   rplot<-ggplot(data)+
#     geom_point(data,color="black", mapping=aes(x=lx, y=ly))+
#    # geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))+
#   labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue))
#   rplot<-rplot+
#     geom_smooth(method='lm', mapping=aes(x=lx,y=ly),formula=y~x,color=fillcolor,se=F )
#   rplot<-rplot+
#     xlab(tit)+
#     ylab(y)+
#     LCV_theme2
#
#
#   if (doshow) {
#     show(rplot)
#   }
#   data
#   data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue)
# }

LCV_MultipleLinearRegression <- function(dataset, x, yfactor = "") {
  # res <- LCV_LinearRegressionBase(dataset,x)
  # for (if in (1:length(yfactor)) {
  #
  # }
  # return (res)
}


LCV_LinearRegressionBase <- function(data, x, xpartition=c()) {
  # Build its own data frame based on rx and ry to gain independence from original dataset
  rvariable<- data[[x]]

  rplot<-ggplot()+
    xlab(x)
  if (length(xpartition)>0) {
    rplot <- rplot+
      geom_vline(xintercept = xpartition)
  }
  rplot<-rplot +
    LCV_theme2
  return(rplot)
}

LCV_FactorLinearRegression <- function(data, x, y,xpartition=c(),ypartition=c(),factorby="BAD",pal=c(),doshow=TRUE, numpoints=500, full=TRUE) {
  # Build its own data frame based on rx and ry to gain independence from original dataset
  series <- unique(data[[factorby]])
  rx <- data[[x]]
  ry <- data[[y]]
  colors=c("red", "black", "black", "black")
  # pal=c()
  # for (ifctr in (1:length(series))) {
  #   pal[series[ifctr]] = colors[ifctr]
  # }
  for (ifctr in (1:length(series))) {
    fctr <- series[ifctr]
    fdata <- data[data[[factorby]]==fctr,]
    rvariable<- fdata[[x]]
    rvalue <- fdata[[y]]
    # rx<-rvariable
    # ry<-rvalue
    rdf <- data.frame(rx=rvariable,ry=rvalue)
    # define the regression model and find its main parameters
    rmodel <- lm(ry~rx,rdf)
    rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
    rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
    rcx <- rmodel$coefficients[2] # regression coefficients
    ri <- rmodel$coefficients[1]
    rmse <- mean(rmodel$residuals^2) # Mean Square Error
    rr2<-summary(rmodel)$r.squared # R2
    rtext<-paste("y=",round(rcx,2),"x","+",round(ri,2)) # textual formula
    # Calculates the new points of the regressoin
    newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
    newy<-predict(rmodel,newdata=data.frame(rx=newx))
    # build a new df based on tht regression
    pdf<-data.frame(rx=newx,ry=newy)
    # Since it is lineal, also calculates Pearson's R and its pvalue
    pearson <-cor.test(x=rdf$rx, y=rdf$ry, method=("pearson"))
    rpearson<-pearson$estimate
    ppearsonpvalue <- pearson$p.value

    if (ifctr==1) {
      rplot<-ggplot()+
        geom_point(data, mapping=aes(rx,ry, size=1,color=as.factor(data[[factorby]])),alpha=0.5)+
        xlab(x)+
        ylab(y)+
        geom_point(pdf, color=pal[ifctr], mapping=aes(x=rx,y=ry,))+
        # scale_fill_discrete(type = LCV_colors)

      # ggplot(tdf_winners, aes(x = edition, y = time_overall, fill = winner_name_lbl)) + geom_col() +
      scale_color_manual(values = pal)

      # rplot<-ggplot(rdf)+
      #   geom_point(aes(x=rx,y=ry))+
      #   scale_color_manual(values = rcolor)+
      #   xlab(x)+
      #   ylab(y)+
      #   labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue,"\nPearson=",rpearson,"\tp=",ppearsonpvalue))+
      #   geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))



            # theme(legend.position="bottom")

      if (length(xpartition)>0) {
        rplot <- rplot+
          geom_vline(xintercept = xpartition)
      }
    }
    else {

      rplot<-rplot+
        geom_point(pdf, color=pal[ifctr], mapping=aes(x=rx,y=ry,))

      # rplot<-rplot+
      # geom_point(colour=as.factor(data[[factorby]]),data=pdf,mapping=aes(x=rx,y=ry))
      #   geom_point(data=pdf, mapping=aes(rx,ry, size=2,color=as.factor(data[[factorby]])),alpha=0.75)
         # geom_point(alpha=100,mapping=aes(data=pdf,x=rx,y=ry,color=as.factor(data[[factorby]])))
      # geom_point(data=rdf,mapping=aes(x=rx,y=ry, size=1), color=LCV_colors[ifctr], alpha=0.5)+
      # geom_point(color=LCV_colors[ifctr],alpha=100,data=pdf,aes(x=rx,y=ry))
    }

  }
  if (length(ypartition)>0) {
    rplot <- rplot+
      geom_hline(yintercept = ypartition)
  }

  rplot<-rplot +
    LCV_theme2


  show(rplot)
  return(rplot)
}

LCV_LinearRegression <- function(data, x, y,xpartition=c(),ypartition=c(),rcolor="black",doshow=FALSE, numpoints=500, full=FALSE) {
  # Build its own data frame based on rx and ry to gain independence from original dataset
  rvariable<- data[[x]]
  rvalue <- data[[y]]
  # rx<-rvariable
  # ry<-rvalue
  rdf <- data.frame(rx=rvariable,ry=rvalue)
  # define the regression model and find its main parameters
  rmodel <- lm(ry~rx,rdf)
  rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
  rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
  rcx <- rmodel$coefficients[2] # regression coefficients
  ri <- rmodel$coefficients[1]
  rmse <- mean(rmodel$residuals^2) # Mean Square Error
  rr2<-summary(rmodel)$r.squared # R2
  rtext<-paste("y=",round(rcx,2),"x","+",round(ri,2)) # textual formula
  # Calculates the new points of the regressoin
  newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
  newy<-predict(rmodel,newdata=data.frame(rx=newx))
  # build a new df based on tht regression
  pdf<-data.frame(rx=newx,ry=newy)
  # Since it is lineal, also calculates Pearson's R and its pvalue
  pearson <-cor.test(x=rdf$rx, y=rdf$ry, method=("pearson"))
  rpearson<-pearson$estimate
  ppearsonpvalue <- pearson$p.value

  if (full) {
    rplot<-ggplot(rdf)+
      geom_point(aes(x=rx,y=ry))+
      scale_color_manual(values = rcolor)+
      xlab(x)+
      ylab(y)+
      labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue,"\nPearson=",rpearson,"\tp=",ppearsonpvalue))+
      geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
  } else {
    rplot<-ggplot(rdf)+
      geom_point(aes(x=rx,y=ry),color=rcolor)+
      xlab(x)+
      ylab(y)+
      geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
  }
  if (length(xpartition)>0) {
    rplot <- rplot+
      geom_vline(xintercept = xpartition)
  }
  if (length(ypartition)>0) {
    rplot <- rplot+
      geom_hline(yintercept = ypartition)
  }

  rplot<-rplot +
    LCV_theme2
  if (doshow) {
    show(rplot)
  }

  # data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue,pearson=rpearson,pp=ppearsonpvalue)
  return(rplot)
}


LCV_PolynomialRegression <- function(data, x, y,gradepolynomial=3,xpartition=c(),ypartition=c(),rcolor="black",doshow=FALSE, numpoints=500, full=FALSE) {
  # Build its own data frame based on rx and ry to gain independence from original dataset
  rvariable<- data[[x]]
  rvalue <- data[[y]]
  # rx<-rvariable
  # ry<-rvalue
  rdf <- data.frame(rx=rvariable,ry=rvalue)
  # define the regression model and find its main parameters
  rmodel <- lm(ry~rx+I(rx^2),rdf)
  rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
  rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
  rmse <- mean(rmodel$residuals^2) # Mean Square Error
  rr2<-summary(rmodel)$r.squared # R2
  cx0 <- rmodel$coef[1]
  cx1 <- rmodel$coef[2]
  cx2 <- rmodel$coef[3]
  rtext<-paste("y=",round(cx2,2),"x²","+",round(cx1,2),"x","+",round(cx0,2)) # textual formula
  plynomial <- function(x) cx2*x^2+cx1*x+cx0
  # Calculates the new points of the regressoin
  newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
  newy<-predict(rmodel,newdata=data.frame(rx=newx))
  # build a new df based on tht regression
  pdf<-data.frame(rx=newx,ry=newy)
  # Since it is lineal, also calculates Pearson's R and its pvalue
  pearson <-cor.test(x=rdf$rx, y=rdf$ry, method=("pearson"))
  rpearson<-pearson$estimate
  ppearsonpvalue <- pearson$p.value

  if (full) {
    rplot<-ggplot(rdf)+
      geom_point(aes(x=rx,y=ry))+
      scale_color_manual(values = rcolor)+
      xlab(x)+
      ylab(y)+
      labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue,"\nPearson=",rpearson,"\tp=",ppearsonpvalue))+
      geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
    # stat_function(fun=plynomial, colour="white")
  } else {
    rplot<-ggplot(rdf)+
      geom_point(aes(x=rx,y=ry),color=rcolor)+
      xlab(x)+
      ylab(y)+
      geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
  }
  if (length(xpartition)>0) {
    rplot <- rplot+
      geom_vline(xintercept = xpartition)
  }
  if (length(ypartition)>0) {
    rplot <- rplot+
      geom_hline(yintercept = ypartition)
  }

  rplot<-rplot +
    LCV_theme2
  if (doshow) {
    show(rplot)
  }

  data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue,pearson=rpearson,pp=ppearsonpvalue)
  rplot
}

# LCV_PolynomialRegression <- function(data, x, y,gradepolynomial=3,xpartition=c(),ypartition=c(),rcolor="black",doshow=FALSE, numpoints=500, full=FALSE) {
#   # Build its own data frame based on rx and ry to gain independence from original dataset
#   rvariable<- data[[x]]
#   rvalue <- data[[y]]
#   # rx<-rvariable
#   # ry<-rvalue
#   rdf <- data.frame(rx=rvariable,ry=rvalue)
#   # define the regression model and find its main parameters
#   rmodel <- lm(ry~poly(rx,gradepolynomial,raw=TRUE),rdf)
#   rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
#   rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
#   rcx <- rmodel$coefficients[2] # regression coefficients
#   ri <- rmodel$coefficients[1]
#   rmse <- mean(rmodel$residuals^2) # Mean Square Error
#   rr2<-summary(rmodel)$r.squared # R2
#   rtext<-paste("y=",round(rcx,2),"x","+",round(ri,2)) # textual formula
#   # Calculates the new points of the regressoin
#   newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
#   newy<-predict(rmodel,newdata=data.frame(rx=newx))
#   # build a new df based on tht regression
#   pdf<-data.frame(rx=newx,ry=newy)
#   # Since it is lineal, also calculates Pearson's R and its pvalue
#   pearson <-cor.test(x=rdf$rx, y=rdf$ry, method=("pearson"))
#   rpearson<-pearson$estimate
#   ppearsonpvalue <- pearson$p.value
#
#   if (full) {
#     rplot<-ggplot(rdf)+
#       geom_point(aes(x=rx,y=ry))+
#       scale_color_manual(values = rcolor)+
#       xlab(x)+
#       ylab(y)+
#       labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue,"\nPearson=",rpearson,"\tp=",ppearsonpvalue))+
#       geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
#   } else {
#     rplot<-ggplot(rdf)+
#       geom_point(aes(x=rx,y=ry),color=rcolor)+
#       xlab(x)+
#       ylab(y)+
#       geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
#   }
#   if (length(xpartition)>0) {
#     rplot <- rplot+
#       geom_vline(xintercept = xpartition)
#   }
#   if (length(ypartition)>0) {
#     rplot <- rplot+
#       geom_hline(yintercept = ypartition)
#   }
#
#   rplot<-rplot +
#     LCV_theme2
#   if (doshow) {
#     show(rplot)
#   }
#
#   data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue,pearson=rpearson,pp=ppearsonpvalue)
#   rplot
# }

LCV_ExponentialRegression <- function(data, x, y,xpartition=c(),ypartition=c(),rcolor="black",doshow=FALSE, numpoints=500, full=FALSE) {
  # Build its own data frame based on rx and ry to gain independence from original dataset
  rvariable<- data[[x]]
  rvalue <- data[[y]]
  # rx<-rvariable
  # ry<-rvalue
  rdf <- data.frame(rx=rvariable,ry=rvalue)
  # define the regression model and find its main parameters
  rmodel <- lm(log(ry)~rx,rdf)
  rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
  rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
  rcx <- rmodel$coefficients[2] # regression coefficients
  ri <- rmodel$coefficients[1]
  rmse <- mean(rmodel$residuals^2) # Mean Square Error
  rr2<-summary(rmodel)$r.squared # R2
  rtext<-paste("y=",round(rcx,2),"x","+",round(ri,2)) # textual formula
  # Calculates the new points of the regressoin
  newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
  newy<-exp(predict(rmodel,newdata=data.frame(rx=newx)))
  # build a new df based on tht regression
  pdf<-data.frame(rx=newx,ry=newy)
  # Since it is lineal, also calculates Pearson's R and its pvalue
  pearson <-cor.test(x=rdf$rx, y=rdf$ry, method=("pearson"))
  rpearson<-pearson$estimate
  ppearsonpvalue <- pearson$p.value

  if (full) {
    rplot<-ggplot(rdf)+
      geom_point(aes(x=rx,y=ry))+
      scale_color_manual(values = rcolor)+
      xlab(x)+
      ylab(y)+
      labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue,"\nPearson=",rpearson,"\tp=",ppearsonpvalue))+
      geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
  } else {
    rplot<-ggplot(rdf)+
      geom_point(aes(x=rx,y=ry),color=rcolor)+
      xlab(x)+
      ylab(y)+
      geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))
  }
  if (length(xpartition)>0) {
    rplot <- rplot+
      geom_vline(xintercept = xpartition)
  }
  if (length(ypartition)>0) {
    rplot <- rplot+
      geom_hline(yintercept = ypartition)
  }

  rplot<-rplot +
    LCV_theme2
  if (doshow) {
    show(rplot)
  }

  data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue,pearson=rpearson,pp=ppearsonpvalue)
  rplot
}




#
# LCV_ExponentialRegression <- function(data, x, y, doshow=FALSE, numpoints=500) {
#   # Build its own data frame based on rx and ry to gain independence from original dataset
#   rvariable<- data[[x]]
#   rvalue <- log(data[[y]])
#   rdf <- data.frame(rx=rvariable,ry=rvalue)
#   # define the regression model and find its main parameters
#   rmodel <- lm(ry~rx,rdf)
#   rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
#   rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
#   rcx <- rmodel$coefficients[2] # regression coefficients
#   ri <- rmodel$coefficients[1]
#   rmse <- mean(rmodel$residuals^2) # Mean Square Error
#   rr2<-summary(rmodel)$r.squared # R2
#   rtext<-paste("y= e^(",round(rcx,2),"x","+",round(ri,2),")") # textual formula
#   # Calculates the new points of the regressoin
#   newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
#   newy<-exp(predict(rmodel,newdata=data.frame(rx=newx)))
#   # build a new df based on tht regression
#   pdf<-data.frame(rx=newx,ry=newy)
#   rplot<-ggplot(rdf)+
#     geom_point(color="black",aes(x=rx,y=ry))+
#     geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))+
#     xlab(x)+
#     ylab(y)+
#     labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue))+
#     LCV_theme
#   if (doshow) {
#     show(rplot)
#   }
#   data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue)
# }
#
#
# LCV_PolynomialRegression <- function(data, x, y, doshow=FALSE, numpoints=500, gradepolynomial=3) {
#   # Build its own data frame based on rx and ry to gain independence from original dataset
#   rvariable<- data[[x]]
#   rvalue <- data[[y]]
#   rdf <- data.frame(rx=rvariable,ry=rvalue)
#   # define the regression model and find its main parameters
#   rmodel <- lm(ry~poly(rx,gradepolynomial,raw=TRUE),rdf)
#   rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
#   rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
#   rcx <- rmodel$coefficients[2] # regression coefficients
#   ri <- rmodel$coefficients[1]
#   rmse <- mean(rmodel$residuals^2) # Mean Square Error
#   rr2<-summary(rmodel)$r.squared # R2
#   rtext<-paste("y= e^(",round(rcx,2),"x","+",round(ri,2),")") # textual formula
#   # Calculates the new points of the regressoin
#   newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
#   newy<-predict(rmodel,newdata=data.frame(rx=newx))
#   # build a new df based on tht regression
#   pdf<-data.frame(rx=newx,ry=newy)
#   rplot<-ggplot(rdf)+
#     geom_point(color="black",aes(x=rx,y=ry))+
#     geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))+
#     xlab(x)+
#     ylab(y)+
#     labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue))+
#     LCV_theme
#   if (doshow) {
#     show(rplot)
#   }
#   data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue)
# }
#
# LCV_LogarithmicRegression <- function(data, x, y, doshow=FALSE, numpoints=500) {
#   # Build its own data frame based on rx and ry to gain independence from original dataset
#   rvariable<- data[[x]]
#   rvalue <- data[[y]]
#   rdf <- data.frame(rx=rvariable,ry=rvalue)
#   # define the regression model and find its main parameters
#   rmodel <- lm(exp(ry)~rx,rdf)
#   rf <- summary(rmodel)$fstatistic # needed to find pvalue of regression
#   rpvalue <- pf(rf[1],rf[2],rf[3],lower.tail=F) # pvalue of regression
#   rcx <- rmodel$coefficients[2] # regression coefficients
#   ri <- rmodel$coefficients[1]
#   rmse <- mean(rmodel$residuals^2) # Mean Square Error
#   rr2<-summary(rmodel)$r.squared # R2
#   rtext<-paste("y= e^(",round(rcx,2),"x","+",round(ri,2),")") # textual formula
#   # Calculates the new points of the regressoin
#   newx <- seq(min(rdf$rx),max(rdf$rx),(max(rdf$rx)-min(rdf$rx))/numpoints)
#   newy<-log(predict(rmodel,newdata=data.frame(rx=newx)))
#   # build a new df based on tht regression
#   pdf<-data.frame(rx=newx,ry=newy)
#   rplot<-ggplot(rdf)+
#     geom_point(color="black",aes(x=rx,y=ry))+
#     geom_point(color=fillcolor,data=pdf,mapping=aes(x=rx,y=ry))+
#     xlab(x)+
#     ylab(y)+
#     labs(caption=paste(rtext,"\nMSE=",round(rmse,3),"\tR2=",round(rr2,3),"\tpvalue=",rpvalue))+
#     LCV_theme
#   if (doshow) {
#     show(rplot)
#   }
#   data.frame(formula=rtext,mse=rmse,r2=rr2,rp=rpvalue)
# }
#



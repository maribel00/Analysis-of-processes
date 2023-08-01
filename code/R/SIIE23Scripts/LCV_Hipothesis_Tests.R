library(reshape2)

source("SIIE23Scripts/LCV_Theme.R")

LCV_Pre_ANOVA <- function(data,variable, value) {
  dataset<-data
  Avariable<-variable
  Avalue<-value
  boxplotX<-Avariable
  boxplotY <- Avalue
  excludeRows<-c()

  dataset.sel <- subset(dataset,select = c(boxplotX, boxplotY))
  # dataset.unique<-dataset.sel[!(dataset.sel$Year %in% excludeRows),]
  dataset.melt<-dataset.sel
  show(LCV_density(dataset.melt,value,10))
  show(LCV_boxplot(dataset.melt,boxplotX,boxplotY))
  shapiro.test(dataset.sel[[Avalue]])

}

LCV_ANOVA <- function(data,variable, value) {
  dataset<-data
  Avariable<-variable
  Avalue<-value
  boxplotX<-Avariable
  boxplotY <- Avalue
  excludeRows<-c()
  show(LCV_boxplot(data,variable,value))
  dataset.sel <- subset(dataset,select = c(boxplotX, boxplotY))
  dataset.unique<-dataset.sel[!(dataset.sel$Year %in% excludeRows),]
  dataset.melt<-dataset.unique
  dataset.lm <- lm(dataset.melt[[Avalue]]~dataset.melt[[Avariable]],dataset.melt)

  dataset.kw <- kruskal.test(dataset.melt[[Avalue]]~dataset.melt[[Avariable]],dataset.melt)
  show(dataset.kw)

  dataset.aov <- aov(dataset.lm)
  show(summary(dataset.aov))
  plot(dataset.aov$residuals)
  boxplot(dataset.aov$residuals)
  qqnorm(dataset.aov$residuals)
  qqline(dataset.aov$residuals)
  dataset.aov
}

LCV_STUDENT <- function(data,variable, value, onetail=FALSE, paired=FALSE) {
  dataset<-data
  Avariable<-variable
  Avalue<-value
  boxplotX<-Avariable
  boxplotY <- Avalue
  excludeRows<-c()
  show(LCV_boxplot(data,variable,value))
  dataset.sel <- subset(dataset,select = c(boxplotX, boxplotY))
  dataset.unique<-dataset.sel[!(dataset.sel$Year %in% excludeRows),]
  dataset.melt<-dataset.unique
  x<-dataset[dataset[[variable]]=="YES",c(value)]
  y<-dataset[dataset[[variable]]=="NO",c(value)]
  dataset.wt <- wilcox.test(x=x,y=y)
  if (onetail){
    dataset.tt <- t.test(x=x, y=y, paired = paired)
  }else{
    dataset.tt <- t.test(x=x, y=y, alt="two.sided", paired = paired)
  }
  cat("%%% WILCOX")
  show(dataset.wt)
  cat("%%% STUDENT")
  show(dataset.tt)

  dataset.tt
}


LCV_STUDENT2 <- function(x,y, onetail=FALSE, paired=FALSE) {
  dataset.wt <- wilcox.test(x=x,y=y)
  if (onetail){
    dataset.tt <- t.test(x=x, y=y, paired = paired)
  }else{
    dataset.tt <- t.test(x=x, y=y, alt="two.sided", paired = paired)
  }
  cat("%%% WILCOX")
  show(dataset.wt)
  cat("%%% STUDENT")
  show(dataset.tt)

  dataset.tt
}


LCV_TUKEY <- function(data,variable, value) {
  data.tukey <- TukeyHSD(LCV_ANOVA(data,variable,value))
  show(data.tukey)
  tukeydf <- data.frame(data.tukey$`dataset.melt[[Avariable]]`)
  cat(">>>>>>>>>>>> TUEKY HSD [",min(tukeydf$p.adj),",",max(tukeydf$p.adj),"]")
  ggHSD(data.tukey)+
    theme_bw()+
    ggtitle(paste("Confidence levels of ",value))+
  theme(
    title = element_text(size=0),
    axis.text.x = element_text(size=12, color=textcolor),
    axis.text.y = element_text(size=10,angle=0),
    plot.title = element_text(size=18,face="bold",angle=0),
  )+
    LCV_theme2
}

LCV_confidence_intervals <- function(data, variable, value) {
  dataset<-data
  Avariable<-variable
  Avalue<-value
  boxplotX<-Avariable
  boxplotY <- Avalue
  dvalue<-as.vector(data[[value]])
  dvariable <-as.vector(data[[variable]])
  dataset.lm <- lm(dvalue~dvariable)
  dataset.aov <- aov(dataset.lm)
  res <- TukeyHSD(dataset.aov)
  show(res)
  show(GGTukey(res))
  # tuk_plot(res,"Confidence levels",Avalue)
  res

}



LCV_Normality <-function(data, variable) {
  ddataset <- data
  dvariable <- ddataset[[variable]]
  show(shapiro.test(dvariable))
  show(ks.test(dvariable, "pnorm", mean = mean(dvariable), sd=sd(dvariable)))
show(LCV_density(data,variable,15))

}


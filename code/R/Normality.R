library(xtable)
library('ggpubr')

source("LCV_Theme.R")
source("LCV_Bayes.R")
source("LCV_plotting.R")
source("LCV_Hipothesis_Tests.R")

# Normalidad de los datos
setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
ndataset <- read.csv("MBSessionsExt.csv", header = TRUE)
head(ndataset)
names(ndataset) <- c("Year","Group","Fail","Solved","Allsessions")
head(ndataset)

nVariable <- "Year"
nValue <- "Allsessions"
ndsp <- ndataset[,c(nVariable,nValue)]
ndataset.lm <- lm(ndsp[[nValue]]~ndsp[[nVariable]], ndsp)
ndataset.aov <- aov(ndataset.lm)
summary <- summary(ndataset.aov)
print(xtable(as.matrix(summary)), include.rownames = TRUE)

ndataset.tukey<-TukeyHSD(ndataset.aov)
ndataset.tukey
print(xtable(as.matrix(ndataset.tukey$`ndsp[[nVariable]]`)), include.rownames = TRUE)

plot(ndataset.tukey)
plot(ndataset.aov$residuals)
boxplot(ndataset.aov$residuals, main=paste("Residuals ",nValue))
hist(ndataset.aov$residuals, main = paste("Residuals ",nValue))
ggdensity(ndataset[[nValue]],xlab=nValue)
ggqqplot(ndataset[[nValue]], xlab = nValue)

LCV_Tomato_Theme()
LCV_confidence_intervals(ndataset, nVariable, nValue)

LCV_Lime_Theme()
LCV_boxplot(ndataset, nVariable, nValue)

boxplot <- boxplot(Allsessions~Year,ndataset)
plot <- ggplot(data=ndataset,mapping=aes(x=Year,y=Allsessions))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

LCV_Heaven_Theme()
LCV_density(ndataset, nValue, showall = TRUE)

mdataset <- read.csv("MBSessionsProblems.csv", header = TRUE)
head(mdataset)
names(mdataset) <- c("Year","Problem","Sessions")

LCV_Lime_Theme()
LCV_boxplot(mdataset, "Problem", "Sessions")

boxplot <- boxplot(Sessions~Problem,mdataset)
plot <- ggplot(data=mdataset,mapping=aes(x=Problem,y=Sessions))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

ndataset <- read.csv("MBSessionsProblemExt.csv", header = TRUE)
head(ndataset)
names(ndataset) <- c("Year","Problem","Fail","Solved","Allsessions","FailRatio")
head(ndataset)

nVariable <- "Problem"
nValue <- "FailRatio"
ndsp <- ndataset[,c(nVariable,nValue)]
ndataset.lm <- lm(ndsp[[nValue]]~ndsp[[nVariable]], ndsp)
ndataset.aov <- aov(ndataset.lm)
summary <- summary(ndataset.aov)
print(xtable(as.matrix(summary)), include.rownames = TRUE)

ndataset.tukey<-TukeyHSD(ndataset.aov)
ndataset.tukey
print(xtable(as.matrix(ndataset.tukey$`ndsp[[nVariable]]`)), include.rownames = TRUE)

plot(ndataset.tukey)

LCV_Tomato_Theme()
LCV_confidence_intervals(ndataset, nVariable, nValue)

boxplot <- boxplot(FailRatio~Problem,ndataset)
plot <- ggplot(data=ndataset,mapping=aes(x=Problem,y=FailRatio))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

LCV_Lime_Theme()
LCV_boxplot(ndataset, "Problem", "FailRatio")

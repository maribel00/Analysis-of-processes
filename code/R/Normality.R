library(xtable)
library('ggpubr')

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

boxplot <- boxplot(Allsessions~Year,ndataset)
plot <- ggplot(data=ndataset,mapping=aes(x=Year,y=Allsessions))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

mdataset <- read.csv("MBSessionsProblems.csv", header = TRUE)

boxplot <- boxplot(Session~Problem,mdataset)
plot <- ggplot(data=mdataset,mapping=aes(x=Problem,y=Session))+geom_boxplot(fill = "olivedrab1")+theme_bw()
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

boxplot <- boxplot(FailRatio~Problem,ndataset)
plot <- ggplot(data=ndataset,mapping=aes(x=Problem,y=FailRatio))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

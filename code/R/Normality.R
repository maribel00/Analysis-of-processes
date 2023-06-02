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

grid(nx=16, ny=16)
par(new = TRUE)
boxplot(Allsessions~Year,
        data=ndataset,
        main="Number of sessions per year",
        xlab="Year",
        ylab="Number of sessions"
)

mdataset <- read.csv("MBSessionsProblems.csv", header = TRUE)

grid(nx=16, ny=16)
par(new = TRUE)
boxplot(Session~Problem,
        data=mdataset,
        main="Number of sessions per problem",
        xlab="Problem",
        ylab="Number of sessions"
)

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

grid(nx=16, ny=16)
par(new = TRUE)
boxplot(FailRatio~Problem,
        data=ndataset,
        main="Fail ratio per problem",
        xlab="Problem",
        ylab="Fail ratio"
)

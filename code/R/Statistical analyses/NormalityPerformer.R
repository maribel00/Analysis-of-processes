library(readr)
library(xtable)
library('ggpubr')

source("LCV_Theme.R")
source("LCV_Bayes.R")
source("LCV_plotting.R")
source("LCV_Hipothesis_Tests.R")

rdataset <- data

boxplot <- boxplot(Performer~Year,rdataset)
plot <- ggplot(data=rdataset,mapping=aes(x=Year,y=Performer))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

Variable <- "Year"
Value <- "p"
rdataset.lm <- lm(rdataset[[Value]]~rdataset[[Variable]], rdataset)
rdataset.aov <- aov(rdataset.lm)
rdataset.aov
summary <- summary(rdataset.aov)
summary
print(xtable(as.matrix(summary)), include.rownames = TRUE)

LCV_ANOVA(rdataset,"Year","p")

LCV_Lime_Theme()
LCV_boxplot(rdataset, Variable, Value)

rdataset.tukey<-TukeyHSD(rdataset.aov)
rdataset.tukey
print(xtable(as.matrix(rdataset.tukey$`rdataset[[Variable]]`)), include.rownames = TRUE)

LCV_Heaven_Theme()
LCV_density(rdataset, Value, showall = TRUE)

plot(rdataset.tukey)
plot(rdataset.aov$residuals)
boxplot(rdataset.aov$residuals, main=paste("Residuals ",Value))
hist(rdataset.aov$residuals, main = paste("Residuals ",Value))
ggdensity(rdataset[[Value]],xlab=Value)
ggqqplot(rdataset[[Value]], xlab =Value)

LCV_Tomato_Theme()
LCV_confidence_intervals(rdataset, Variable, Value)

LCV_Orange_Theme()
LCV_histogram(rdataset,"p")

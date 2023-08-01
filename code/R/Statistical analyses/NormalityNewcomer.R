library(readr)
library(xtable)
library('ggpubr')

setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("LCV_Theme.R")
source("LCV_Bayes.R")
source("LCV_plotting.R")
source("LCV_Hipothesis_Tests.R")
source("SIIE2023.R")

rdataset <- doLoadData()
rdataset <- rdataset[rdataset$s < 1000,]
rdataset <- rdataset[rdataset$p > 6,]
nrow(rdataset)
head(rdataset)

boxplot <- boxplot(Newcomer~Year,rdataset)
plot <- ggplot(data=rdataset,mapping=aes(x=Year,y=Newcomer))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

Variable <- "Year"
Value <- "ot"
rdataset.lm <- lm(rdataset[[Value]]~rdataset[[Variable]], rdataset)
rdataset.aov <- aov(rdataset.lm)
summary <- summary(rdataset.aov)
print(xtable(as.matrix(summary)), include.rownames = TRUE)

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

LCV_ANOVA(rdataset, "Year", "ot")
# Kruskal-Wallis rank sum test

# data:  dataset.melt[[Avalue]] by dataset.melt[[Avariable]]
# Kruskal-Wallis chi-squared = 29.624, df = 6, p-value =
#  4.633e-05
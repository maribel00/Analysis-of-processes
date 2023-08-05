setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE23master.R")
library(xtable)
library('ggpubr')

# data <- readBaseDataset()
# data <- data[data$Level == 10,]
data <- SIIE23doLoadData()

Variable <- "Year"
Value <- "s"
data.lm <- lm(data[[Value]]~data[[Variable]], data)
data.aov <- aov(data.lm)
summary <- summary(data.aov)
print(xtable(as.matrix(summary)), include.rownames = TRUE)

plot(data.aov$residuals)

LCV_Lime_Theme()
df <- data.frame(residuals = data.aov$residuals)
LCV_boxplot_one(df, df$residuals, "residuals")

LCV_Orange_Theme()
LCV_histogram(df, variable = "residuals",nintervals = 7)
ggqqplot(data[[Value]], xlab = Value, color = "midnightblue", ggtheme = LCV_theme2)

LCV_Heaven_Theme()
LCV_density(data, "s", showall = TRUE)

LCV_Lime_Theme()
LCV_boxplot(data, Variable, Value)

data <- data[data$s < 1000,]
data <- data[data$p > 6,]

LCV_boxplot(data, Variable, Value)

LCV_ANOVA(data,"Year","s")

LCV_Heaven_Theme()
LCV_Normality(data, "s")

data.lm <- lm(data[[Value]]~data[[Variable]], data)
data.aov <- aov(data.lm)
data.tukey<-TukeyHSD(data.aov)
data.tukey
print(xtable(as.matrix(data.tukey$`data[[Variable]]`)), include.rownames = TRUE)

LCV_Tomato_Theme()
LCV_confidence_intervals(data, Variable, Value)
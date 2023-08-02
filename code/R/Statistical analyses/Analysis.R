setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE2023.R")
library(tidyr)
library(xtable)
library(lubridate)

SIIE23doInitDatasets()
SIIE23doLoadSessions()
head(SIIE23RAW)

# Fail Ratio

agg_data <- SIIE23RAW %>% group_by(Group, Problem, OutCome) %>% count()
head(agg_data)

agg_data <- pivot_wider(agg_data, names_from = OutCome, values_from = n)
agg_data <- agg_data %>% mutate(all = fail + solved)
agg_data <- agg_data %>% mutate(fr = fail/all)

data <- data.frame(agg_data$Group, agg_data$Problem, agg_data$fr)
data <- data %>% drop_na()
colnames(data) <- c("Group","Problem","fr")
head(data)

LCV_Lime_Theme()
LCV_boxplot(data, "Problem", "fr")

Variable <- "Problem"
Value <- "fr"
data.lm <- lm(data[[Value]]~data[[Variable]], data)
data.aov <- aov(data.lm)
summary <- summary(data.aov)
summary
print(xtable(as.matrix(summary)), include.rownames = TRUE)

LCV_ANOVA(data,"Problem","fr")

data.tukey<-TukeyHSD(data.aov)
data.tukey
print(xtable(as.matrix(data.tukey$`data[[Variable]]`)), include.rownames = TRUE)

LCV_Tomato_Theme()
LCV_confidence_intervals(data, Variable, Value)

kruskal.test(fr ~ Problem, data = data)

# Duración

agg_data <- SIIE23RAW
agg_data$SStart <- dmy_hms(agg_data$SStart)
agg_data$SEnd <- dmy_hms(agg_data$SEnd)
head(agg_data)

# Ordenar el dataframe por 'Group' y 'SEnd'
begin_time <- agg_data %>%
  group_by(Group, Problem) %>%
  mutate(start = min(SStart))
begin_time <- unique(begin_time[,c("Group","Problem","start")])
head(begin_time)

end_time <- agg_data %>%  arrange(Group, SEnd)

end_time <- end_time[end_time$OutCome=="solved",] %>% group_by(Group, Problem) %>% slice(1)
head(end_time)
end_time <- end_time[,c("Group","Problem","SEnd")]
nrow(end_time)

# Combinar los datasets por las columnas 'Group' y 'Problem'
dataset <- merge(begin_time, end_time, by = c("Group", "Problem"))
dataset <- dataset %>% mutate(duration = as.numeric(difftime(SEnd, start), units = "hours"))
dataset <- dataset[dataset$duration < 2500,]
head(dataset)

LCV_Lime_Theme()
LCV_boxplot(dataset, "Problem", "duration")

Variable <- "Problem"
Value <- "duration"
data.lm <- lm(dataset[[Value]]~dataset[[Variable]], dataset)
data.aov <- aov(data.lm)
summary <- summary(data.aov)
summary
print(xtable(as.matrix(summary)), include.rownames = TRUE)

LCV_ANOVA(dataset,"Problem","duration")

data.tukey<-TukeyHSD(data.aov)
data.tukey
print(xtable(as.matrix(data.tukey$`dataset[[Variable]]`)), include.rownames = TRUE)

LCV_Tomato_Theme()
LCV_confidence_intervals(dataset, Variable, Value)

kruskal.test(duration ~ Problem, data = dataset)
# Kruskal-Wallis rank sum test

# data:  duration by Problem
# Kruskal-Wallis chi-squared = 29.341, df = 8, p-value = 0.0002763
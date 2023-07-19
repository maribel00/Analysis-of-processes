setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
library(dplyr)

source("LCV_Theme.R")
source("LCV_plotting.R")
source("LCV_Hipothesis_Tests.R")

data <- read.delim2("Dataset Unificado15-21V5 - Dataset.tsv")
cat("OK loaded ",nrow(data), " rows")

agg_data <- aggregate(data[, c('Session', 'Grupo', 'problem', 'Milestone')], list(data$sTime), tail, 1)
agg_data <- data %>% select('Session', 'Grupo', 'problem', 'Milestone')
agg_data <- data %>% 
  mutate(state = if_else(Milestone == 5, "OK", "FAIL"))
agg_data <- agg_data %>% group_by(Grupo, problem, state) %>% count()

problems <- agg_data %>% ungroup %>% select('Grupo','problem','n')
problems <- problems %>% group_by(Grupo, problem) %>% summarise_all(list(sum))

complete <- merge(agg_data, problems, by = c('Grupo', 'problem'), all.x = TRUE, all.y = TRUE, all = TRUE)
complete <- complete %>% filter(state == 'FAIL')
complete <- complete %>% mutate(fr = n.x/n.y)
complete <- complete %>% select(-state, -n.x, -n.y)

LCV_Lime_Theme()
LCV_boxplot(complete, 'problem', 'fr')

setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')
library(dplyr)

data <- read.delim2("Dataset Unificado15-21V5 - Dataset.tsv")
cat("OK loaded ",nrow(data), " rows")

data <- data %>% 
  mutate(state = if_else(Milestone == 5, "OK", "FAIL"))
head(data)

data <- aggregate(data[, c('Session', 'problem', 'state')], list(data$Session), tail, 1)

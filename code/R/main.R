# setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("Load.R")
source("Dot.R")
source("GraphMiner.R")
library(stringr)
library(dplyr)

SIIE23doInitDatasets()
SIIE23doLoadSessions()
# Asignación de nuevos nombres a las columnas del data frame
colnames(SIIE23RAW) <- c('Group','Year','Time','Session','Problem','Milestone','Composite','Grade','Size','relDays','relHours','X','X_')
head(SIIE23RAW)

data <- SIIE23RAW
# Crear la nueva columna "State"
data <- data %>%
  mutate(State = ifelse(Milestone > 4, paste(Problem, "OK"), paste(Problem, "FAIL")))
head(data)

graphoptions1 <- graphoptions('SIIE2023_1',maxproblems=10)
graphoptions2 <- graphoptions('SIIE2023_2',fieldactivity='Problem')
graphoptions3 <- graphoptions('SIEE2023_3',fieldactivity='State') 
graphoptions4 <- graphoptions('SIIE2023_4',fieldactivity='State',minproblems=1,maxproblems=10)

graphpwd1 <- "./Graphs"
graph1 <- GraphMiner(data, graphoptions1, graphpwd1)
graphpwd2 <- "./GraphsProblems"
graph2 <- GraphMinerProblems(data, graphoptions2, graphpwd2)
graphpwd3 <- "./GraphsStates"
graph3 <- GraphMinerProblems(data, graphoptions3, graphpwd3)
graphpwd4 <- "./GraphsStates_wc"
graph4 <- GraphMiner(data, graphoptions4, graphpwd4)

#dsSIIE23RAW <- SIIE23RAW
#dsList <- SIIE23doLoadGraphs(dsSIIE23RAW)
#GraphList <- dsList
#dsSIIE23RAW <- SIIE23addQuantiles(dsSIIE23RAW)
#head(dsSIIE23RAW)
#dataset <- SIIE23doGenerateDatasets(downto=1)

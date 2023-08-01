setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE2023.R")
source("SIIE23Scripts/SIIE23master.R")
source("Dot.R")
source("GraphMiner.R")

SIIE23doLoadMilestones<-function(){
  SIIE23Milestones <<- read.delim2("~/Descargas/Unified.tsv")
}

SIIE23doInitDatasets()
SIIE23doLoadSessions()
SIIE23doLoadMilestones()

names(SIIE23Milestones)[names(SIIE23Milestones) == "Grupo"] <- "Group"

graphoptions0 <- graphoptions('SIIE2023', minproblems = 1, maxproblems = 10)
graphoptions1 <- graphoptions('SIIE2023_1')
graphoptions2 <- graphoptions('SIIE2023_2', fieldactivity='Problem')
graphoptions3 <- graphoptions('SIEE2023_3') 
graphoptions4 <- graphoptions('SIIE2023_4', minproblems=1, maxproblems=10)

graph <- GraphMiner(SIIE23RAW, graphoptions0)
graphpwd1 <- "./GraphsSummary"
graph1 <- GraphMinerSummary(SIIE23Milestones, graphoptions1, graphpwd1)
# graphpwd2 <- "./GraphsProblems"
# graph2 <- GraphMinerProblems(data, graphoptions2, graphpwd2)
# graphpwd3 <- "./GraphsStates"
# graph3 <- GraphMinerProblems(data, graphoptions3, graphpwd3)
# graphpwd4 <- "./GraphsStates_wc"
# graph4 <- GraphMinerSummary(data, graphoptions4, graphpwd4)
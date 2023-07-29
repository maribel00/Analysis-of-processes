setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE2023.R")
source("SIIE23master.R")
source("Dot.R")
source("GraphMiner.R")

SIIE23doLoadMilestones<-function(){
  SIIE23Milestones <<- unique(read.delim2("~/Descargas/Unified.tsv") %>% select(Session, Milestone))
}

SIIE23doInitDatasets()
SIIE23doLoadSessions()
SIIE23doLoadMilestones()
nrow(SIIE23RAW)
head(SIIE23RAW)
SIIE23Milestones <- SIIE23Milestones %>% group_by(Session) %>% summarise(Milestone = max(Milestone))
nrow(SIIE23Milestones)
head(SIIE23Milestones)

data <- merge(SIIE23RAW, SIIE23Milestones, by = "Session")
# Crear la nueva columna "State"
data <- data %>%
  mutate(State = ifelse(OutCome == "fail", paste(Problem, "FAIL"), paste(Problem, "OK")))

data$Composition <- paste(data$Problem, data$Milestone, sep = "_")
head(data)

graphoptions0 <- graphoptions('SIIE2023', fieldactivity='Composite', minproblems = 1, maxproblems = 10)
graphoptions1 <- graphoptions('SIIE2023_1', maxproblems=10)
graphoptions2 <- graphoptions('SIIE2023_2', fieldactivity='Problem')
graphoptions3 <- graphoptions('SIEE2023_3', fieldactivity='State') 
graphoptions4 <- graphoptions('SIIE2023_4', fieldactivity='State',minproblems=1,maxproblems=10)

graphpwd <- "./Graphs"
graph <- GraphMiner(data, graphoptions0, graphpwd)
graphpwd1 <- "./GraphsSummary"
graph1 <- GraphMinerSummary(data, graphoptions1, graphpwd1)
graphpwd2 <- "./GraphsProblems"
graph2 <- GraphMinerProblems(data, graphoptions2, graphpwd2)
graphpwd3 <- "./GraphsStates"
graph3 <- GraphMinerProblems(data, graphoptions3, graphpwd3)
graphpwd4 <- "./GraphsStates_wc"
graph4 <- GraphMinerSummary(data, graphoptions4, graphpwd4)

#GraphList <- SIIE23doLoadGraphs(SIIE23RAW)

#dsList <- list()
#dsbase <- GenerateBaseDataset()

#dsend <- dsbase[dsbase$Level >= 8,]
#treeend <- C5.0(dsend[,allmetricsF()], as.factor(dsend$PERFORMANCE), trials = 100, rules=TRUE)
#summary(treeend)

#dsbegin <- dsbase[dsbase$Level <= 5,]
#treebegin <- C5.0(dsbegin[,allmetricsF()], as.factor(dsbegin$PERFORMANCE), trials = 100, rules=TRUE)
#summary(treebegin)
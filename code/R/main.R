setwd('./Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/R/')

source("SIIE23Scripts/SIIE2023.R")
source("SIIE23Scripts/SIIE23master.R")

SIIE23doInitDatasets()
SIIE23doLoadSessions()
GraphList <- SIIE23doLoadGraphs(SIIE23RAW)

dsList <- list()
dsbase <- GenerateBaseDataset()

dsbase$Di <- log(dsbase$Di)
dsbase$PERFORMANCE <- as.factor(dsbase$PERFORMANCE)

new <- dsbase[dsbase$Group=="DBA 1516 P2 GA",c("p")]
new <- max(new)
new

# MÉTRICAS

LCV_Lime_Theme()

SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ns")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="np")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ot")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="st")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="rt")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ft")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ps")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="fr")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="sq")

SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Cl")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="De")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Dm")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Le")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Di")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="We")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Ef")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="St")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Dag")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="WDag")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Be")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="Ba")

# COMBINACIÓN DE TODAS LAS MÉTRICAS

dsend <- dsbase[dsbase$Level >= 8,]
treeend <- C5.0(dsend[,allmetricsF()], as.factor(dsend$PERFORMANCE), trials = 100, rules=TRUE)
summary(treeend)

predictend <- predict(treeend, dsend[,allmetricsF()])
cmend <- confusionMatrix(predictend, dsend[,c("PERFORMANCE")])
cmend

dsbegin <- dsbase[dsbase$Level <= 5,]
# treebegin <- C5.0(dsbegin[,allmetricsF()], as.factor(dsbegin$PERFORMANCE), trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE, minCases = 3,))
treebegin <- C5.0(dsbegin[,allmetricsF()], dsbegin$PERFORMANCE, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegin)

summary(dsbegin$PERFORMANCE)

predictbegin3 <- predict(treebegin, dsbegin[dsbegin$Level<=3,allmetricsF()])
cmbegin3 <- confusionMatrix(predictbegin3, dsbegin[dsbegin$Level<=3,c("PERFORMANCE")])
cmbegin3

predictbegin <- predict(treebegin, dsbegin[,allmetricsF()])
cmbegin <- confusionMatrix(predictbegin, dsbegin[,c("PERFORMANCE")])
cmbegin

# MEDIDAS DE COMPLEJIDAD DE PROPÓSITO GENERAL

treeend <- C5.0(dsend[,topologicalmetrics()], as.factor(dsend$PERFORMANCE), trials = 100, rules=TRUE)
summary(treeend)

predictend <- predict(treeend, dsend[,allmetricsF()])
cmend <- confusionMatrix(predictend, dsend[,c("PERFORMANCE")])
cmend

# treebegin <- C5.0(dsbegin[,allmetricsF()], as.factor(dsbegin$PERFORMANCE), trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE, minCases = 3,))
treebegintopological <- C5.0(dsbegin[,topologicalmetrics()], dsbegin$PERFORMANCE, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegintopological)

predictbegintopological <- predict(treebegintopological, dsbegin[,topologicalmetrics()])
cmbegin <- confusionMatrix(predictbegintopological, dsbegin[,c("PERFORMANCE")])
cmbegin

LCV_Heaven_Theme()
LCV_MosaicDensities(dsbase[dsbase$Level == 9,], topologicalmetrics())

# MEDIDAS DE RENDIMIENTO CLÁSICAS

treeend_classic <- C5.0(dsend[,classicalmetrics()], dsend$PERFORMANCE, trials = 100, rules=TRUE)
summary(treeend_classic)

head(dsend)
predictend <- predict(treeend, dsend[,allmetricsF()])
cmend <- confusionMatrix(predictend, dsend[,c("PERFORMANCE")])
cmend

treebegin <- C5.0(dsbegin[,allmetricsF()], dsbegin$PERFORMANCE, trials = 100, rules=TRUE)
summary(treebegin)

predictbegin <- predict(treebegin, dsbegin[,allmetricsF()])
cmbegin <- confusionMatrix(predictbegin, dsbegin[,c("PERFORMANCE")])
cmbegin

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
dsbase$KMEANS_Grade <- as.factor(dsbase$KMEANS_Grade)

# MÉTRICAS

LCV_Lime_Theme()

SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ns")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="np")
# SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ot")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="st")
SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="rt")
# SIIE23ShowMetric(dsbase[dsbase$Level == 10,], metric="ft")
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
treeend <- C5.0(dsend[,allmetricsF()], dsend$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeend)

predictend <- predict(treeend, dsend[,allmetricsF()])
cmend <- confusionMatrix(predictend, dsend[,c("PERFORMANCE")])
cmend

treeendg <- C5.0(dsend[,allmetricsF()], dsend$KMEANS_Grade, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeendg)

predictendg <- predict(treeendg, dsend[,allmetricsF()])
cmendg <- confusionMatrix(predictendg, dsend[,c("KMEANS_Grade")])
cmendg

dsbegin <- dsbase[dsbase$Level <= 5,]
treebegin <- C5.0(dsbegin[,allmetricsF()], dsbegin$PERFORMANCE, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegin)

summary(dsbegin$PERFORMANCE)

predictbegin3 <- predict(treebegin, dsbegin[dsbegin$Level<=3,allmetricsF()])
cmbegin3 <- confusionMatrix(predictbegin3, dsbegin[dsbegin$Level<=3,c("PERFORMANCE")])
cmbegin3

predictbegin <- predict(treebegin, dsbegin[,allmetricsF()])
cmbegin <- confusionMatrix(predictbegin, dsbegin[,c("PERFORMANCE")])
cmbegin

treebeging <- C5.0(dsbegin[,allmetricsF()], dsbegin$KMEANS_Grade, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebeging)

predictbeging <- predict(treebeging, dsbegin[,allmetricsF()])
cmbeging <- confusionMatrix(predictbeging, dsbegin[,c("KMEANS_Grade")])
cmbeging

# MEDIDAS DE COMPLEJIDAD DE PROPÓSITO GENERAL

treeendtopological <- C5.0(dsend[,topologicalmetrics()], dsend$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeendtopological)

predictendtopological <- predict(treeendtopological, dsend[,topologicalmetrics()])
cmend <- confusionMatrix(predictendtopological, dsend[,c("PERFORMANCE")])
cmend

treeendtopologicalg <- C5.0(dsend[,topologicalmetrics()], dsend$KMEANS_Grade, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeendtopologicalg)

predictendtopologicalg <- predict(treeendtopologicalg, dsend[,topologicalmetrics()])
cmend <- confusionMatrix(predictendtopologicalg, dsend[,c("PERFORMANCE")])
cmend

# treebegin <- C5.0(dsbegin[,allmetricsF()], as.factor(dsbegin$PERFORMANCE), trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE, minCases = 3,))
treebegintopological <- C5.0(dsbegin[,topologicalmetrics()], dsbegin$PERFORMANCE, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegintopological)

predictbegintopological <- predict(treebegintopological, dsbegin[,topologicalmetrics()])
cmbegin <- confusionMatrix(predictbegintopological, dsbegin[,c("PERFORMANCE")])
cmbegin

treebegintopologicalg <- C5.0(dsbegin[,topologicalmetrics()], dsbegin$KMEANS_Grade, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegintopologicalg)

predictbegintopologicalg <- predict(treebegintopologicalg, dsbegin[,topologicalmetrics()])
cmbeging <- confusionMatrix(predictbegintopologicalg, dsbegin[,c("KMEANS_Grade")])
cmbeging

LCV_Heaven_Theme()
LCV_MosaicDensities(dsbase[dsbase$Level == 9,], topologicalmetrics())

# MEDIDAS DE RENDIMIENTO CLÁSICAS

treeend_classic <- C5.0(dsend[,classicalmetrics()], dsend$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeend_classic)

head(dsend)
predictend <- predict(treeend, dsend[,allmetricsF()])
cmend <- confusionMatrix(predictend, dsend[,c("PERFORMANCE")])
cmend

treebegin <- C5.0(dsbegin[,allmetricsF()], dsbegin$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegin)

predictbegin <- predict(treebegin, dsbegin[,allmetricsF()])
cmbegin <- confusionMatrix(predictbegin, dsbegin[,c("PERFORMANCE")])
cmbegin

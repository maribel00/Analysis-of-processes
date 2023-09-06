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

allmetrics <- allmetricsF()[-1]
allmetrics <- allmetrics[-1]
cat(allmetrics)

dsend <- dsbase[dsbase$Level >= 8,]
treeend <- C5.0(dsend[,allmetrics], dsend$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeend)

predictend <- predict(treeend, dsend[,allmetrics])
cmend <- confusionMatrix(predictend, dsend[,c("PERFORMANCE")])
cmend

treeendg <- C5.0(dsend[,allmetrics], dsend$KMEANS_Grade, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeendg)

predictendg <- predict(treeendg, dsend[,allmetrics])
cmendg <- confusionMatrix(predictendg, dsend[,c("KMEANS_Grade")])
cmendg

dsbegin <- dsbase[dsbase$Level <= 5,]
treebegin <- C5.0(dsbegin[,allmetrics], dsbegin$PERFORMANCE, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegin)

summary(dsbegin$PERFORMANCE)

predictbegin3 <- predict(treebegin, dsbegin[dsbegin$Level<=3,allmetricsF()])
cmbegin3 <- confusionMatrix(predictbegin3, dsbegin[dsbegin$Level<=3,c("PERFORMANCE")])
cmbegin3

predictbegin <- predict(treebegin, dsbegin[,allmetrics])
cmbegin <- confusionMatrix(predictbegin, dsbegin[,c("PERFORMANCE")])
cmbegin

treebeging <- C5.0(dsbegin[,allmetrics], dsbegin$KMEANS_Grade, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebeging)

predictbeging <- predict(treebeging, dsbegin[,allmetrics])
cmbeging <- confusionMatrix(predictbeging, dsbegin[,c("KMEANS_Grade")])
cmbeging

# MEDIDAS DE COMPLEJIDAD DE PROPÓSITO GENERAL

topological<- topologicalmetrics()[-1]
topological <- topological[-1]
cat(topological)

treeendtopological <- C5.0(dsend[,topological], dsend$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeendtopological)

predictendtopological <- predict(treeendtopological, dsend[,topological])
cmend <- confusionMatrix(predictendtopological, dsend[,c("PERFORMANCE")])
cmend

treeendtopologicalg <- C5.0(dsend[,topological], dsend$KMEANS_Grade, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeendtopologicalg)

predictendtopologicalg <- predict(treeendtopologicalg, dsend[,topological])
cmend <- confusionMatrix(predictendtopologicalg, dsend[,c("KMEANS_Grade")])
cmend

# treebegin <- C5.0(dsbegin[,allmetricsF()], as.factor(dsbegin$PERFORMANCE), trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE, minCases = 3,))
treebegintopological <- C5.0(dsbegin[,topological], dsbegin$PERFORMANCE, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegintopological)

predictbegintopological <- predict(treebegintopological, dsbegin[,topological])
cmbegin <- confusionMatrix(predictbegintopological, dsbegin[,c("PERFORMANCE")])
cmbegin

treebegintopologicalg <- C5.0(dsbegin[,topological], dsbegin$KMEANS_Grade, trials = 100, rules=TRUE,  control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegintopologicalg)

predictbegintopologicalg <- predict(treebegintopologicalg, dsbegin[,topological])
cmbeging <- confusionMatrix(predictbegintopologicalg, dsbegin[,c("KMEANS_Grade")])
cmbeging

LCV_Heaven_Theme()
LCV_MosaicDensities(dsbase[dsbase$Level == 9,], topologicalmetrics())

# MEDIDAS DE RENDIMIENTO CLÁSICAS

classicalmetrics <- classicalmetrics()[-1]
classicalmetrics <- classicalmetrics[-1]
cat(classicalmetrics)

treeend_classic <- C5.0(dsend[,classicalmetrics], dsend$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeend_classic)

predictend_classic <- predict(treeend_classic, dsend[,classicalmetrics])
cmend_classic <- confusionMatrix(predictend_classic, dsend[,c("PERFORMANCE")])
cmend_classic

treebegin_classic <- C5.0(dsbegin[,classicalmetrics], dsbegin$PERFORMANCE, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegin_classic)

predictbegin_classic <- predict(treebegin_classic, dsbegin[,classicalmetrics])
cmbegin_classic <- confusionMatrix(predictbegin_classic, dsbegin[,c("PERFORMANCE")])
cmbegin_classic

treeend_classicg <- C5.0(dsend[,classicalmetrics], dsend$KMEANS_Grade, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treeend_classicg)

predictend_classicg <- predict(treeend_classicg, dsend[,classicalmetrics])
cmend_classicg <- confusionMatrix(predictend_classicg, dsend[,c("KMEANS_Grade")])
cmend_classicg

treebegin_classicg <- C5.0(dsbegin[,classicalmetrics], dsbegin$KMEANS_Grade, trials = 100, rules=TRUE, control = C5.0Control(fuzzyThreshold = TRUE,))
summary(treebegin_classicg)

predictbegin_classicg <- predict(treebegin_classicg, dsbegin[,classicalmetrics])
cmbegin_classicg <- confusionMatrix(predictbegin_classicg, dsbegin[,c("KMEANS_Grade")])
cmbegin_classicg

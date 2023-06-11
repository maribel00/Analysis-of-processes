library(readr)
library(xtable)
library('ggpubr')

rdataset<-read.csv(file = "DBA1521.tsv", sep = "\t", dec = ",")
rdataset <- rdataset[(rdataset$DAG <=2),c(3,7:12,16,19,25,30:31,33:34,52,71)]
colnames(rdataset) <- c("Achiever","Year","DAG","Perseverant","SessionsBefore","SessionsAfter","SolvedSessions","Newcomer","EarlyBird","Performer","RWSP","MAN","SIM","MDN","FDegree9","FLAP9")
head(rdataset)

boxplot <- boxplot(SessionsBefore~Year,rdataset)
plot <- ggplot(data=rdataset,mapping=aes(x=Year,y=SessionsBefore))+geom_boxplot(fill = "olivedrab1")+theme_bw()
plot

Variable <- "Year"
Value <- "SessionsBefore"
rdataset.lm <- lm(rdataset[[Value]]~rdataset[[Variable]], rdataset)
rdataset.aov <- aov(rdataset.lm)
summary <- summary(rdataset.aov)
print(xtable(as.matrix(summary)), include.rownames = TRUE)

rdataset.tukey<-TukeyHSD(rdataset.aov)
rdataset.tukey
print(xtable(as.matrix(rdataset.tukey$`rdataset[[Variable]]`)), include.rownames = TRUE)

plot(rdataset.tukey)
plot(rdataset.aov$residuals)
boxplot(rdataset.aov$residuals, main=paste("Residuals ",Value))
hist(rdataset.aov$residuals, main = paste("Residuals ",Value))
ggdensity(rdataset[[Value]],xlab=Value)
ggqqplot(rdataset[[Value]], xlab =Value)

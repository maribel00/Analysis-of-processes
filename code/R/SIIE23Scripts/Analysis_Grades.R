# filename ="~/Dropbox/Research/DBA/MB_ProcessMining/MB_Freq_by_Grade_Quartile.tsv"
# field="Q.Grade"
# MB <- read.delim2(filename)
field="Year"
MB<-MBMaster2
MB.melt <- melt(MB, id=c(field), variable.name = "Problem", value.name = "Freq")
MB.boxplot <- boxplot(Freq~Problem, MB.melt)
MB.plot <- ggplot(data=MB.melt,mapping=aes(x=Problem,y=Freq, colour=Q.Grade))+geom_boxplot()+theme_bw()
MB.plot
# ANOVA
MB.lm <- lm(Freq~Q.Grade,MB.melt)
MB.aov <- aov(MB.lm)
summary(MB.aov)
#Df  Sum Sq Mean Sq F value Pr(>F)
#Year          3    9769    3256   0.805  0.492
#Residuals   274 1107935    4044
#plaot828 observations deleted due to missingness
MB.tukey <- TukeyHSD(MB.aov)
MB.tukey
plot(MB.tukey)


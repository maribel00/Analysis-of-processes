filename ="~/Dropbox/Research/DBA/MB_ProcessMining/MB_Freq_by_Grade_Quartile.tsv"
field="Q.Grade"
#MB <- read.delim2(filename)

MB.melt <- melt(MB, id=c(field), variable.name = "Problem", value.name = "Freq")
MB.boxplot <- boxplot(Freq~field, MB.melt)
MB.plot <- ggplot(data=MB.melt,mapping=aes(x=Problem,y=Freq, colour=year))+geom_boxplot()+theme_bw()
MB.melt <- melt(MB, id=c(field), variable.name = "Problem", value.name = "Freq")
MB.boxplot <- boxplot(Freq~field, MB.melt)
MB.plot <- ggplot(data=MB.melt,mapping=aes(x=Problem,y=Freq, colour=field))+geom_boxplot()+theme_bw()
# ANOVA
MB.lm <- lm(Freq~year,MB.melt)
MB.aov <- aov(MB.lm)
summary(MB.aov)
#Df  Sum Sq Mean Sq F value Pr(>F)
#Year          3    9769    3256   0.805  0.492
#Residuals   274 1107935    4044
#plaot828 observations deleted due to missingness
MB.tukey <- TukeyHSD(MB.aov)
plot(MB.tukey)

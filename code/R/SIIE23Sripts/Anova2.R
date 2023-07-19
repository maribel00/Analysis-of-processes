mgroup="Year"
mvariable="Problem"
mvalue="Freq"
dataset =MBMaster2
dataset.sel=dataset
# dataset.sel =dataset[c(mgroup, mvariable, mvalue)]
dataset.unique=dataset.sel
# dataset.unique=unique(dataset.sel)
#MB <- MB MB <- read.delim2(filename)
dataset.melt <- melt(dataset.sel, id=c(mgroup),variable.name = mvariable, value.name = mvalue)
# dataset.melt=dataset.unique
dataset.boxplot <- boxplot(dataset.melt[[mvalue]]~dataset.melt[[mvariable]],dataset.melt)
dataset.boxplot
dataset.plot <- ggplot(data=dataset.melt,mapping=aes(x=dataset.melt[[mvariable]],y=dataset.melt[[mvalue]], colour=dataset.melt[[mgroup]]))+geom_boxplot()+theme_bw()
dataset.plot
# ANOVA
dataset.lm <- lm(mvalue~mvariable,dataset.melt)
dataset.aov <- aov(dataset.lm)
summary(dataset.aov)
dataset.tukey <- TukeyHSD(dataset.aov)
plot(dataset.tukey)


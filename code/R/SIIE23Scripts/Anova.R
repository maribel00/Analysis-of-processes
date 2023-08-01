source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Theme.R")
LCV_Heaven_Theme()
LCV_boxplot(DBAGroups15.21,"Year","After","Year")

dataset<-DBAGroups15.21
#
Avariable<-"Year"
Avalue<-"After"
boxplotX<-Avariable
boxplotY <- Avalue
boxplotGroup<-"Year"
excludeRows<-c()

#notas por año
# Avariable<-"Year"
# Avalue<-"Grade"
# boxplotX<-"Year"
# boxplotY<-"Grade"
# boxplotGroup<-"Year"
# excludeRows<-c()

# Avariable<-"Year"
# Avalue<-"DGrade"
# boxplotY<-"Grade"
# boxplotX<-"Year"
# boxplotGroup<-"Group"
# excludeRows<-c()



# Problemas --> DAG
# Avariable<-"Grade"
# Avalue<-"DAG"
# boxplotX<-"Grade"
# boxplotY<-"DAG"
# boxplotGroup<-"Year"
# excludeRows<-c("Y2018")


# Años --> distraccion
# Avariable<-"Problem"
# Avalue<-"Distract"
# boxplotX<-"Problem"
# boxplotY<-"Distract"
# boxplotGroup<-"Year"



# Problemas --> distraccion
# Avariable<-"Problem"
# Avalue<-"Distract"
# boxplotX<-"Problem"
# boxplotY<-"Distract"
# boxplotGroup<-"Year"





# # Años --> distraccion
# Avariable<-"Year"
# Avalue<-"Distract"
# boxplotX<-"Year"
# boxplotY<-"Distract"
# boxplotGroup<-"Year"


# Años -< Notas
# Avariable<-"Year"
# Avalue<-"Grade"
# boxplotX<-"Year"
# boxplotY<-"Grade"
# boxplotGroup<-"Year"
# excludeRows<-c()

# Años -> Problemas
# Avariable<-"Year"
# Avalue<-"All.trials"
#
# boxplotX<-"Problem"
# boxplotY<-"All.trials"
# boxplotGroup<-"Year"
# lgroup<-dataset$Year
# lvariable<-dataset$All.trials
# lvalue<-dataset$Problem
# dataset.sel <- dataset[c(mgroup, mvariable, mvalue)]
dataset.sel <- subset(dataset,select = c(boxplotGroup, boxplotX, boxplotY))
dataset.unique<-dataset.sel[!(dataset.sel$Year %in% excludeRows),]

#dataset.moreunique <- unique(dataset.unique)
#dataset.histogram <- ggplot(dataset.moreunique,aes=(x=boxplotY))+geom_histogram(fill = "white", colour = "black")
#dataset.histogram

# dataset.unique=unique(dataset.sel)
#MB <- MB MB <- read.delim2(filename)
# dataset.melt <- melt(dataset.sel, id=c(mgroup),
#                      variable.name = mvariable,
#                      value.name = mvalue)
dataset.melt<-dataset.unique
dataset.boxplot <- boxplot(dataset.melt[[boxplotY]]~dataset.melt[[boxplotX]],dataset.melt)
dataset.plotbn <- ggplot(data=dataset.melt,mapping=aes(x=.data[[boxplotX]],y=.data[[boxplotY]]))+
  geom_boxplot(fill = fillcolor, color=textcolor,alpha=alphafill)+
#  theme_bw()
LCVtheme

dataset.plotbn

ggdensity(dataset.unique[[Avalue]],xlab=Avalue)+
  LCVtheme
ggqqplot(dataset.unique[[Avalue]])
shapiro.test(dataset.unique[[Avalue]])
dataset.plotc <- ggplot(data=dataset.melt,mapping=aes(x=.data[[boxplotX]],y=.data[[boxplotY]],color=.data[[boxplotGroup]]))+geom_boxplot()+theme_bw()
dataset.plotc
head(dataset.melt)
# ANOVA
dataset.lm <- lm(dataset.melt[[Avalue]]~dataset.melt[[Avariable]],dataset.melt)
dataset.aov <- aov(dataset.lm)
summary(dataset.aov)
dataset.tukey <- TukeyHSD(dataset.aov)
dataset.tukey
tuk_plot(dataset.tukey,"Confidence levels",boxplotY)
plot(dataset.aov$residuals, xlab = Avalue)
dataset.kw <- kruskal.test(dataset.melt[[Avalue]]~dataset.melt[[Avariable]],dataset.melt)
dataset.kw
plot(dataset.aov$residuals)
boxplot(dataset.aov$residuals)
hist(dataset.aov$residuals)

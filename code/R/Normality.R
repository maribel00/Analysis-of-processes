# Nrmalidad de los datos
ndataset <- DBAProblems15.21
nVariable <- "Year"
nValue <- "All.trials"
ndsp <- ndataset[,c(nVariable,nValue)]
ndataset.lm <- lm(ndsp[[nValue]]~ndsp[[nVariable]], ndsp)
ndataset.aov <- aov(ndataset.lm)
summary(ndataset.aov)
ndataset.tukey<-TukeyHSD(ndataset.aov)
ndataset.tukey
plot(ndataset.tukey)
plot(ndataset.aov$residuals)
boxplot(ndataset.aov$residuals, main=paste("Residuals ",nValue))
hist(ndataset.aov$residuals, main = paste("Residuals ",nValue))
ggdensity(ndataset[[nValue]],xlab=nValue)
ggqqplot(ndataset[[nValue]], xlab = nValue)


# ddata <- data.frame(ddataset$Hour)
# ggplot(data=ddata, mapping=aes(x=ddataset$Hour))+
#   geom_bar()+
#   #  scale_x_discrete(labels =c("Sunday", "Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday"))+
#   geom_text(stat="count", aes(label=..count..),vjust=-1)+
#   xlab("Time of the day")+
#   #scale_x_discrete(xmin=0,xmax=23)
#   scale_x_continuous(n.breaks=24, limits=c(-1,24))+
#   ylab("Number of work sessions")+
#   theme_classic()
#
#
#
# ndataset <- DBAGroups15.21
# nVariable <- "Group"
# nValue <- "Newcomer.RelTime"
# hValue<-ndataset[[nValue]]
# ggplot(ndataset, mapping=aes(x=hValue))+
#   geom_bar()+
#   # geom_histogram(binwidth=0.1)+
#   xlab(nValue)+ylab("Frequency")+
#   # scale_x_discrete()+
#   geom_text(stat="count", aes(x=hValue,label=..count..), vjust=-1)+
#   # labs(title=nValue)+
#   theme_classic()


# source("./plot_style.R")
source("/home/lcv/Dropbox/Research/DBA/MB/Scripts/LCV_Theme.R")
LCV_Graphite_Theme()
LCV_histogram2(DBASessions15.21,"DayOfWeek",7,c("Sunday", "Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday"))
LCV_density(DBAProblems15.21,"After.trials")
LCV_density2(DBAProblems15.21,"After.trials",50)
LCV_histogram(DBASessions15.21,"Hour")


ddataset <- DBASessions15.21
dvariable <- ddataset$DayOfWeek
ddataset$DayOfWeek <- as.factor(ddataset$DayOfWeek)
ddata <- data.frame(ddataset$DayOfWeek)
ggplot(data=ddata, mapping=aes(x=ddataset$DayOfWeek))+
  geom_bar(fill="lightblue", color="black")+
  scale_x_discrete(labels =c("Sunday", "Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday"))+
  geom_text(stat="count", aes(label=..count..),vjust=-1)+
  xlab("Days of week")+
  ylab("Number of work sessions")+
  bptheme

ddata <- data.frame(ddataset$Hour)
ggplot(data=ddata, mapping=aes(x=ddataset$Hour))+
  geom_bar()+
#  scale_x_discrete(labels =c("Sunday", "Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday"))+
  geom_text(stat="count", aes(label=..count..),vjust=-1)+
  xlab("Time of the day")+
  #scale_x_discrete(xmin=0,xmax=23)
  scale_x_continuous(n.breaks=24, limits=c(-1,24))+
  ylab("Number of work sessions")+
  theme_classic()

#+
#  labs(title="Activity in the server by day of week", x="Day of week",
#  y="Number of sessions")
#  geom_bar()+
#  geom_text(stat='count', aes(label=..count..), vjust=-1)
ddataset<-DBAGroups15.21
hVar<-ddataset$Reward.pattern
step<-0.1
ddata <- data.frame(hVar)
ggplot(ddataset,aes(x=hVar))+
  geom_histogram(binwidth=step)+
  stat_bin(binwidth=step,geom="text", aes(label=..count..),vjust=-1)+
  xlab("Follower")+
  scale_x_continuous(n.breaks=1/step)+
  ylab("Frequency")+
  theme_classic()

ddataset<-DBAGroups15.21
nVar<-"Sessions"
hVar=ddataset[[nVar]]
maxVar <- max(hVar)
minVar <-min(hVar)
step<-10
ddata <- data.frame(hVar)
ggplot(ddataset,aes(x=hVar))+
  geom_histogram(binwidth=step)+
  stat_bin(binwidth=step,geom="text", aes(label=..count..),vjust=-1)+
  xlab(nVar)+
  scale_x_continuous(n.breaks=(maxVar-minVar)/step)+
  ylab("Frequency")+
  theme_classic()


ddataset<-DBAGroups15.21
nVar<-"Sessions"
hVar=ddataset[[nVar]]
maxVar <- max(hVar)
minVar <-min(hVar)
nintervals<-50
nticks<-nintervals/4
ddata <- data.frame(hVar)

ggplot(ddataset,aes(x=hVar))+
  geom_histogram(binwidth=(maxVar-minVar)/nintervals,aes(y=after_stat(count)))+
  geom_density(color="darkblue",fill="lightblue",alpha=0.5, aes(y=after_stat(count/max(count))))+
  #geom_density(color="darkblue",fill="lightblue",alpha=0.5)+
  #geom_density(aes(y=after_stat(150*count/max(count))))+
  xlab(nVar)+
  scale_x_continuous(n.breaks=nticks)+
  ylab("Frequency")+
  LCVtheme
  # theme_classic()


c5dataset <- DBAGroups15.21[(DBAGroups15.21$Grade < 18.4 ) ,]

#c5indexvars<-c(seq(9,14),seq(18,22),seq(24,39))
#c5indexvars<-c(seq(9:12),seq(25:33),seq(37:39))
# c5indexvars<-c(9,10,11,25,26,27,28,29,30,31,32,33,37,38,39)
 # c5indexvars<-c("SingleThreaded", "Sessions", "Perseverance", "Closing.Time", "Secs.Day", "Reaction.Time","Improving.Time", "Problems.Solved", "Milestones" )
# c5indexvars<-c("DAG","N.Sessions","N.Sessions.Before"  ,  "N.Sessions.After",     "N.Sessions.Solve","Mins.GroupPeriod"    , "Newcomer"             ,"Mins.Starter"        , "Mins.ReactionTime",    "Mins.EarlyBird"  ,    "Mins.ClosingTime"    ,"Mins.Sessions"   ,
# "Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved"  ,"Mins.Day","W.Problems.Solved", "LAPLAP", "LAP1");
# c5indexvars<-c("DAG","Mins.Sessions", "Mins.Day", "Laplace","N.Sessions", "W.Problems.Solved",  "N.Sessions.After",     "Newcomer"             , "Mins.ReactionTime",    "Mins.EarlyBird"  ,    "Mins.ClosingTime"    ,"Mins.Sessions");
c5indexvars<-c("DAG","Mins.Sessions", "Mins.Day", "Laplace", "PostLaplace","PreLaplace", "N.Sessions", "N.Sessions.Before"  ,  "N.Sessions.After","Mins.Sessions.Before" ,"Mins.Sessions.After" );
# c5indexvars<-c(seq(25:35))
set.seed(100)
# c5dataset<-c5dataset[(c5dataset$QGrade=='Q1'),]
rows <- sample(nrow(c5dataset))
c5dataset<-c5dataset[rows,]
c5vardataset <- c5dataset[,c5indexvars]
c5Output <- as.factor(c5dataset$QGrade)
c50tree<- C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
# c50tree<- C5.0(x=c5vardataset,y=c5Output, trials=100)
c50tree
summary(c50tree)
plot(c50tree)


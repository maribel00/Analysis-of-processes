set.seed(54)
x <- "MAN"
x2 <- "Degree8"
x3<-"FDegree8"
rdataset <- DBAGroupsFail
rdataset$Mix <- rdataset[[x3]]+rdataset[[x2]]
# rdataset<-rdataset[(rdataset$LAP < 10) ,]
# rdataset<-rdataset[(rdataset$LAP4 > 1) ,]
# rdataset<-rdataset[(rdataset$Grade < 9) ,]
# rdataset<-rdataset[(rdataset$Size > 0) ,]
# nfields <- c("Grade","XQGRade","DAG","N.Sessions","N.Sessions.Before","N.Sessions.After","N.Sessions.Solve","Days.AcademicPeriod",
#                "Mins.AcademicPeriod", "Mins.GroupPeriod","Newcomer","Mins.Starter","Mins.ReactionTime",
#                "Mins.EarlyBird","Mins.ClosingTime","Mins.Sessions","Mins.Sessions.Before","Mins.Sessions.After","LARVA","N.ProblemsSolved","PATH".
#               "Mins.Day","W.Problems.Solved","Milestones","EffortAdj","EffortDegree","FailDegree"     );

nfields <-c(
              "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
              "DAG",
              "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
              "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
              "Mins.ClosingTime"    ,
              "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
              "LAP",
              "Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
              "FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
              "LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9",
              "Mix"
  );
# summary(DBAGroups15.21)

              # ,"","","","","","","","","","","","","","",

# nfields <- c("SingleThreaded", "X50HP", "Sessions", "Perseverance", "Grade", "W.Problems.Solved","Closing.Time", "Secs.Day", "Reaction.Time","Improving.Time", "Problems.Solved", "Milestones" )
# rdataset <- rdataset[,c("NewComer","EarlyBird","Follower","MessedUp","RoadRunner","Perseverant","Procrastinator","Terminator","TimeBurner","Achiever","Performer")]
# head(rdataset)
# show(nfields)
rdataset <- rdataset[,nfields]
pairs(rdataset)
variable<-rdataset[[x]]
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
for (y in nfields) {
# y <- "Grade"
value <- rdataset[[y]]
# Scatter plot and linear regression line
# coef <- round(coef(lm(rdataset[[y]] ~ rdataset[[x]])), 2)
rdataset.RP<- cor.test(variable, value, method=("pearson"))
rdataset.RK<- cor.test(variable, value, method=("kendall"))
rdataset.RS<- cor.test(variable, value, method=("spearman"))
cat("Processing x=",x," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
# rdatasest.lm <- lm(value~variable)
plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)
# plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"    Y =", coef[1], "+", coef[2], "x\t\tR Pearson=",rdataset.RP$estimate), ylab = y)
# abline(rdataset.lm, col = 4, lwd = 3)
#boxplot(rdataset[[y]] ~ rdataset[[x]],rdataset, xlab = x, ylab = y)
# Text
# coef <- round(coef(lm(rdataset[[y]] ~ rdataset[[x]])), 2)
# text(2, 70,  paste("Y = ", coef[1], "+", coef[2], "x"))

}



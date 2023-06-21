

doLoadData<-function(){
  readline("Press INTRO to LOAD DATA AND PREPROCESS")
  data <- read.delim2("SIIE23.tsv")
  cat("OK loaded ",nrow(data), " rows")
  readline("Press INTRO to Preprocess data (logarithmic adjustements)")

  for (sfield  in c("LAP01", "LAP02", "LAP03", "LAP04", "LAP05", "LAP06", "LAP07", "LAP08", "LAP09", "LAP10")) {
    data[paste("LOG",sfield, sep = "")] <- log(data[[sfield]])
    data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
  }

  for (sfield  in c("FLAP1", "FLAP2", "FLAP3", "FLAP4", "FLAP5", "FLAP6", "FLAP7", "FLAP8", "FLAP9")) {
    data[[paste("LOG",sfield, sep="")]] <- log(data[[sfield]])
    data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
  }

  data$m<-data$s

  benefits <- c("ns","np", "sq","ps")
  costs <- c("ot","st","rt","ft","fr")

  for (goal in benefits) {
    data$m <- data$m + data[[goal]]
  }
  for (cost in costs) {
    data$m <- data$m + (1- data[[cost]])
  }
  data$m <- data$m / (length(benefits)+length(costs))
  data$ng <- data$Grade/10
  data$all<-TRUE
  return(data)
}



######################################
######################################
######################################



highestMetrics <- data.frame(name=character(0),pearson=numeric(0), kendall=numeric(0),spearman=numeric(0), key=numeric(0))

doSIIERegression<-function(data, objective="MAN", maxGrade=10,exponential=FALSE,   inverse=FALSE, nfields =c(
  "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
  "DAG",
  "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
  "Newcomer",
  # "Mins.Starter" ,"Mins.ReactionTime",
  "Mins.EarlyBird"  ,
  "Mins.ClosingTime"    ,
  "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
  "Degree8","Degree9",
  "FDegree8","FDegree9",
  "LAP9", "Fit"  )) {
  rdataset <- data
  # rdataset<-rdataset[(rdataset$Size==1),]
  rdataset <- rdataset[(rdataset$Grade<=maxGrade),]
  highestMetrics <<- data.frame(name=character(0),fit=numeric(0),r2=numeric(0),mse=numeric(0),p=numeric(0))
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  for (xf in nfields) {
    if (xf != objective) {
      if (exponential) {
        # fit<-LCV_ExponentialRegression(rdataset,y=y, x=objective)
        # highestMetrics[nrow(highestMetrics)+1,] <<- c(y, abs(fit[1]),fit[2],fit[3],fit[4])
        # cat(paste(y,"~",objective,"\n"))
      } else {
        if (inverse) {
          fit<-LCV_LinearRegression(rdataset,x=objective,y=xf)
          highestMetrics[nrow(highestMetrics)+1,] <<- c(xf, abs(fit$pearson),fit$r2,-1,-1)
          cat(paste(xf,"~",objective,"=",fit$pearson),"\n")
        }else {
          fit<-LCV_LinearRegression(rdataset,y=objective,x=xf)
          highestMetrics[nrow(highestMetrics)+1,] <<- c(xf, abs(fit$pearson),fit$r2,-1,-1)
          cat(paste(objective,"~",xf,"=",fit$pearson),"\n")
        }
      }
    }
  }
  highestMetrics <- highestMetrics[(order(highestMetrics$fit, decreasing=TRUE)),]
  show(highestMetrics)
  for (i in seq(1:5)) {
    LCV_LinearRegression(rdataset,highestMetrics[i,"name"],objective)
  }
  highestMetrics
}


doRegression<-function(data, objective=c("MAN"), maxGrade=10) {
  # objective <- c("MAN", "N.ProblemsSolved", "Mins.EarlyBird")
  rdataset <- data
  rdataset<-rdataset[(rdataset$Size==1),]
  rdataset <- rdataset[(rdataset$Grade<=maxGrade),]
  oname <- ""
  if (length(objective) > 0) {
    rdataset$objective <- 0
    for (smix in objective) {
      rdataset$objective <- rdataset$objective + rdataset[[smix]]
      oname <- paste(oname, smix,"/")
    }
    rdataset$objective = rdataset$objective / length(objective)
  } else {
    rdataset$objective <- 0
  }
  nfields <-c(
    "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
    "DAG",
    "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
    "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
    "Mins.ClosingTime"    ,
    "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
    "Degree1","Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
    "FDegree1","FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
    "LAP1","LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9",
    "objective"
  );
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  variable<-rdataset$objective
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  highestMetrics <<- data.frame(name=character(0),pearson=numeric(0), kendall=numeric(0),spearman=numeric(0), key=numeric(0))
  for (y in nfields) {
    if (y != "objective") {
      value <- rdataset[[y]]
      rdataset.RP<- cor.test(variable, value, method=("pearson"))
      rdataset.RK<- cor.test(variable, value, method=("kendall"))
      rdataset.RS<- cor.test(variable, value, method=("spearman"))
      RP<- abs(rdataset.RP$estimate)
      RK<- abs(rdataset.RK$estimate)
      RS<- abs(rdataset.RS$estimate)
      cat("Processing x=",oname," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
      plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)
      highestMetrics[nrow(highestMetrics)+1,] <<- c(y, RP, RK, RS, (RP+RK+RS)/3)
    }
  }
  highestMetrics[(order(highestMetrics$key, decreasing=TRUE)),]
  # highestMetrics
}

doRegressionLM<-function(data, objective=c("MAN"), maxGrade=10) {
  # objective <- c("MAN", "N.ProblemsSolved", "Mins.EarlyBird")
  rdataset <- data
  rdataset<-rdataset[(rdataset$Size==1),]
  rdataset <- rdataset[(rdataset$Grade<=maxGrade),]
  oname <- ""
  if (length(objective) > 0) {
    rdataset$objective <- 0
    for (smix in objective) {
      rdataset$objective <- rdataset$objective + rdataset[[smix]]
      oname <- paste(oname, smix,"/")
    }
    rdataset$objective = rdataset$objective / length(objective)
  } else {
    rdataset$objective <- 0
  }
  nfields <-c(
    "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
    "DAG",
    "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
    "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
    "Mins.ClosingTime"    ,
    "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
    "Degree1","Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
    "FDegree1","FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
    "LAP1","LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9",
    "objective"
  );
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  variable<-rdataset$objective
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  highestMetrics <<- data.frame(name=character(0),pearson=numeric(0), kendall=numeric(0),spearman=numeric(0), key=numeric(0))
  for (y in nfields) {
    if (y != "objective") {
      value <- rdataset[[y]]
      linearmodel <- lm(value ~ variable)
      cat("Processing x=",oname," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
      plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)
      highestMetrics[nrow(highestMetrics)+1,] <<- c(y, RP, RK, RS, (RP+RK+RS)/3)
    }
  }
  highestMetrics[(order(highestMetrics$key, decreasing=TRUE)),]
  # highestMetrics
}

doC50 <- function(data,metrics) {
  c5dataset <- data
  smetrics <- sort(metrics,decreasing=TRUE)
  c5indexvars<-c(smetrics.);
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

}

doRegressionSimplified<-function(data, variable) {
  x <- variable
  x2 <- "Degree8"
  x3<-"FDegree8"
  rdataset <- data
  rdataset$Mix <- rdataset[[x3]]+rdataset[[x2]]
  nfields <-c(
    variable,
    "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
    "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
    "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
    "Mins.ClosingTime"    ,
    "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day"
  );
  rdataset <- rdataset[,nfields]
  # pairs(rdataset)
  x<-"objective"
  variable<-rdataset[[x]]
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  for (y in nfields) {
    value <- rdataset[[y]]
    rdataset.RP<- cor.test(variable, value, method=("pearson"))
    rdataset.RK<- cor.test(variable, value, method=("kendall"))
    rdataset.RS<- cor.test(variable, value, method=("spearman"))
    cat("Processing x=",oname," agsinst y=", y,"\t\tR-Pearson:", rdataset.RP$estimate,"\t\tR-Kendall:", rdataset.RK$estimate,"\t\tR-Spearman:", rdataset.RS$estimate,"\n")
    plot <- plot(x=variable, y=value, pch = 16,xlab = paste(x,"Pearson=",rdataset.RP$estimate), ylab = y)

  }
}



doSIIECluster2<-function(data, ids=c("Group"),metrics=c("SIM","N.ProblemsSolved","MAN","DAG","Grade","N.Sessions","N.Sessions.Before","N.Sessions.After","Mins.Sessions","Mins.Sessions.Before","Mins.Sessions.After","Mins.Day","Mins.Starter","Mins.ReactionTime","Mins.EarlyBird","Mins.ClosingTime")) {
  KM.data <<- data[,c(ids,metrics)]
  KM.nc <<- NbClust(data[,metrics],distance="euclidean",min.nc=2, max.nc = 10, method="kmeans")
  data$fKM <- as.factor(KM.nc$Best.partition)
  data$nKM <- KM.nc$Best.partition

  for (clus in unique(KM.nc$Best.partition)) {
    cat(paste("Cluster ",clus))
    show(data[(data$fKM == clus),c("Group")])
  }
  for (sm in metrics) {
    show(LCV_boxplot(data,"fKM",sm))
  }
  c5vardataset <- data[,metrics]
  c5Output <- data$fKM
  data.tree<- C5.0(x=c5vardataset,y=c5Output, trials=100, rules=FALSE)
  data.rules<- C5.0(x=c5vardataset,y=c5Output, trials=100, rules=TRUE)
  # show(c50tree)
  # show(summary(data.tree))
  # plot(data.tree)
  # show(data[,c("Group","fKM")])
  data
}



# nfields <-c(
#   "Grade","XQGRade","W.Problems.Solved","CNProblems","MAN","MAN01", "SIM","MDN","N.ProblemsSolved",
#   "DAG",
#   "N.Sessions","N.Sessions.Before"  ,"N.Sessions.After",  "N.Sessions.Solve","Mins.GroupPeriod",
#   "Newcomer","Mins.Starter" , "Mins.ReactionTime","Mins.EarlyBird"  ,
#   "Mins.ClosingTime"    ,
#   "Mins.Sessions","Mins.Sessions.Before" ,"Mins.Sessions.After" , "N.ProblemsSolved","Mins.Day",
#   "Degree1","Degree2", "Degree3", "Degree4", "Degree5", "Degree6",  "Degree7","Degree8", "Degree9",
#   "FDegree1","FDegree2", "FDegree3", "FDegree4", "FDegree5", "FDegree6",  "FDegree7","FDegree8", "FDegree9",
#   "LAP1","LAP2", "LAP3", "LAP4", "LAP5", "LAP6",  "LAP7","LAP8", "LAP9"  );

SIIEridge <- function(data) {
  res <- data.frame(Group=character(0), x=numeric(0), y=numeric(0))
  row <- data[1]
  col<-c  ("LAP01","LAP02","LAP03","LAP04","LAP05","LAP06","LAP07","LAP08","LAP09")
  for (i in (1:9)) {
    ngroup <- c(row["Group"])
    x<-i
    col <- paste("LAP0",i, sep="")
    y<-row[col]
    newrow <- c(ngroup,x,y)
    res [res[ncol()]]
  }
  res
}

SIIE23ViewGroup <- function(data, np) {
  res <- paste(paste(str_replace_all(data," " ,"_"),"_",sep=""))
  final <- ""
  for (s in paste(np,"Problems/",res,"/",res,"*DAG*.png", sep="")) {
    final <- paste(final," ", s)
  }
  cat(paste("montage ",final," -geometry 300x300+0+1 -tile 5x", sep=""))
}

doPrepareScales<-function (data){
  readline("Press INTRO to Preprocess data (logarithmic adjustements")

  for (sfield  in c("LAP01", "LAP02", "LAP03", "LAP04", "LAP05", "LAP06", "LAP07", "LAP08", "LAP09", "LAP10")) {
    data[paste("LOG",sfield, sep = "")] <- log(data[[sfield]])
    data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
  }

  for (sfield  in c("FLAP1", "FLAP2", "FLAP3", "FLAP4", "FLAP5", "FLAP6", "FLAP7", "FLAP8", "FLAP9")) {
    data[[paste("LOG",sfield, sep="")]] <- log(data[[sfield]])
    data[paste("CLUS_LOG",sfield, sep = "")] <- as.factor(round(log(data[[sfield]])/10, 0))
  }

  data$fm<-data$s
  data$fmn<-data$s

  benefits <- c("ns","np", "sq","ps")
  costs <- c("ot","st","rt","ft", "fr")

  for (goal in benefits) {
    data$fm <- data$fm + data[[goal]]
  }
  for (cost in costs) {
    data$fm <- data$fm + (1- data[[cost]])
  }

  benefits <- c("ns","np", "sq","ps")
  costs <- c("fr")

  for (goal in benefits) {
    data$fmn <- data$fmn + data[[goal]]
  }
  for (cost in costs) {
    data$fmn <- data$fmn + (1- data[[cost]])
  }


    data$fmn <- data$fmn / (length(benefits)+length(costs))
  data$ng <- data$Grade/10

  return(data)
}


SIIE23Init <- function() {
  readline("Press INTRO to LOAD DATA AND PREPROCESS")
  SIIE23 <<- read.delim2("~/Descargas/SIIE23.tsv")
  SIIE23 <<- doPrepareLogScale(SIIE23)
  # SIIE23$QuartileGrade<- as.factor(SIIE23$QuartileGrade)
  # SIIE23clus <- doSIIECluster1(SIIE23, id_column = c("Group"), main_kpi = "LOGLAP09", other_kpi = c("QuartileGrade","rt","ot","ns"),force_nc=-1, compare_to = c("Grade","m","np",	"rt",	"ot",	"ns"	,"ps"	,"ot"	,"est"	,"ft"	,"fr"))
  cat("OK\n")
  readline("Press INTRO to ANALISIS OF m/mf wrt Grade")
  for (metric in c("mf","m")) {
    show(LCV_density(SIIE23,metric, showall = TRUE))
    LCV_LinearRegression(SIIE23,x=metric, y="Grade", doshow = TRUE, full = TRUE)
  }
  readline("Press INTRO to ANALISIS OF COMPONENTS wrt Grade")
  for (metric in c("ng","ns", "np", "ot", "st", "rt", "ft", "sq", "ps", "fr")) {
    show(LCV_density(SIIE23,metric, showall = TRUE))
    LCV_LinearRegression(SIIE23,x=metric, y="Grade", doshow = TRUE, full = TRUE)
  }
  readline("Press INTRO to CLUSTER ANALISIS")
  SIIE23clus <<- doSIIECluster1(SIIE23, id_column = c("Group"), main_kpi = "LOGLAP09",
                                other_kpi = c("np","rt","ot","ns"),force_nc=-1, compare_to = c("np","rt","ot","ns","LOGLAP09", "QuartileGrade", "Grade"))

  readline("Press INTRO to RULES DISCOVERY")
  rules<<-C5.0(x=SIIE23clus[,c("np","rt","ot","ns","LOGLAP09")],y=as.factor(SIIE23clus[,c("QuartileGrade")]),trials = 100, rules=TRUE)
  # rules<<-C5.0(x=SIIE23clus[,c("st","ft","ps","fr","rt","ot","ns")],y=as.factor(SIIE23clus[,c("KMEANS_LOGLAP09")]),trials = 100, rules=TRUE)
  show(summary(rules))
  readline("Press INTRO to DT DISCOVERY")
  tree<<-C5.0(x=SIIE23clus[,c("np","rt","ot","ns","LOGLAP09")],y=as.factor(SIIE23clus[,c("QuartileGrade")]),trials = 100, rules=FALSE)
  # tree<<-C5.0(x=SIIE23clus[,c("st","ft","ps","fr","rt","ot","ns")],y=as.factor(SIIE23clus[,c("KMEANS_LOGLAP09")]),trials = 100, rules=FALSE)
  show(summary(tree))
  plot(tree)
  readline("Expectations")
  LCV_densitiesFactor(SIIE23clus, "ot","QuartileGrade", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "ot","KMEANS_LOGLAP09", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "rt","QuartileGrade", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "rt","KMEANS_LOGLAP09", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "ns","QuartileGrade", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "ns","KMEANS_LOGLAP09", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "m","QuartileGrade", showmax=TRUE)
  LCV_densitiesFactor(SIIE23clus, "m","KMEANS_LOGLAP09", showmax=TRUE)
}


SIIE23AnalyzeCluster<-function(data, cluster) {
  kpi=cluster;
  for (clus in c("CLUS_LOGLAP05","CLUS_LOGLAP06","CLUS_LOGLAP07","CLUS_LOGLAP08","CLUS_LOGLAP09")) {
    show(LCV_densitiesFactor(SIIE23,kpi,clus, showmax = TRUE))
  }
  for (clus in c("CLUS_LOGFLAP5","CLUS_LOGFLAP6","CLUS_LOGFLAP7","CLUS_LOGFLAP8","CLUS_LOGFLAP9" )) {
    show(LCV_densitiesFactor(SIIE23,kpi,clus, showmax = TRUE))
  }
  for (clus in
       c("QuartileGrade" )) {
    show(LCV_densitiesFactor(SIIE23,kpi,clus, showmax = TRUE))
  }
  show(LCV_histogram(data,cluster, nintervals = 10))
}

BasePath <- "/home/lcv/Dropbox/Research/DBA/MB/"

LoadDataset <- function (name) {
  read.delim2(paste(BasePath,"Datasets/",name, sep=""))
}


PreprocessDBA <- function() {
  DBADataset <- LoadDataset("DBA15-21.tsv")
  Sessions <-data.frame(
    Group=character(),
    Year=character(),
    Start=numeric(),
    End=numeric(),
    Session=character(),
    Problem=character(),
    Outcome=character()
    # name=c("Group", "Year", "Start", "End", "Session", "Problem", "Outcome"),
    # type=c("String", "String", "integer", "integer", "String", "String", "String")
  )

  nSessions <- 0
  slist <- unique(DBADataset$Session)
  tsessions <- length(slist)
  cat("Found ",tsessions, " different sessions")
  for (row in 1:tsessions) {
      nSessions <- nSessions+1
      mysession <- DBADataset[(DBADataset$Session == slist[row]),]
      srecords <- length(mysession)
      smint <- min(mysession$Time)
      smaxt <- max(mysession$Time)
      smaxmi <- max(mysession$Milestone)
      sgroup <- mysession$Grupo[1]
      syear <- mysession$Year[1]
      sproblem <-  mysession$Problem[1]
      cat(nSessions," /", tsessions, "\n" )
      # cat("Session (",nSessions,") has ",length(mysession), " records")
      if (smaxmi<100)
        soutcome="fail"
      else
        soutcome="solved"
      Sessions[nSessions,] <-c(sgroup, syear,smint, smaxt,slist[row], sproblem, soutcome)
  }
  cat("Done")
  save(DBADataset, file=(paste(BasePath,"Datasets/Base.dts", sep="")))
  save(Sessions, file=(paste(BasePath,"Datasets/Sessions.dts", sep="")))
}

FastLoad <- function() {
  load(paste(BasePath,"Datasets/Base.dts", sep=""))
  load(paste(BasePath,"Datasets/Sessions.dts", sep=""))

}

Sessions <- Dataset[,c("Group", "Year", "Start", "End", "Problem", "OutCome")]

StartOfYear <- function (year) {
  min(Sessions[Sessions$Year==year,c("Start")])
}

EndOfYear <- function (year) {
  max(Sessions[Sessions$Year==year,c("Start")])
}

DurationOfYear <- function (year) {
  EndOfYear(year)-StartOfYear(year)
}

RelativeTime<- function (year, time) {
  (time-StartOfYear(year))/DurationOfYear(year)
}

YearOfGroup<-function(group) {
  Sessions[(Sessions$Group == group),"Year"][1]
}

GetAllGroups <- function() {
  unique(Sessions$Group)
}

GetAllYears <- function() {
  unique(Sessions$Year)
}


GetAllSessions<-function(group) {
  Sessions[Sessions$Group == group,]
}

StartOfGroup<-function(group) {
year <- YearOfGroup(group)
 (min (Sessions[(Sessions$Group==group),c("Start")])-StartOfYear(year))/DurationOfYear(year)
}

EndOfGroup<-function(group) {
  year <- YearOfGroup(group)
  (max (Sessions[(Sessions$Group==group),c("Start")])-StartOfYear(year))/DurationOfYear(year)
}

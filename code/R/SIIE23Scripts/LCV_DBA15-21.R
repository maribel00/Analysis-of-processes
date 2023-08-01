source("SIIE23Scripts/LCV_Hipothesis_Tests.R")

dens <- ""
bpn <- ""
bpl <- ""

Battery<-function(data, kpi) {
  LCV_Heaven_Theme()
  cat("%%%%%%%%%%%%%%%% Density")
  # dens <<- LCV_density(data,kpi,10)
  # plot(dens)
  LCV_Normality(data, kpi)
  readline(prompt="Press [enter] to continue")
  cat("%%%%%%%%%%%%%%%% Years")
  bpn <<- LCV_boxplot(data,"Year", kpi, log=FALSE)
  bpl <<- LCV_boxplot(data,"Year", kpi, log=TRUE)
  plot(bpn)
  plot(bpl)
  summary(LCV_ANOVA(data,"Year", kpi))
  tuk<<-LCV_TUKEY(data,"Year", kpi)
  plot(tuk)
  readline(prompt="Press [enter] to continue")
  LCV_Apple_Theme()
  cat("%%%%%%%%%%%%%%%% Conditioned")
  bpn<-LCV_boxplot(data,"LARVA", kpi, log=FALSE)
  bpl<-LCV_boxplot(data,"LARVA", kpi, log=TRUE)
  plot(bpn)
  plot(bpl)
  show(LCV_STUDENT(data, "LARVA", kpi))
}

original <- DBAGroups15.21
outlierlevel<-100000
outliergroups<- original[(original$Sessions > outlierlevel), "Group"]
# outliers <- boxplot(DBAGroups15.21[,c("nYear","Sessions")],"nYear","Sessions")$out
# outliergroups <- original[(original$Sessions %in% outliers), "Group"]
cat("Eliminating groups ",outliergroups)
Groups  <-  original[!(original$Group %in% outliergroups),]
GroupsPre <- Groups
# LCV_Normality(Groups,"Sessions")
# LCV_boxplot(Groups, "Year", "Sessions")
# tuk<-LCV_TUKEY(Groups,"Year", "Sessions")
# tuk
#
#
# Battery(Groups, "Grade")
# LCV_boxplot(Groups,"Year","Grade")
# LCV_boxplot(Groups,"LARVA","Grade")
# LCV_STUDENT(Groups,"LARVA","Problems.Solved",onetail = TRUE)
# LCV_STUDENT(Groups,"LARVA","Grade",onetail = TRUE)
# LCV_STUDENT(Groups,"LARVA","TotalNetTime",onetail = TRUE)
# LCV_STUDENT(Groups,"LARVA","Closing.Time",onetail = TRUE)

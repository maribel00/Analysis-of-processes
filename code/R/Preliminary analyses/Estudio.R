# Study of the segmented data

# Step 0: Install and load package "xtable" to convert outputs into latex tables
# install.packages("xtable")
library(xtable)

# Step 1: Import the data
# Import .csv file
setwd("/home/maribel/Escritorio/5º\ DGIIM/TFG/Analysis-of-processes/code/datasets") # Change working directory
dataS <- read.csv("datasetS.csv", header = TRUE)
head <- head(dataS)
head
#    group year size mean_grade                date map action
# 1 Girtab 1617    6       4.67 15/11/2016 7:31:31    1      1
# 2 Girtab 1617    6       4.67 15/11/2016 7:31:53    1      1
# 3 Girtab 1617    6       4.67 15/11/2016 12:50:03   1      1
# 4 Girtab 1617    6       4.67 15/11/2016 12:50:28   1      1
# 5 Girtab 1617    6       4.67 15/11/2016 13:27:27   1      1
# 6 Girtab 1617    6       4.67 15/11/2016 13:27:58   1      1
table <- as.matrix(head)
table
#   group    year   size mean_grade date                  map action
# 1 "Girtab" "1617" "6"  "4.67"     "15/11/2016 7:31:31 " "1" "1"   
# 2 "Girtab" "1617" "6"  "4.67"     "15/11/2016 7:31:53 " "1" "1"   
# 3 "Girtab" "1617" "6"  "4.67"     "15/11/2016 12:50:03" "1" "1"   
# 4 "Girtab" "1617" "6"  "4.67"     "15/11/2016 12:50:28" "1" "1"   
# 5 "Girtab" "1617" "6"  "4.67"     "15/11/2016 13:27:27" "1" "1"   
# 6 "Girtab" "1617" "6"  "4.67"     "15/11/2016 13:27:58" "1" "1"  

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# dimension of the dataset
dim(dataS)
# [1] 2397    7

dataA <- read.csv("datasetA.csv", header = TRUE)
head <- head(dataA)
head
#       group year size mean_grade                date map action
# 1 Bellatrix 1920    4      6.835 05/11/2019 10:24:51   1      1
# 2 Bellatrix 1920    4      6.835 05/11/2019 10:24:51   1      2
# 3 Bellatrix 1920    4      6.835 05/11/2019 10:25:04   1      1
# 4 Bellatrix 1920    4      6.835 05/11/2019 10:25:26   1      2
# 5 Bellatrix 1920    4      6.835 05/11/2019 10:25:39   1      1
# 6 Bellatrix 1920    4      6.835 05/11/2019 10:25:45   1      2
table <- as.matrix(head)
table
#   group       year   size mean_grade date                  map action
# 1 "Bellatrix" "1920" "4"  "6.835"    "05/11/2019 10:24:51" "1" "1"   
# 2 "Bellatrix" "1920" "4"  "6.835"    "05/11/2019 10:24:51" "1" "2"   
# 3 "Bellatrix" "1920" "4"  "6.835"    "05/11/2019 10:25:04" "1" "1"   
# 4 "Bellatrix" "1920" "4"  "6.835"    "05/11/2019 10:25:26" "1" "2"   
# 5 "Bellatrix" "1920" "4"  "6.835"    "05/11/2019 10:25:39" "1" "1"   
# 6 "Bellatrix" "1920" "4"  "6.835"    "05/11/2019 10:25:45" "1" "2" 

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# dimension of the dataset
dim(dataA)
# [1] 7053    7

dataN <- read.csv("datasetN.csv", header = TRUE)
head <- head(dataN)
head
#      group year size mean_grade                date map action
# 1 Achernar 1516    5        8.2 17/10/2015 19:41:45   0      0
# 2 Achernar 1516    5        8.2 22/10/2015 17:29:21   1      1
# 3 Achernar 1516    5        8.2 22/10/2015 17:29:22   1      2
# 4 Achernar 1516    5        8.2 22/10/2015 17:29:39   1      3
# 5 Achernar 1516    5        8.2 22/10/2015 17:34:09   1      1
# 6 Achernar 1516    5        8.2 22/10/2015 17:34:10   1      2
table <- as.matrix(head)
table
#   group      year   size mean_grade date                  map action
# 1 "Achernar" "1516" "5"  "8.2"      "17/10/2015 19:41:45" "0" "0"   
# 2 "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:29:21" "1" "1"   
# 3 "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:29:22" "1" "2"   
# 4 "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:29:39" "1" "3"   
# 5 "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:34:09" "1" "1"   
# 6 "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:34:10" "1" "2" 

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# dimension of the dataset
dim(dataN)
# [1] 18840     7

dataSOB <- read.csv("datasetSOB.csv", header = TRUE)
head <- head(dataSOB)
head
#       group year size mean_grade                date map action
# 1 Bellatrix 1617    5       9.72 07/11/2016 11:13:31   1      1
# 2 Bellatrix 1617    5       9.72 07/11/2016 11:36:59   1      1
# 3 Bellatrix 1617    5       9.72 07/11/2016 11:41:07   1      1
# 4 Bellatrix 1617    5       9.72 07/11/2016 11:42:22   1      1
# 5 Bellatrix 1617    5       9.72 07/11/2016 11:45:37   1      1
# 6 Bellatrix 1617    5       9.72 07/11/2016 11:49:23   1      1
table <- as.matrix(head)
table
#   group       year   size mean_grade date                  map action
# 1 "Bellatrix" "1617" "5"  "9.72"     "07/11/2016 11:13:31" "1" "1"   
# 2 "Bellatrix" "1617" "5"  "9.72"     "07/11/2016 11:36:59" "1" "1"   
# 3 "Bellatrix" "1617" "5"  "9.72"     "07/11/2016 11:41:07" "1" "1"   
# 4 "Bellatrix" "1617" "5"  "9.72"     "07/11/2016 11:42:22" "1" "1"   
# 5 "Bellatrix" "1617" "5"  "9.72"     "07/11/2016 11:45:37" "1" "1"   
# 6 "Bellatrix" "1617" "5"  "9.72"     "07/11/2016 11:49:23" "1" "1"  

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# dimension of the dataset
dim(dataSOB)
# [1] 17891     7

dataMH <- read.csv("datasetMH.csv", header = TRUE)
head <- head(dataMH)
head
#      group year size mean_grade                date map action
# 1 Achernar 1617    5         10 09/11/2016 21:23:22   1      1
# 2 Achernar 1617    5         10 09/11/2016 21:25:11   1      1
# 3 Achernar 1617    5         10 10/11/2016 0:21:37    1      1
# 4 Achernar 1617    5         10 10/11/2016 0:22:50    1      1
# 5 Achernar 1617    5         10 10/11/2016 0:23:12    1      1
# 6 Achernar 1617    5         10 10/11/2016 0:25:53    1      1
table <- as.matrix(head)
table
#   group      year   size mean_grade date                  map action
# 1 "Achernar" "1617" "5"  "10"       "09/11/2016 21:23:22" "1" "1"   
# 2 "Achernar" "1617" "5"  "10"       "09/11/2016 21:25:11" "1" "1"   
# 3 "Achernar" "1617" "5"  "10"       "10/11/2016 0:21:37 " "1" "1"   
# 4 "Achernar" "1617" "5"  "10"       "10/11/2016 0:22:50 " "1" "1"   
# 5 "Achernar" "1617" "5"  "10"       "10/11/2016 0:23:12 " "1" "1"   
# 6 "Achernar" "1617" "5"  "10"       "10/11/2016 0:25:53 " "1" "1" 

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# dimension of the dataset
dim(dataMH)
# [1] 1646    7

# Step 2: Plot a box plot
# Map
dataS.boxplot.map <- boxplot(dataS$map~dataS$year, xlab = "Year", ylab = "Map", las = 2)
title("Boxplot of Year by Map (Suspenso)")
dataS.boxplot.map$out # outliers
# numeric(0)

# Action
dataS.boxplot.action <- boxplot(dataS$action~dataS$year, xlab = "Year", ylab = "Action", las = 2)
title("Boxplot of Year by Action (Suspenso)")
dataS.boxplot.action$out # outliers
# numeric(0)

# Map
dataA.boxplot.map <- boxplot(dataA$map~dataA$year, xlab = "Year", ylab = "Map", las = 2)
title("Boxplot of Year by Map (Aprobado)")
dataA.boxplot.map$out # outliers
#   [1] 7 7 7 8 8 8 8 8 9 9 9 9 9 9 7 7 7 9 9 9 9 9 9 7 7 7 7 7 9 9 9 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 8 8
#  [58] 8 8 8 9 9 9 4 4 5 5 5 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 4 4 5 5 5 5 5 5 5 5 5 5 5 4 4 4 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7
# [115] 7 7 7 7 7 7 7 7 4 4 4 4 4 6 6 6 6 6 8 8 8 9 9 9 9 8 8 8 8 8 8

dataA.NO <- dataA[!(dataA$map %in% dataA.boxplot.map$out),]

# dimension of the dataset
dim(dataA.NO)
# [1] 3545    7

# Action
dataA.boxplot.action <- boxplot(dataA$action~dataA$year, xlab = "Year", ylab = "Action", las = 2)
title("Boxplot of Year by Action (Aprobado)")
dataA.boxplot.action$out # outliers
# numeric(0)

# Map
dataN.boxplot.map <- boxplot(dataN$map~dataN$year, xlab = "Year", ylab = "Map", las = 2)
title("Boxplot of Year by Map (Notable)")
dataN.boxplot.map$out # outliers
# numeric(0)

# Action
dataN.boxplot.action <- boxplot(dataN$action~dataN$year, xlab = "Year", ylab = "Action", las = 2)
title("Boxplot of Year by Action (Notable)")
dataN.boxplot.action$out # outliers
# numeric(0)

# Map
dataSOB.boxplot.map <- boxplot(dataSOB$map~dataSOB$year, xlab = "Year", ylab = "Map", las = 2)
title("Boxplot of Year by Map (Sobre.)")
dataSOB.boxplot.map$out # outliers
# numeric(0)

# Action
dataSOB.boxplot.action <- boxplot(dataSOB$action~dataSOB$year, xlab = "Year", ylab = "Action", las = 2)
title("Boxplot of Year by Action (Sobre.)")
dataSOB.boxplot.action$out # outliers
# numeric(0)

# Map
dataMH.boxplot.map <- boxplot(dataMH$map~dataMH$year, xlab = "Year", ylab = "Map", las = 2)
title("Boxplot of Year by Map (M.H.)")
dataMH.boxplot.map$out # outliers
# numeric(0)

# Action
dataMH.boxplot.action <- boxplot(dataMH$action~dataMH$year, xlab = "Year", ylab = "Action", las = 2)
title("Boxplot of Year by Action (M.H.)")
dataMH.boxplot.action$out # outliers
# numeric(0)

# Step 3: Compute the two way ANOVA test
# Suspenso and M.H. datasets is not necessary because the data is only from one year

# Aprobado
anova_two_way.A <- aov(year~map*action, data = dataA.NO)
summary <- summary(anova_two_way.A)
table <- as.matrix(summary)
table
#               Df   Sum Sq Mean Sq F value   Pr(>F)    
# map            1  4962967 4962967 182.695  < 2e-16 ***
# action         1   593110  593110  21.833 3.08e-06 ***
# map:action     1   121031  121031   4.455   0.0349 *  
# Residuals   3541 96192485   27165                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Obtaining the coefficients
coefficients.A <- coef(anova_two_way.A)
coefficients.A
# (Intercept)         map      action  map:action 
# 1659.499246   55.955952   18.358724   -5.477871 

table <- as.matrix(coefficients.A)
table
#                    [,1]
# (Intercept) 1659.499246
# map           55.955952
# action        18.358724
# map:action    -5.477871

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Interaction plot
interaction.plot(dataA.NO$map, dataA.NO$action, dataA.NO$year)

# Diagnostic plots
layout(matrix(1:4,ncol=2))
par(pty="s",mar=c(3,1,2,1)+0.1)
plot(anova_two_way.A)

# Notable
anova_two_way.N <- aov(year~map*action, data = dataN)
summary <- summary(anova_two_way.N)
table <- as.matrix(summary)
table
#                Df    Sum Sq Mean Sq F value Pr(>F)    
# map             1   4302465 4302465 210.824 <2e-16 ***
# action          1     38274   38274   1.875  0.171    
# map:action      1     13094   13094   0.642  0.423    
# Residuals   18836 384401361   20408                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Obtaining the coefficients
coefficients.N <- coef(anova_two_way.N)
coefficients.N
#  (Intercept)          map       action   map:action 
# 1664.2872074    4.6485013   -1.8893531    0.2333182

table <- as.matrix(coefficients.N)
table
#                     [,1]
# (Intercept) 1664.2872074
# map            4.6485013
# action        -1.8893531
# map:action     0.2333182

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Interaction plot
interaction.plot(dataN$map, dataN$action, dataN$year)

# Diagnostic plots
layout(matrix(1:4,ncol=2))
par(pty="s",mar=c(3,1,2,1)+0.1)
plot(anova_two_way.N)

# Sobresaliente
anova_two_way.SOB <- aov(year~map*action, data = dataSOB)
summary <- summary(anova_two_way.SOB)
table <- as.matrix(summary)
table
#                Df    Sum Sq  Mean Sq  F value Pr(>F)    
# map             1  36996263 36996263 1485.984 <2e-16 ***
# action          1      5760     5760    0.231  0.631    
# map:action      1     36808    36808    1.478  0.224    
# Residuals   17887 445329291    24897                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Obtaining the coefficients
coefficients.SOB <- coef(anova_two_way.SOB)
coefficients.SOB
#  (Intercept)          map       action   map:action 
# 1631.1254988   14.9809133   -1.8027238    0.3819704

table <- as.matrix(coefficients.SOB)
table
#                     [,1]
# (Intercept) 1631.1254988
# map           14.9809133
# action        -1.8027238
# map:action     0.3819704

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Interaction plot
interaction.plot(dataSOB$map, dataSOB$action, dataSOB$year)

# Diagnostic plots
layout(matrix(1:4,ncol=2))
par(pty="s",mar=c(3,1,2,1)+0.1)
plot(anova_two_way.SOB)
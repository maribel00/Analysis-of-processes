# Cleaning & Filtering the data

# Step 1: Import the data
# Import .csv file
setwd("/home/maribel/Escritorio/5º\ DGIIM/TFG/Analysis-of-processes/code/datasets") # Change working directory
data <- read.csv("DBA1520.csv", header = FALSE)

# dimension of the dataset
dim(data)
# [1] 47834     5

# assigning new names to the columns of the data frame
names(data) <- c('year','group','date','map','action')

# Print head and summary of data frame
print("Top 6 Entries of data frame:")
head(data)
#   year     group                date map action
# 1 1516  Achernar 17/10/2015 19:41:45   0      0
# 2 1516 Bellatrix 17/10/2015 19:41:45   0      0
# 3 1516  Cerastes 17/10/2015 19:41:45   0      0
# 4 1516  Denebola 17/10/2015 19:41:45   0      0
# 5 1516    Elnath 17/10/2015 19:41:45   0      0
# 6 1516     Furud 17/10/2015 19:41:45   0      0
print("Summary:")
summary(data)
#      year              group               date                map            action         
# Length:47834       Length:47834       Length:47834       Min.   :0.000   Min.   :    0.000  
# Class :character   Class :character   Class :character   1st Qu.:1.000   1st Qu.:    1.000  
# Mode  :character   Mode  :character   Mode  :character   Median :3.000   Median :    2.000  
#                                                          Mean   :3.834   Mean   :    3.062  
#                                                          3rd Qu.:6.000   3rd Qu.:    3.000  
#                                                          Max.   :9.000   Max.   :31617.000  
#                                                                          NA's   :3  

# Step 2: Take a look at the data
# install.packages("dplyr")
library(dplyr)
glimpse(data)
# Rows: 47,834
# Columns: 5
# $ year   <chr> "1516", "1516", "1516", "1516", "1516", "1516", "1516", "1516", "1516", "1516", "1516", "1516", "1516"…
# $ group  <chr> "Achernar", "Bellatrix", "Cerastes", "Denebola", "Elnath", "Furud", "Girtab", "Haldus", "Izar", "Acher…
# $ date   <chr> "17/10/2015 19:41:45", "17/10/2015 19:41:45", "17/10/2015 19:41:45", "17/10/2015 19:41:45", "17/10/201…
# $ map    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ action <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

# Step 3: Check the format of the variable poison
levels(data$map)
# NULL

# Step 4: Clean the dataset

# Delete rows with nulls
data <- na.omit(data)
summary(data)
#     year              group               date                map            action         
# Length:47831       Length:47831       Length:47831       Min.   :0.000   Min.   :    0.000  
# Class :character   Class :character   Class :character   1st Qu.:1.000   1st Qu.:    1.000  
# Mode  :character   Mode  :character   Mode  :character   Median :3.000   Median :    2.000  
#                                                          Mean   :3.834   Mean   :    3.062  
#                                                          3rd Qu.:6.000   3rd Qu.:    3.000  
#                                                          Max.   :9.000   Max.   :31617.000 

# Filter the data
data <- filter(data,data$action<=5 & data$action>=0)
summary(data)
#     year              group               date                map            action     
# Length:47828       Length:47828       Length:47828       Min.   :0.000   Min.   :0.000  
# Class :character   Class :character   Class :character   1st Qu.:1.000   1st Qu.:1.000  
# Mode  :character   Mode  :character   Mode  :character   Median :3.000   Median :2.000  
#                                                          Mean   :3.834   Mean   :2.325  
#                                                          3rd Qu.:6.000   3rd Qu.:3.000  
#                                                          Max.   :9.000   Max.   :5.000  

FullScrumADL <- read.csv("FullScrumADL.csv", row.names=1)
FullScrumADL.lm<-lm(EC~Leader,FullScrumADL)
FullScrumADL.aov <- aov(FullScrumADL.lm)
summary(FullScrumADL.aov)
# Df Sum Sq Mean Sq F value Pr(>F)
# Leader         2   4.89  2.4437   107.5 <2e-16 *
#   Residuals   6133 139.36  0.0227
# ---
#   Signif. codes:  0 ‘*’ 0.001 ‘*’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1
FullScrumADL.tukey <- TukeyHSD(FullScrumADL.aov)
FullScrumADL.tukey
# Tukey multiple comparisons of means
# 95% family-wise confidence level
#
# Fit: aov(formula = FullScrumADL.lm)
#
# $Leader
# diff          lwr         upr     p adj
# D-A  0.07278430  0.060193214  0.08537538 0.0000000
# L-A  0.01181177  0.001092603  0.02253094 0.0265295
# L-D -0.06097253 -0.072281607 -0.04966345 0.0000000
plot(FullScrumADL.tukey)
FullScrumADL.boxplot <- boxplot(EC~Leader,FullScrumADL)
FullScrumADL.boxplot$out
# [1]  0.479  0.499  0.519  0.538  0.515  0.534  0.554  0.573  0.528  0.508  0.528 -0.315 -0.295  0.482 -0.490 -0.470 -0.472
# [18] -0.463 -0.444 -0.424 -0.415 -0.396 -0.376 -0.356 -0.337 -0.317 -0.297  0.472  0.477  0.478  0.469  0.488  0.479  0.499
# [35]  0.605  0.624  0.626  0.646  0.665  0.602  0.621  0.593  0.613  0.632  0.591  0.611  0.612  0.632  0.590  0.500  0.519
# [52]  0.539  0.534  0.553  0.573  0.593  0.550  0.570  0.590  0.572  0.592  0.611  0.613  0.632  0.652  0.671  0.691  0.485
# [69]  0.505  0.525  0.492  0.511  0.484  0.504  0.524  0.491 -0.291 -0.395 -0.375 -0.355 -0.336 -0.316 -0.297  0.500  0.519
# [86]  0.539  0.505  0.525  0.545  0.564  0.573  0.593  0.487  0.507  0.526  0.546  0.565  0.585  0.488  0.508  0.528  0.547
# [103]  0.567  0.495  0.514  0.534
FullScrumADL.NO <- FullScrumADL[!(FullScrumADL$EC %in% FullScrumADL.boxplot$out),]
summary(FullScrumADL)
# Leader                EC
# Length:6136        Min.   :-0.4900
# Class :character   1st Qu.: 0.0060
# Mode  :character   Median : 0.0940
# Mean   : 0.1147
# 3rd Qu.: 0.2040
# Max.   : 0.6910
summary(FullScrumADL.NO)
# Leader                EC
# Length:6008        Min.   :-0.278
# Class :character   1st Qu.: 0.005
# Mode  :character   Median : 0.091
# Mean   : 0.109
# 3rd Qu.: 0.197
# Max.   : 0.582
FullScrumADL.NO.lm<-lm(EC~Leader,FullScrumADL.NO)
FullScrumADL.NO.aov<-aov(FullScrumADL.NO.lm)
summary(FullScrumADL.NO.aov)

# Step 5: Save the dataset 

# Write the cleaned dataset into a .csv file
write.csv(data, "cleandataset.csv", row.names = FALSE, col.names = TRUE)
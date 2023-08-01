# Two way ANOVA Test

# Step 0: Install and load package "xtable" to convert outputs into latex tables
# install.packages("xtable")
library(xtable)

# Step 1: Import the data
# Import .csv file
setwd("/home/maribel/Escritorio/5º\ DGIIM/TFG/Analysis-of-processes/code/datasets") # Change working directory
data <- read.csv("cleandataset.csv", header = TRUE)

# dimension of the dataset
dim(data)
# [1] 47828     5

# Print head and summary of data frame
print("Top 6 Entries of data frame:")
head <- head(data)
table <- as.matrix(head)
table
#   year   group       date                  map action
# 1 "1516" "Achernar"  "17/10/2015 19:41:45" "0" "0"   
# 2 "1516" "Bellatrix" "17/10/2015 19:41:45" "0" "0"   
# 3 "1516" "Cerastes"  "17/10/2015 19:41:45" "0" "0"   
# 4 "1516" "Denebola"  "17/10/2015 19:41:45" "0" "0"   
# 5 "1516" "Elnath"    "17/10/2015 19:41:45" "0" "0"   
# 6 "1516" "Furud"     "17/10/2015 19:41:45" "0" "0" 

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

print("Summary:")
summary <- summary(data)
table <- as.matrix(summary)
table
#      year         group               date                map            action     
# Min.   :1516   Length:47828       Length:47828       Min.   :0.000   Min.   :0.000  
# 1st Qu.:1516   Class :character   Class :character   1st Qu.:1.000   1st Qu.:1.000  
# Median :1617   Mode  :character   Mode  :character   Median :3.000   Median :2.000  
# Mean   :1700                                         Mean   :3.834   Mean   :2.325  
# 3rd Qu.:1920                                         3rd Qu.:6.000   3rd Qu.:3.000  
# Max.   :1920                                         Max.   :9.000   Max.   :5.000 

# Transform table into a latex table:
print(xtable(table), include.rownames = FALSE)

# Step 2: Convert the variable group as ordered level and the variables map and
# action as double.
# install.packages("dplyr")
library(dplyr)
data[,2]<-as.ordered(data[,2])
data[,3]<-as.character.Date(data[,3])
glimpse(data)
# Rows: 47,828
# Columns: 5
# $ year   <int> 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, 1516, …
# $ group  <ord> Achernar, Bellatrix, Cerastes, Denebola, Elnath, Furud, Girtab, Haldus, Izar, Achernar, Achernar, Ache…
# $ date   <chr> "17/10/2015 19:41:45", "17/10/2015 19:41:45", "17/10/2015 19:41:45", "17/10/2015 19:41:45", "17/10/201…
# $ map    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
# $ action <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 1, 2, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

# Step 3: Check the format of the variable group
levels(data$group)
# [1] "Achernar"  "Bellatrix" "Castor"    "Cerastes"  "Denebola"  "Elnath"    "Furud"     "Girtab"    "Haedus"   
# [10] "Haldus"    "Izar"      "Jabbah"    "Jih"       "Keid"      "Lesath"    "Maasym"    "Nahn"   

# Check NA values
any(is.na(data))
# [1] FALSE

# Check if the values of the variables map and action are numeric
is.numeric(data$map)
# [1] TRUE
is.numeric(data$action)
# [1] TRUE

# Step 4: Plot a box plot
# install.packages("ggplot2") # Install it again
library(ggplot2) # Load the library (you have to do this one on each new session)
ggplot(data, aes(x = map, y = year, fill = as.factor(map))) +
  geom_boxplot() +
  theme_classic() +
  #ggtitle("Boxplot of Map by Year") +
  scale_fill_brewer(palette = "Paired")

# Calculate outliers
outliers <- boxplot.stats(data$map)$out

# Print outliers
print(outliers)
# integer(0)

ggplot(data, aes(x = action, y = year, fill = as.factor(action))) +
  geom_boxplot() +
  theme_classic() +
  #ggtitle("Boxplot of Action by Year") + 
  scale_fill_brewer(palette = "Paired")


# Calculate outliers
outliers <- boxplot.stats(data$action)$out

# Print outliers
print(outliers)
# integer(0)

layout(1)
boxplot.map <- boxplot(data$map, ylab = "Map")

data.boxplot.map <- boxplot(data$map~data$year, xlab = "Year", ylab = "Map", las = 2)
#title("Boxplot of Year by Map")
data.boxplot.map$out # outliers
# numeric(0)

boxplot.action <- boxplot(data$action, ylab = "Action")

data.boxplot.action <- boxplot(data$action~data$year, xlab = "Year", ylab = "Action", las = 2)
# title("Boxplot of Year by Action")
data.boxplot.action$out # outliers
# numeric(0)

data.NO.map <- data[!(data$map %in% data.boxplot.map$out),]
summary(data.NO.map)
#      year            group           date                map            action     
# Min.   :1516   Elnath   : 6617   Length:47828       Min.   :0.000   Min.   :0.000  
# 1st Qu.:1516   Bellatrix: 6222   Class :character   1st Qu.:1.000   1st Qu.:1.000  
# Median :1617   Girtab   : 4862   Mode  :character   Median :3.000   Median :2.000  
# Mean   :1700   Furud    : 4703                      Mean   :3.834   Mean   :2.325  
# 3rd Qu.:1920   Haldus   : 4521                      3rd Qu.:6.000   3rd Qu.:3.000  
# Max.   :1920   Denebola : 4512                      Max.   :9.000   Max.   :5.000  
#                (Other)  :16391                                               

data.NO.map.action <- data[!(data$action %in% data.boxplot.action$out),]
summary(data.NO.map.action)
#      year            group           date                map            action     
# Min.   :1516   Elnath   : 6617   Length:47828       Min.   :0.000   Min.   :0.000  
# 1st Qu.:1516   Bellatrix: 6222   Class :character   1st Qu.:1.000   1st Qu.:1.000  
# Median :1617   Girtab   : 4862   Mode  :character   Median :3.000   Median :2.000  
# Mean   :1700   Furud    : 4703                      Mean   :3.834   Mean   :2.325  
# 3rd Qu.:1920   Haldus   : 4521                      3rd Qu.:6.000   3rd Qu.:3.000  
# Max.   :1920   Denebola : 4512                      Max.   :9.000   Max.   :5.000  
#                (Other)  :16391      

# Calculate the inferior and superior limit
q1 <- quantile(data$map, 0.25)
q3 <- quantile(data$map, 0.75)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Select only the values within the limits
data_no_outliers <- subset(data, map >= lower_bound & map <= upper_bound)
dim(data_no_outliers)
# [1] 47828     5

# Calculate the inferior and superior limit
q1 <- quantile(data$action, 0.25)
q3 <- quantile(data$action, 0.75)
iqr <- q3 - q1
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Select only the values within the limits
data_no_outliers <- subset(data, action >= lower_bound & action <= upper_bound)
dim(data_no_outliers)
# [1] 47828     5

# Step 5: Compute the two way ANOVA test
anova_two_way <- aov(year~map*action, data = data)
summary <- summary(anova_two_way)
table <- as.matrix(summary)
table
#                Df    Sum Sq  Mean Sq  F value Pr(>F)    
# map             1 6.292e+07 62918414 2689.147 <2e-16 ***
# action          1 1.012e+05   101245    4.327 0.0375 *  
# map:action      1 8.797e+03     8797    0.376 0.5398    
# Residuals   47824 1.119e+09    23397                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Obtaining the coefficients
coefficients <- coef(anova_two_way)
coefficients
#  (Intercept)          map       action   map:action 
# 1655.0253915   12.3144501   -1.5051580    0.1176494 

table <- as.matrix(coefficients)
table
#                     [,1]
# (Intercept) 1655.0253915
# map           12.3144501
# action        -1.5051580
# map:action     0.1176494

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Interaction plot
layout(1)
par(legend.justification = "right")
interaction.plot(data$map, data$action, data$year)

# Diagnostic plots
layout(matrix(1:4,ncol=2))
par(pty="s",mar=c(3,1,2,1)+0.1)
plot(anova_two_way)

# Run a pairwise t-test
data$map <- as.factor(data$map)
data$action <- as.factor(data$action)
anova_two_way <- aov(year~map*action, data = data)
summary <- summary(anova_two_way)
summary
#                Df    Sum Sq  Mean Sq F value  Pr(>F)    
# map             9 9.373e+07 10414659  462.78 < 2e-16 ***
# action          4 1.392e+06   348089   15.47 1.2e-12 ***
# map:action     32 1.154e+07   360697   16.03 < 2e-16 ***
# Residuals   47782 1.075e+09    22504                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coef(anova_two_way)

data.hsd <- TukeyHSD(anova_two_way)
data.hsd

table <- as.matrix(data.hsd$map)
table

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

layout(1)
plot(data.hsd)
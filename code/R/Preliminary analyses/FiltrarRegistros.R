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

# Step 5: Save the dataset 

# Write the cleaned dataset into a .csv file
write.csv(data, "cleandataset.csv", row.names = FALSE)
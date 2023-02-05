# Segmentation

# Step 0: Install and load package "xtable" to convert outputs into latex tables
# install.packages("xtable")
library(xtable)

# Step 1: Import the data
# Import .csv file
setwd("/home/maribel/Escritorio/5º\ DGIIM/TFG/Analysis-of-processes/code/datasets") # Change working directory
data <- read.csv("DBA1520GRADED.csv", header = TRUE)

# dimension of the dataset
dim(data)
# [1] 118   5

# Print head and summary of data frame
print("Top 6 Entries of data frame:")
head <- head(data)
head
#   Group           Team Size Year Grade
# 1    G1 DBA 1819 P3 GL    4 1819 10,00
# 2    G2 DBA 1920 P3 GJ    4 1920  4,01
# 3    G3 DBA 1819 P2 GH    4 1819  7,96
# 4    G4 DBA 1920 P2 GE    4 1920  8,95
# 5    G5 DBA 1920 P3 GK    4 1920  4,51
# 6    G6 DBA 1415 P3 G6    6 1415  7,20
table <- as.matrix(head)
table
#   Group Team             Size Year   Grade  
# 1 "G1"  "DBA 1819 P3 GL" "4"  "1819" "10,00"
# 2 "G2"  "DBA 1920 P3 GJ" "4"  "1920" "4,01" 
# 3 "G3"  "DBA 1819 P2 GH" "4"  "1819" "7,96" 
# 4 "G4"  "DBA 1920 P2 GE" "4"  "1920" "8,95" 
# 5 "G5"  "DBA 1920 P3 GK" "4"  "1920" "4,51" 
# 6 "G6"  "DBA 1415 P3 G6" "6"  "1415" "7,20" 

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

print("Summary:")
summary <- summary(data)
summary
#    Group               Team                Size            Year         Grade          
# Length:118         Length:118         Min.   :3.000   Min.   :1314   Length:118        
# Class :character   Class :character   1st Qu.:4.000   1st Qu.:1516   Class :character  
# Mode  :character   Mode  :character   Median :5.000   Median :1718   Mode  :character  
#                                       Mean   :4.831   Mean   :1662                     
#                                       3rd Qu.:6.000   3rd Qu.:1819                     
#                                       Max.   :6.000   Max.   :1920
table <- as.matrix(summary)
table
#    Group               Team                Size            Year         Grade          
# Length:118         Length:118         Min.   :3.000   Min.   :1314   Length:118        
# Class :character   Class :character   1st Qu.:4.000   1st Qu.:1516   Class :character  
# Mode  :character   Mode  :character   Median :5.000   Median :1718   Mode  :character  
#                                       Mean   :4.831   Mean   :1662                     
#                                       3rd Qu.:6.000   3rd Qu.:1819                     
#                                       Max.   :6.000   Max.   :1920 

# Transform table into a latex table:
print(xtable(table), include.rownames = FALSE)

# Step 2: Clean the data
# deleting duplicates
data_sin_duplicados <- data[!duplicated(data), ]

# dimension of the dataset
dim(data_sin_duplicados)
# [1] 118   5

# Check NA values
any(is.na(data))
# [1] FALSE

# Step 3: Explore the data
# exploration of the tuples per year
data_filtrado <- data[data$Year == 1314, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 24   G24 DBA 1314 P3 G1    6 1314 10,00
# 41   G41 DBA 1314 P3 G4    6 1314  7,40
# 44   G44 DBA 1314 P2 G5    6 1314  8,80
# 45   G45 DBA 1314 P3 G7    6 1314  9,40
# 47   G47 DBA 1314 P3 G6    6 1314  7,60
# 49   G49 DBA 1314 P3 G8    6 1314  7,48

# exploration of the tuples per year
data_filtrado <- data[data$Year == 1415, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 6     G6 DBA 1415 P3 G6    6 1415  7,20
# 15   G15 DBA 1415 P3 G3    5 1415  8,10
# 19   G19 DBA 1415 P2 G4    5 1415  8,10
# 20   G20 DBA 1415 P3 G9    6 1415  6,60
# 22   G22 DBA 1415 P2 G3    5 1415 10,00
# 26   G26 DBA 1415 P2 G1    5 1415  9,00

# exploration of the tuples per year
data_filtrado <- data[data$Year == 1516, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 14   G14 DBA 1516 P3 G3    6 1516  9,60
# 21   G21 DBA 1516 P3 G4    6 1516  4,40
# 23   G23 DBA 1516 P3 G1    5 1516  8,60
# 29   G29 DBA 1516 P3 G9    3 1516 10,00
# 32   G32 DBA 1516 P2 G3    6 1516 10,00
# 42   G42 DBA 1516 P2 G1    5 1516  7,80

data_filtrado <- data[data$Year == 1617, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 9     G9 DBA 1617 P2 G6    5 1617  8,86
# 11   G11 DBA 1617 P2 G2    5 1617  9,44
# 13   G13 DBA 1617 P3 G9    4 1617 10,00
# 17   G17 DBA 1617 P2 G9    5 1617 10,00
# 28   G28 DBA 1617 P2 G8    6 1617  9,75
# 33   G33 DBA 1617 P2 G4    6 1617  8,79

data_filtrado <- data[data$Year == 1718, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 10   G10 DBA 1718 P2 GH    5 1718  6,46
# 31   G31 DBA 1718 P3 GH    5 1718  4,22
# 37   G37 DBA 1718 P2 GA    5 1718  7,40
# 43   G43 DBA 1718 P3 GE    4 1718  9,65
# 48   G48 DBA 1718 P3 GD    5 1718  7,88
# 61   G61 DBA 1718 P3 GA    5 1718  9,26

data_filtrado <- data[data$Year == 1819, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 1     G1 DBA 1819 P3 GL    4 1819 10,00
# 3     G3 DBA 1819 P2 GH    4 1819  7,96
# 7     G7 DBA 1819 P2 GB    4 1819  8,23
# 12   G12 DBA 1819 P2 GK    4 1819  9,64
# 16   G16 DBA 1819 P2 GJ    4 1819  8,14
# 25   G25 DBA 1819 P2 GI    4 1819  7,40

data_filtrado <- data[data$Year == 1920, ]
head(data_filtrado)
#    Group           Team Size Year Grade
# 2     G2 DBA 1920 P3 GJ    4 1920  4,01
# 4     G4 DBA 1920 P2 GE    4 1920  8,95
# 5     G5 DBA 1920 P3 GK    4 1920  4,51
# 8     G8 DBA 1920 P3 GB    4 1920  5,45
# 18   G18 DBA 1920 P2 GB    4 1920  8,22
# 27   G27 DBA 1920 P2 GF    4 1920  8,94

# Step 4: Import the Map-Action dataset
# Import .csv file
data.map.action <- read.csv("cleandataset.csv", header = TRUE)
head(data.map.action)
#   year     group                date map action
# 1 1516  Achernar 17/10/2015 19:41:45   0      0
# 2 1516 Bellatrix 17/10/2015 19:41:45   0      0
# 3 1516  Cerastes 17/10/2015 19:41:45   0      0
# 4 1516  Denebola 17/10/2015 19:41:45   0      0
# 5 1516    Elnath 17/10/2015 19:41:45   0      0
# 6 1516     Furud 17/10/2015 19:41:45   0      0

# Step 5: Find the unique values of the column group
print(unique(data.map.action$group))
#  [1] "Achernar"  "Bellatrix" "Cerastes"  "Denebola"  "Elnath"    "Furud"     "Girtab"    "Haldus"    "Izar"     
# [10] "Jih"       "Castor"    "Haedus"    "Jabbah"    "Keid"      "Lesath"    "Maasym"    "Nahn"

# Step 6: Explore the Map-Action dataset
print(unique(data.map.action$group[data.map.action$year == 1516]))
# [1] "Achernar"  "Bellatrix" "Cerastes"  "Denebola"  "Elnath"    "Furud"     "Girtab"    "Haldus"    "Izar"
print(unique(data.map.action$group[data.map.action$year == 1617]))
#  [1] "Achernar"  "Bellatrix" "Cerastes"  "Denebola"  "Elnath"    "Furud"     "Girtab"    "Haldus"    "Izar"     
# [10] "Jih"
print(unique(data.map.action$group[data.map.action$year == 1718]))
# [1] "Achernar"  "Bellatrix" "Cerastes"  "Denebola"  "Elnath"    "Girtab"    "Haldus"  
print(unique(data.map.action$group[data.map.action$year == 1819]))
# character(0)
print(unique(data.map.action$group[data.map.action$year == 1920]))
#  [1] "Castor"    "Denebola"  "Elnath"    "Furud"     "Haedus"    "Izar"      "Jabbah"    "Keid"      "Lesath"   
# [10] "Maasym"    "Nahn"      "Bellatrix"

# Step 7: Replace the values of the column Team of the first dataset

# A-1 Achernar
# B-2 Bellatrix
# C-3 Cerastes
# D-4 Denebola
# E-5 Elnath
# F-6 Furud
# G-7 Girtab
# H-8 Haldus
# I-9 Izar
# J-10 Jih
# K-11 Keid
# L-12 Lesath
# M-13 Maasym
# N-14 Nahn

data$Team[data$Team == "DBA 1314 P2 G1" | data$Team == "DBA 1314 P3 G1"] <- "Achernar"
data$Team[data$Team == "DBA 1415 P2 G1" | data$Team == "DBA 1415 P3 G1"] <- "Achernar"
data$Team[data$Team == "DBA 1516 P2 G1" | data$Team == "DBA 1516 P3 G1"] <- "Achernar"
data$Team[data$Team == "DBA 1617 P2 G1" | data$Team == "DBA 1617 P3 G1"] <- "Achernar"
data$Team[data$Team == "DBA 1718 P2 GA" | data$Team == "DBA 1718 P3 GA"] <- "Achernar"

data$Team[data$Team == "DBA 1516 P2 G2" | data$Team == "DBA 1516 P3 G2"] <- "Bellatrix"
data$Team[data$Team == "DBA 1617 P2 G2" | data$Team == "DBA 1617 P3 G2"] <- "Bellatrix"
data$Team[data$Team == "DBA 1718 P2 GB" | data$Team == "DBA 1718 P3 GB"] <- "Bellatrix"
data$Team[data$Team == "DBA 1819 P2 GB" | data$Team == "DBA 1819 P3 GB"] <- "Bellatrix"
data$Team[data$Team == "DBA 1920 P2 GB" | data$Team == "DBA 1920 P3 GB"] <- "Bellatrix"

data$Team[data$Team == "DBA 1314 P2 G3" | data$Team == "DBA 1314 P3 G3"] <- "Cerastes"
data$Team[data$Team == "DBA 1415 P2 G3" | data$Team == "DBA 1415 P3 G3"] <- "Cerastes"
data$Team[data$Team == "DBA 1516 P2 G3" | data$Team == "DBA 1516 P3 G3"] <- "Cerastes"
data$Team[data$Team == "DBA 1718 P2 GC" | data$Team == "DBA 1718 P3 GC"] <- "Cerastes"
data$Team[data$Team == "DBA 1819 P2 GC" | data$Team == "DBA 1819 P3 GC"] <- "Cerastes"

data$Team[data$Team == "DBA 1920 P2 GC" | data$Team == "DBA 1920 P3 GC"] <- "Castor"

data$Team[data$Team == "DBA 1314 P2 G4" | data$Team == "DBA 1314 P3 G4"] <- "Denebola"
data$Team[data$Team == "DBA 1415 P2 G4" | data$Team == "DBA 1415 P3 G4"] <- "Denebola"
data$Team[data$Team == "DBA 1516 P2 G4" | data$Team == "DBA 1516 P3 G4"] <- "Denebola"
data$Team[data$Team == "DBA 1617 P2 G4" | data$Team == "DBA 1617 P3 G4"] <- "Denebola"
data$Team[data$Team == "DBA 1718 P2 GD" | data$Team == "DBA 1718 P3 GD"] <- "Denebola"
data$Team[data$Team == "DBA 1819 P2 GD" | data$Team == "DBA 1819 P3 GD"] <- "Denebola"
data$Team[data$Team == "DBA 1920 P2 GD" | data$Team == "DBA 1920 P3 GD"] <- "Denebola"

data$Team[data$Team == "DBA 1314 P2 G5" | data$Team == "DBA 1314 P3 G5"] <- "Elnath"
data$Team[data$Team == "DBA 1516 P2 G5" | data$Team == "DBA 1516 P3 G5"] <- "Elnath"
data$Team[data$Team == "DBA 1617 P2 G5" | data$Team == "DBA 1617 P3 G5"] <- "Elnath"
data$Team[data$Team == "DBA 1718 P2 GE" | data$Team == "DBA 1718 P3 GE"] <- "Elnath"
data$Team[data$Team == "DBA 1819 P2 GE" | data$Team == "DBA 1819 P3 GE"] <- "Elnath"
data$Team[data$Team == "DBA 1920 P2 GE" | data$Team == "DBA 1920 P3 GE"] <- "Elnath"

data$Team[data$Team == "DBA 1314 P2 G6" | data$Team == "DBA 1314 P3 G6"] <- "Furud"
data$Team[data$Team == "DBA 1415 P2 G6" | data$Team == "DBA 1415 P3 G6"] <- "Furud"
data$Team[data$Team == "DBA 1516 P2 G6" | data$Team == "DBA 1516 P3 G6"] <- "Furud"
data$Team[data$Team == "DBA 1617 P2 G6" | data$Team == "DBA 1617 P3 G6"] <- "Furud"
data$Team[data$Team == "DBA 1819 P2 GF" | data$Team == "DBA 1819 P3 GF"] <- "Furud"
data$Team[data$Team == "DBA 1920 P2 GF" | data$Team == "DBA 1920 P3 GF"] <- "Furud"

data$Team[data$Team == "DBA 1314 P2 G7" | data$Team == "DBA 1314 P3 G7"] <- "Girtab"
data$Team[data$Team == "DBA 1415 P2 G7" | data$Team == "DBA 1415 P3 G7"] <- "Girtab"
data$Team[data$Team == "DBA 1516 P2 G7" | data$Team == "DBA 1516 P3 G7"] <- "Girtab"
data$Team[data$Team == "DBA 1617 P2 G7" | data$Team == "DBA 1617 P3 G7"] <- "Girtab"
data$Team[data$Team == "DBA 1718 P2 GG" | data$Team == "DBA 1718 P3 GG"] <- "Girtab"
data$Team[data$Team == "DBA 1819 P2 GG" | data$Team == "DBA 1819 P3 GG"] <- "Girtab"

data$Team[data$Team == "DBA 1314 P2 G8" | data$Team == "DBA 1314 P3 G8"] <- "Haldus"
data$Team[data$Team == "DBA 1415 P2 G8" | data$Team == "DBA 1415 P3 G8"] <- "Haldus"
data$Team[data$Team == "DBA 1516 P2 G8" | data$Team == "DBA 1516 P3 G8"] <- "Haldus"
data$Team[data$Team == "DBA 1617 P2 G8" | data$Team == "DBA 1617 P3 G8"] <- "Haldus"
data$Team[data$Team == "DBA 1718 P2 GH" | data$Team == "DBA 1718 P3 GH"] <- "Haldus"
data$Team[data$Team == "DBA 1819 P2 GH" | data$Team == "DBA 1819 P3 GH"] <- "Haldus"

data$Team[data$Team == "DBA 1920 P2 GH" | data$Team == "DBA 1920 P3 GH"] <- "Haedus"

data$Team[data$Team == "DBA 1415 P2 G9" | data$Team == "DBA 1415 P3 G9"] <- "Izar"
data$Team[data$Team == "DBA 1516 P2 G9" | data$Team == "DBA 1516 P3 G9"] <- "Izar"
data$Team[data$Team == "DBA 1617 P2 G9" | data$Team == "DBA 1617 P3 G9"] <- "Izar"
data$Team[data$Team == "DBA 1819 P2 GI" | data$Team == "DBA 1819 P3 GI"] <- "Izar"
data$Team[data$Team == "DBA 1920 P2 GI" | data$Team == "DBA 1920 P3 GI"] <- "Izar"

data$Team[data$Team == "DBA 1415 P2 G10" | data$Team == "DBA 1415 P3 G10"] <- "Jih"
data$Team[data$Team == "DBA 1617 P2 G10" | data$Team == "DBA 1617 P3 G10"] <- "Jih"
data$Team[data$Team == "DBA 1819 P2 GJ" | data$Team == "DBA 1819 P3 GJ"] <- "Jih"

data$Team[data$Team == "DBA 1920 P2 GJ" | data$Team == "DBA 1920 P3 GJ"] <- "Jabbah"

data$Team[data$Team == "DBA 1819 P2 GK" | data$Team == "DBA 1819 P3 GK"] <- "Keid"
data$Team[data$Team == "DBA 1920 P2 GK" | data$Team == "DBA 1920 P3 GK"] <- "Keid"

data$Team[data$Team == "DBA 1819 P2 GL" | data$Team == "DBA 1819 P3 GL"] <- "Lesath"
data$Team[data$Team == "DBA 1920 P2 GL" | data$Team == "DBA 1920 P3 GL"] <- "Lesath"

data$Team[data$Team == "DBA 1920 P2 GM" | data$Team == "DBA 1920 P3 GM"] <- "Maasym"

data$Team[data$Team == "DBA 1920 P2 GN" | data$Team == "DBA 1920 P3 GN"] <- "Nahn"

head(data)
#   Group   Team Size Year Grade
# 1    G1 Lesath    4 1819 10,00
# 2    G2 Jabbah    4 1920  4,01
# 3    G3 Haldus    4 1819  7,96
# 4    G4 Elnath    4 1920  8,95
# 5    G5   Keid    4 1920  4,51
# 6    G6  Furud    6 1415  7,20

print(unique(data$Team))
#  [1] "Lesath"    "Jabbah"    "Haldus"    "Elnath"    "Keid"      "Furud"     "Bellatrix" "Izar"      "Cerastes" 
# [10] "Jih"       "Denebola"  "Achernar"  "Maasym"    "Girtab"    "Castor"    "Haedus"    "Nahn"     

# Step 8: Prepare the columns of the dataset data

# Delete column Group of dataset data
drop <- c("Group")
data = data[,!(names(data) %in% drop)]

# Rename columns of dataset data
# Get column names
colnames(data)
# [1] "Team"  "Size"  "Year"  "Grade"

# Rename column where name is "Team"
names(data)[names(data) == "Team"] <- "group"
# Rename column where name is "Size"
names(data)[names(data) == "Size"] <- "size"
# Rename column where name is "Year"
names(data)[names(data) == "Year"] <- "year"
# Rename column where name is "Grade"
names(data)[names(data) == "Grade"] <- "grade"
head <- head(data)
head
#    group size year grade
# 1 Lesath    4 1819 10,00
# 2 Jabbah    4 1920  4,01
# 3 Haldus    4 1819  7,96
# 4 Elnath    4 1920  8,95
# 5   Keid    4 1920  4,51
# 6  Furud    6 1415  7,20
table <- as.matrix(head)
table
#   group    size year   grade  
# 1 "Lesath" "4"  "1819" "10,00"
# 2 "Jabbah" "4"  "1920" "4,01" 
# 3 "Haldus" "4"  "1819" "7,96" 
# 4 "Elnath" "4"  "1920" "8,95" 
# 5 "Keid"   "4"  "1920" "4,51" 
# 6 "Furud"  "6"  "1415" "7,20"

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Group by mean using dplyr
library(dplyr)
glimpse(data)
# Rows: 118
# Columns: 4
# $ group <chr> "Lesath", "Jabbah", "Haldus", "Elnath", "Keid", "Furud", "Bellatrix", "Bellatrix", "Furud", "Haldus", "…
# $ size  <int> 4, 4, 4, 4, 4, 6, 4, 4, 5, 5, 5, 4, 4, 6, 5, 4, 5, 4, 5, 6, 6, 5, 5, 6, 4, 5, 4, 6, 3, 6, 5, 6, 6, 5, 4…
# $ year  <int> 1819, 1920, 1819, 1920, 1920, 1415, 1819, 1920, 1617, 1718, 1617, 1819, 1617, 1516, 1415, 1819, 1617, 1…
# $ grade <chr> "10,00", "4,01", "7,96", "8,95", "4,51", "7,20", "8,23", "5,45", "8,86", "6,46", "9,44", "9,64", "10,00…

data[,4] <- as.double(gsub(",", ".", data[,4]))
glimpse(data)
# Rows: 118
# Columns: 4
# $ group <chr> "Lesath", "Jabbah", "Haldus", "Elnath", "Keid", "Furud", "Bellatrix", "Bellatrix", "Furud", "Haldus", "…
# $ size  <int> 4, 4, 4, 4, 4, 6, 4, 4, 5, 5, 5, 4, 4, 6, 5, 4, 5, 4, 5, 6, 6, 5, 5, 6, 4, 5, 4, 6, 3, 6, 5, 6, 6, 5, 4…
# $ year  <int> 1819, 1920, 1819, 1920, 1920, 1415, 1819, 1920, 1617, 1718, 1617, 1819, 1617, 1516, 1415, 1819, 1617, 1…
# $ grade <dbl> 10.00, 4.01, 7.96, 8.95, 4.51, 7.20, 8.23, 5.45, 8.86, 6.46, 9.44, 9.64, 10.00, 9.60, 8.10, 8.14, 10.00…

data_grouped <- data %>%
  group_by(group, year) %>%
  summarize(size = mean(size), mean_grade = mean(grade))
head <- head(data_grouped)
head
# A tibble: 6 × 4
# Groups:   group [2]
#   group      year  size mean_grade
#   <chr>     <int> <dbl>      <dbl>
# 1 Achernar   1314     6       9.4 
# 2 Achernar   1415     5       9   
# 3 Achernar   1516     5       8.2 
# 4 Achernar   1617     5      10   
# 5 Achernar   1718     5       8.33
# 6 Bellatrix  1516     5       8.2 
table <- as.matrix(head)
table
#      group       year   size mean_grade
# [1,] "Achernar"  "1314" "6"  " 9.40"   
# [2,] "Achernar"  "1415" "5"  " 9.00"   
# [3,] "Achernar"  "1516" "5"  " 8.20"   
# [4,] "Achernar"  "1617" "5"  "10.00"   
# [5,] "Achernar"  "1718" "5"  " 8.33"   
# [6,] "Bellatrix" "1516" "5"  " 8.20" 

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Step 9: Inner join of the two datasets
data_joined <- inner_join(data_grouped, data.map.action, by = c("year", "group"))
head <- head(data_joined)
head
# A tibble: 6 × 7
# Groups:   group [1]
#   group     year  size mean_grade date                  map action
#   <chr>    <int> <dbl>      <dbl> <chr>               <int>  <int>
# 1 Achernar  1516     5        8.2 17/10/2015 19:41:45     0      0
# 2 Achernar  1516     5        8.2 22/10/2015 17:29:21     1      1
# 3 Achernar  1516     5        8.2 22/10/2015 17:29:22     1      2
# 4 Achernar  1516     5        8.2 22/10/2015 17:29:39     1      3
# 5 Achernar  1516     5        8.2 22/10/2015 17:34:09     1      1
# 6 Achernar  1516     5        8.2 22/10/2015 17:34:10     1      2
table <- as.matrix(head)
table
#      group      year   size mean_grade date                  map action
# [1,] "Achernar" "1516" "5"  "8.2"      "17/10/2015 19:41:45" "0" "0"   
# [2,] "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:29:21" "1" "1"   
# [3,] "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:29:22" "1" "2"   
# [4,] "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:29:39" "1" "3"   
# [5,] "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:34:09" "1" "1"   
# [6,] "Achernar" "1516" "5"  "8.2"      "22/10/2015 17:34:10" "1" "2"   

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Step 10: Segmentation of the new joined dataset
# Suspenso
data_filtered <- data_joined %>%
  filter(mean_grade >= 0 & mean_grade < 5)
head(data_filtered)
# A tibble: 6 × 7
# Groups:   group [1]
#   group   year  size mean_grade date                    map action
#   <chr>  <int> <dbl>      <dbl> <chr>                 <int>  <int>
# 1 Girtab  1617     6       4.67 "15/11/2016 7:31:31 "     1      1
# 2 Girtab  1617     6       4.67 "15/11/2016 7:31:53 "     1      1
# 3 Girtab  1617     6       4.67 "15/11/2016 12:50:03"     1      1
# 4 Girtab  1617     6       4.67 "15/11/2016 12:50:28"     1      1
# 5 Girtab  1617     6       4.67 "15/11/2016 13:27:27"     1      1
# 6 Girtab  1617     6       4.67 "15/11/2016 13:27:58"     1      1

# Write the cleaned dataset into a .csv file
write.csv(data_filtered, "datasetS.csv", row.names = FALSE, col.names = TRUE)

# Aprobado
data_filtered <- data_joined %>%
  filter(mean_grade >= 5 & mean_grade < 7)
head(data_filtered)
# A tibble: 6 × 7
# Groups:   group [1]
#   group      year  size mean_grade date                  map action
#   <chr>     <int> <dbl>      <dbl> <chr>               <int>  <int>
# 1 Bellatrix  1920     4       6.84 05/11/2019 10:24:51     1      1
# 2 Bellatrix  1920     4       6.84 05/11/2019 10:24:51     1      2
# 3 Bellatrix  1920     4       6.84 05/11/2019 10:25:04     1      1
# 4 Bellatrix  1920     4       6.84 05/11/2019 10:25:26     1      2
# 5 Bellatrix  1920     4       6.84 05/11/2019 10:25:39     1      1
# 6 Bellatrix  1920     4       6.84 05/11/2019 10:25:45     1      2

# Write the cleaned dataset into a .csv file
write.csv(data_filtered, "datasetA.csv", row.names = FALSE, col.names = TRUE)

# Notable
data_filtered <- data_joined %>%
  filter(mean_grade >= 7 & mean_grade < 9)
head(data_filtered)
# A tibble: 6 × 7
# Groups:   group [1]
#   group     year  size mean_grade date                  map action
#   <chr>    <int> <dbl>      <dbl> <chr>               <int>  <int>
# 1 Achernar  1516     5        8.2 17/10/2015 19:41:45     0      0
# 2 Achernar  1516     5        8.2 22/10/2015 17:29:21     1      1
# 3 Achernar  1516     5        8.2 22/10/2015 17:29:22     1      2
# 4 Achernar  1516     5        8.2 22/10/2015 17:29:39     1      3
# 5 Achernar  1516     5        8.2 22/10/2015 17:34:09     1      1
# 6 Achernar  1516     5        8.2 22/10/2015 17:34:10     1      2

# Write the cleaned dataset into a .csv file
write.csv(data_filtered, "datasetN.csv", row.names = FALSE, col.names = TRUE)

# Sobresaliente
data_filtered <- data_joined %>%
  filter(mean_grade >= 9 & mean_grade < 10)
head(data_filtered)
# A tibble: 6 × 7
# Groups:   group [1]
#   group      year  size mean_grade date                  map action
#   <chr>     <int> <dbl>      <dbl> <chr>               <int>  <int>
# 1 Bellatrix  1617     5       9.72 07/11/2016 11:13:31     1      1
# 2 Bellatrix  1617     5       9.72 07/11/2016 11:36:59     1      1
# 3 Bellatrix  1617     5       9.72 07/11/2016 11:41:07     1      1
# 4 Bellatrix  1617     5       9.72 07/11/2016 11:42:22     1      1
# 5 Bellatrix  1617     5       9.72 07/11/2016 11:45:37     1      1
# 6 Bellatrix  1617     5       9.72 07/11/2016 11:49:23     1      1

# Write the cleaned dataset into a .csv file
write.csv(data_filtered, "datasetSOB.csv", row.names = FALSE, col.names = TRUE)

# Matrícula de Honor
data_filtered <- data_joined %>%
  filter(mean_grade == 10)
head(data_filtered)
# A tibble: 6 × 7
# Groups:   group [1]
#   group     year  size mean_grade date                    map action
#   <chr>    <int> <dbl>      <dbl> <chr>                 <int>  <int>
# 1 Achernar  1617     5         10 "09/11/2016 21:23:22"     1      1
# 2 Achernar  1617     5         10 "09/11/2016 21:25:11"     1      1
# 3 Achernar  1617     5         10 "10/11/2016 0:21:37 "     1      1
# 4 Achernar  1617     5         10 "10/11/2016 0:22:50 "     1      1
# 5 Achernar  1617     5         10 "10/11/2016 0:23:12 "     1      1
# 6 Achernar  1617     5         10 "10/11/2016 0:25:53 "     1      1

# Write the cleaned dataset into a .csv file
write.csv(data_filtered, "datasetMH.csv", row.names = FALSE, col.names = TRUE)
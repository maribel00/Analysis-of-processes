# Cleaning & Filtering the data

# Step 1: Import the data
# Import .csv file
setwd("/home/maribel/Escritorio/5º\ DGIIM/TFG/Analysis-of-processes/code/datasets") # Change working directory
data <- read.csv("DBA1520.csv", header = FALSE)

# dimension of the dataset
dim(data)

# assigning new names to the columns of the data frame
names(data) <- c('year','group','date','map','action')

# Print head and summary of data frame
print("Top 6 Entries of data frame:")
head(data)
print("Summary:")
summary(data)

# Step 2: Convert the variables species, branch, location and transpiration as ordered level
# install.packages("dplyr")
library(dplyr)
data[,1]<-as.ordered(data[,1])
data[,2]<-as.ordered(data[,2])
data[,3]<-as.character.Date(data[,3])
data[,4]<-as.ordered(data[,4])
data[,5]<-as.ordered(data[,5])
glimpse(data)

# Step 3: Check the format of the variable poison
levels(data$map)

# Step 4: Clean the dataset

# Delete rows with nulls
data <- na.omit(data)
summary(data)

# Filter the data
data <- filter(data,data$action<=5 & data$action>=0)
summary(data)

# Step 5: Save the dataset 

# Write the cleaned dataset into a .csv file
write.csv(data, "cleandataset.csv", row.names = FALSE, col.names = TRUE)


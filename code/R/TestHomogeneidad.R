# One way ANOVA Test

# Step 1: Import the data
# Import .csv file
setwd("/home/maribel/Escritorio/5º\ DGIIM/TFG/Analysis-of-processes/code/datasets") # Change working directory
data <- read.csv("DBA1520.csv", header = FALSE)
data

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
data[,2]<-as.ordered(data[,2])
data[,4]<-as.ordered(data[,4])
data[,5]<-as.ordered(data[,5])
glimpse(data)

# Step 3: Check the format of the variable poison
levels(data$map)

# Step 4: Cleaning the dataset

# Delete rows with nulls
data <- na.omit(data)
summary(data)

# Write the cleaned dataset into a .csv file

# Step 4: Plot a box plot
# install.packages("ggplot2") # Install it again
library(ggplot2) # Load the library (you have to do this one on each new session)
ggplot(data, aes(x = year, y = map, fill = year)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

ggplot(data, aes(x = year, y = action, fill = year)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

# Step 5: Compute the two way ANOVA test
anova_two_way <- aov(year~map * action, data = data)
summary(anova_two_way)

# Obtaining the coefficients
coef(anova_two_way)

# Interaction plot
interaction.plot(data$map, data$action, data$year)

# Diagnostic plots
layout(matrix(1:4,ncol=2))
par(pty="s",mar=c(3,1,2,1)+0.1)
plot(anova_two_way)

# Run a pairwise t-test
data.hsd <- TukeyHSD(anova_two_way, "map")
data.hsd

layout(1)
plot(data.hsd)
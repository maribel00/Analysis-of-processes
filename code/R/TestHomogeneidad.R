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

# Print head and summary of data frame
print("Top 6 Entries of data frame:")
head <- head(data)
table <- as.matrix(head)
table

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

print("Summary:")
summary <- summary(data)
table <- as.matrix(summary)
table

# Transform table into a latex table:
print(xtable(table), include.rownames = FALSE)

# Step 2: Convert the variables species, branch, location and transpiration as ordered level
# install.packages("dplyr")
library(dplyr)
data[,1]<-as.character.default(data[,1])
data[,2]<-as.ordered(data[,2])
data[,3]<-as.character.Date(data[,3])
data[,4]<-as.ordered(data[,4])
data[,5]<-as.ordered(data[,5])
glimpse(data)

# Step 3: Check the format of the variable poison
levels(data$map)

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
summary <- summary(anova_two_way)
table <- as.matrix(summary)
table

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Obtaining the coefficients
coefficients <- coef(anova_two_way)
coefficients

table <- as.matrix(coefficients)
table

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

# Interaction plot
interaction.plot(data$map, data$action, data$year)

# Diagnostic plots
layout(matrix(1:4,ncol=2))
par(pty="s",mar=c(3,1,2,1)+0.1)
plot(anova_two_way)

# Run a pairwise t-test
data.hsd <- TukeyHSD(anova_two_way, "map")
data.hsd

table <- as.matrix(data.hsd$map)
table

# Transform table into a latex table:
print(xtable(table), include.rownames = TRUE)

layout(1)
plot(data.hsd)
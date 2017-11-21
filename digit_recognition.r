library(caTools)
library(pls)
library(ggplot2)
library(gridExtra)


df <- read.table('ZIP data/zip_train.dat')

# check for NAs
sum(is.na(df))

# center the data
df[, -1] <- scale(df[, -1], scale = FALSE)

# Select a 5% random sample (without replacement) of the train data.
# Use this sample as your training data, and the complete test data for testing.
train.samples <- sample(nrow(df), 0.05 * nrow(df))

X.train <- df[train.samples, -1]
Y.train <- df[train.samples, 1]
X.test <- df[-train.samples, -1]
Y.test <- df[-train.samples, 1]
library(caTools)
library(pls)
library(ggplot2)
library(gridExtra)


df <- read.table('ZIP data/zip_train.dat')

# check for NAs
sum(is.na(df))

# center the data
df[, -1] <- scale(df[, -1], scale = FALSE)
df
# Select a 5% random sample (without replacement) of the train data.
# Use this sample as your training data, and the complete test data for testing.
train.samples <- sample(nrow(df), 0.05 * nrow(df))

X.train <- as.matrix(df[train.samples, -1])
Y.train <- df[train.samples, 1]
X.test <- as.matrix(df[-train.samples, -1])
Y.test <- df[-train.samples, 1]

oneHot <- function(x) {
  result <- rep(0, 10)
  result[x + 1] <- 1
  return(result)
}

Y.train.one.hot <- t(sapply(Y.train, oneHot))
Y.test.one.hot <- t(sapply(Y.test, oneHot))

# Perform a multivariate regression with the training data.
mreg <- lm(Y.train.one.hot ~ X.train - 1) 

# Compute the average R2.
mreg.summaries = summary(mreg)
r.squared.mean = mean(sapply(mreg.summaries, function(x) x$r.squared))

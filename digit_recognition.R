library(caTools)
library(pls)
library(ggplot2)
library(gridExtra)

# Read the data
df.train <- read.table('ZIP data/zip_train.dat')
df.test <- read.table('ZIP data/zip_test.dat')

# check for NAs
sum(is.na(df.train))

# center the data
df.train[, -1] <- scale(df.train[, -1], scale = FALSE)
df.test[, -1] <- scale(df.test[, -1], scale = FALSE)

# Select a 5% random sample (without replacement) of the train data.
# Use this sample as your training data, and the complete test data for testing.
train.samples <- sample(nrow(df.train), 0.05 * nrow(df.train))

X.train <- as.matrix(df.train[train.samples, -1])
Y.train <- df.train[train.samples, 1]

X.test <- as.matrix(df.test[, -1])
Y.test <- df.test[, 1]

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
r.squared.train.mean = mean(sapply(mreg.summaries, function(x) x$r.squared))

# Compute the average of the R2 by Leave One Out.
PRESS <- colSums((mreg$residuals / (1 - ls.diag(mreg)$hat)) ^ 2)
r.squared.train.cv.mean <- mean(1 - PRESS / (diag(var(Y.train.one.hot)) * (nrow(Y.train.one.hot) - 1)))

# Predict the responses in the test data.
predictions.one.hot.test <- X.test %*% mreg$coefficients

# Compute the average R2 in the test data.
TSS = apply(Y.test.one.hot, 2, function(x) {
  sum((x - mean(x)) ^ 2)
})
RSS = colSums((Y.test.one.hot - predictions.one.hot.test) ^ 2)
r.squared.test.mean = mean(1 - (RSS / TSS))

# Assign every test individual to the maximum response and compute the error rate.
predictions.test <- as.numeric(apply(predictions.one.hot.test, 1, which.max) - 1)
sum(predictions.test != as.numeric(Y.test)) / length(predictions.test)

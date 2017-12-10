# library(caTools)
# library(pls)
# library(ggplot2)
# library(gridExtra)
library(plsdepot)
# library(calibrate)

# Read the data
setwd("~/miri/kblmm/multivariate_modelling/digit_recognition")
df.train <- read.table('data/zip_train.dat')
df.test <- read.table('data/zip_test.dat')

# check for NAs
sum(is.na(df.train))

# center the data
train.colMeans <- colMeans(df.train[, -1])
df.train[, -1] <- sweep(df.train[, -1], 2, train.colMeans)
df.test[, -1] <- sweep(df.test[, -1], 2, train.colMeans)

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

iba <- interbat(X.train, Y.train.one.hot, scaled = FALSE)

names(iba)

T <- iba$x.scores
nd <- 10

lmY <- lm(Y.train.one.hot~T[,1:nd])
dim(Y.train.one.hot)
dim(T[, 1:nd])
dim(lmY$coefficients[-1, ])
predictions.one.hot.train <- T[, 1:nd] %*% lmY$coefficients[-1, ] + lmY$coefficients[1, ]

# Assign every train individual to the maximum response and compute the error rate.
predictions.train <- as.numeric(apply(predictions.one.hot.train, 1, which.max) - 1)
sum(predictions.train != as.numeric(Y.train)) / length(predictions.train)

# Assign every test individual to the maximum response and compute the error rate.
T.test <- X.test %*% iba$x.wgs
predictions.one.hot.test <- T.test[, 1:nd] %*% lmY$coefficients[-1, ] + lmY$coefficients[1, ]
predictions.test <- as.numeric(apply(predictions.one.hot.test, 1, which.max) - 1)
sum(predictions.test != as.numeric(Y.test)) / length(predictions.test)

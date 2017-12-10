library(caTools)
library(pls)
library(ggplot2)
library(gridExtra)

# Read the data
df.train <- read.table('data/zip_train.dat')
df.test <- read.table('data/zip_test.dat')

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
TSS = diag(t(Y.test.one.hot) %*% Y.test.one.hot)
RSS = diag(t(Y.test.one.hot - predictions.one.hot.test) %*% (Y.test.one.hot - predictions.one.hot.test))
r.squared.test.mean = mean(1 - (RSS / TSS))


#####
mreg.test <- lm(Y.test.one.hot ~ X.test - 1)
mreg.test.summaries = summary(mreg.test)
r.squared.test.mean = mean(sapply(mreg.test.summaries, function(x) x$r.squared))

#####
# Assign every test individual to the maximum response and compute the error rate.
predictions.test <- as.numeric(apply(predictions.one.hot.test, 1, which.max) - 1)
sum(predictions.test != as.numeric(Y.test)) / length(predictions.test)

# Perform a PCR (using LOO). Decide how many components you retain for prediction.
pc <- pcr(Y.train.one.hot ~ X.train, validation = "LOO")
variance_explained = cumsum(explvar(pc))

var_exp = data.frame("Components" = 1:length(variance_explained), "Variance" = variance_explained)
ggplot(var_exp, aes(x=Components,y=Variance)) + geom_point() + 
  theme_bw() + ggtitle("PCR \nVariance Explained") +geom_hline(yintercept=80, col="red")

yhat = predict(pc, X.test)
dim(yhat[, , 2])
pc$coefficients
acc = c()
for(i in 1:100) {
  prediction <- unname(apply(yhat[, , i], 1, function(x) which.max(x) - 1))
  cm = table(Y.test, prediction)
  acc[i] = sum(diag(cm)) / sum(cm)
}
acc[37]
#Plotting the accuracies
accuracy_df = as.data.frame(cbind(
  "Components" = 1:100,
  "Accuracy" = acc,
  "Error" = 1 - acc
))

plot1 <- ggplot(accuracy_df, aes(x = Components, y = Accuracy)) +
  geom_line(col = "blue") + geom_point(col = "black") +
  theme_bw() + ggtitle("Test Data \nAccuracy")

plot2 <- ggplot(accuracy_df, aes(x = Components, y = Error)) +
  geom_line(col = "red") + geom_point(col = "black") + theme_bw() +
  ggtitle("Test Data \nError")

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

# Predict the responses in the test data.
prediction <- unname(apply(yhat[, , 37], 1, function(x) which.max(x) - 1))
cm = table(Y.test, prediction)
acc[i] = sum(diag(cm)) / sum(cm)

# Compute the average R2 in the test data.
TSS = apply(Y.test.one.hot, 2, function(x) {
  sum((x - mean(x)) ^ 2)
})
RSS = colSums((Y.test.one.hot - yhat[, , 37]) ^ 2)
r.squared.test.mean = mean(1 - (RSS / TSS))

# Assign every test individual to the maximum response and compute the error rate.
predictions.test <- as.numeric(apply(predictions.one.hot.test, 1, which.max) - 1)
sum(predictions.test != as.numeric(Y.test)) / length(predictions.test)


r2.test <- 1 - colSums((Y.test - pred.test)^2) / (apply(Y.test,2,var) * (n.test -1 ))

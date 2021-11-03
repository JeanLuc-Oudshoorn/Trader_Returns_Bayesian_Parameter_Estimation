library(moments)

means <- c()
sdev <- c()
skew <- c()
kurt <- c()

for (i in 1:ncol(detorocopy)){
  means[i] <- mean(na.omit(detorocopy[,i]))
  sdev[i] <- sd(na.omit(detorocopy[,i]))
  skew[i] <- skewness(na.omit(detorocopy[,i]))
  kurt[i] <- kurtosis(na.omit(detorocopy[,i]))
}

par(mfrow = c(2,2))
boxplot(means, horizontal = TRUE, main = "Means")
boxplot(sdev, horizontal = TRUE, main = "Standard Dev.")
boxplot(skew, horizontal = TRUE, main = "Skewness")
boxplot(kurt, horizontal = TRUE, main = "Kurtosis")


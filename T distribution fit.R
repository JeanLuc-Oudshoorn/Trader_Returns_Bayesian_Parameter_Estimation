library(dplyr)
library(tidyr)
library(xts)
library(fitdistrplus)
library(metRology)

fit <- data.frame(df = rep(0, 17), mean = rep(0, 17), sd = rep(0,17))
ses <- data.frame(dfs = rep(0, 17), means = rep(0, 17), sds = rep(0,17))
output <- c()

## 1.) Fit T-distributions to log return data on the basis of maximum likelihood
for (i in 1:ncol(popreturn)){
  
  output <- fitdist(as.vector(na.omit(coredata(popreturn[,i]))), "t.scaled",
                    start = list(df = 3,
                                 mean = mean(na.omit(coredata(popreturn[,i]))),
                                 sd = sd(na.omit(coredata(popreturn[,i]))))) 
  print(output)
  qqcomp(list(output))
  fit[i,] <- output$estimate
  ses[i,] <- output$sd
 
## 2.) Fit a log-normal distribution if the T-fit result contains a very high number of degrees of freedom (>100)
 if(fit$df[i]>100){
      output <- fitdist(as.vector(na.omit(coredata(popreturn[,i]))), "norm")
      print(output)
      fit$mean[i] <- output$estimate[1]
      fit$sd[i] <- output$estimate[2]
      ses$means[i] <- output$sd[1]
      ses$sds[i] <- output$sd[2]
  }
}

names <- colnames(popreturn)
rownames(fit) <- names
rownames(ses) <- names

fit <- cbind(fit, ses)

## 3.) Sample 10,000 observations from the most likely distributions with variability
samples <- matrix(nrow = 10000, ncol = 17)

for(i in 1:nrow(fit)){
  if(fit$df[i] < 100){
    
    samples[,i] <-  (rt(10000, df =  fit$df[i]) *
                        rnorm(10000, fit$sd[i], fit$sds[i]) + 
                        rnorm(10000, fit$mean[i], fit$means[i]))
  } else {
        samples[,i] <- (rnorm(10000, mean = rnorm(10000, fit$mean[i], fit$means[i]), sd = rnorm(10000, fit$sd[i], fit$sds[i])))
  }
}  

## 4.) Generate a table of performance metrics based on the large sample
colnames(samples) <- rownames(fit)

metrics <- as.data.frame(matrix(nrow = 17, ncol = 8))
rownames(metrics) <- colnames(samples)
colnames(metrics) <- c("99%", "VaR", "70%", "50%", "20%", "5%", "1%", "ExpPos")

for (i in 1:17){
  metrics[i,] <- quantile(samples[,i], c(0.01, 0.05, 0.3, 0.5, 0.8, 0.95, 0.99, 1))
  metrics$ExpPos[i] <- sum(samples[,i] > 0)/length(samples[,i])*100
}

metrics <- round(metrics[1:17,], 3)


## 5.) Wrangle the sample data to plot density functions
samples <- as.data.frame(samples)

long_samples <- pivot_longer(samples, cols = 1:17, names_to = "trader", values_to = "return")

sub_1 <- long_samples$trader %in% unique(long_samples$trader)[1:6]
long_samples[sub_1,]

  ggplot(long_samples[sub_1,], aes(x = return, color = trader)) +
  geom_density() +
  xlim(-0.2, 0.2) +
  ylim(0, 21)
  
sub_2 <- long_samples$trader %in% unique(long_samples$trader)[7:11]
long_samples[sub_2,]
  
  ggplot(long_samples[sub_2,], aes(x = return, color = trader)) +
    geom_density() +
    xlim(-0.2, 0.2) +
    ylim(0, 21)
  
sub_3 <- long_samples$trader %in% unique(long_samples$trader)[12:17]
long_samples[sub_3,]
  
  ggplot(long_samples[sub_3,], aes(x = return, color = trader)) +
    geom_density() +
    xlim(-0.2, 0.2) +
    ylim(0, 21)

  
  
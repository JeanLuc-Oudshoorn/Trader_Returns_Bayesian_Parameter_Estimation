library(dplyr)
library(QRM)

fit <- data.frame(nu = rep(0, 18), mu = rep(0, 18), sigma = rep(0,18))
ses <- data.frame(nus = rep(0, 18), mus = rep(0, 18), sigmas = rep(0,18))
output <- c()


for (i in 1:ncol(etorocopy)){
    if(i == 4 | i == 14){
      next
    }
    output <- fit.st(na.omit(etorocopy[,i]))
    print(output$par.ests)
    print(output$par.ses)
    fit[i,] <- output$par.ests
    ses[i,] <- output$par.ses
}

names <- colnames(etorocopy)
rownames(fit) <- names
rownames(ses) <- names

fit <- cbind(fit, ses)

attach(fit)
ranking <- fit[order(-nu),]
print(ranking)

samples <- matrix(nrow = 10000, ncol = 18)

for(i in 1:nrow(ranking)){
 samples[,i] <- (rt(10000, df = ranking$nu[i]) * rnorm(10000, ranking$sigma[i], ranking$sigmas[i]) + 
                   rnorm(10000, ranking$mu[i], ranking$mus[i]))
}
colnames(samples) <- rownames(ranking)

metrics <- as.data.frame(matrix(nrow = 18, ncol = 8))
rownames(metrics) <- colnames(samples)
colnames(metrics) <- c("99%", "VaR", "70%", "50%", "20%", "5%", "1%", "ExpPos")

for (i in 1:16){
  metrics[i,] <- quantile(samples[,i], c(0.01, 0.05, 0.3, 0.5, 0.8, 0.95, 0.99, 1))
  metrics$ExpPos[i] <- sum(samples[,i] > 0)/length(samples[,i])*100
}

metrics <- round(metrics[1:16,], 1)

samples <- as.data.frame(samples)

long_samples <- pivot_longer(samples, cols = 1:16, names_to = "trader", values_to = "return")
long_samples <- long_samples[,3:4]

sub_1 <- long_samples$trader %in% unique(long_samples$trader)[1:5]
long_samples[sub_1,]

  ggplot(long_samples[sub_1,], aes(x = return, color = trader)) +
  geom_density() +
  xlim(-15, 15) +
  ylim(0, 0.2)
  
sub_2 <- long_samples$trader %in% unique(long_samples$trader)[6:10]
long_samples[sub_2,]
  
  ggplot(long_samples[sub_2,], aes(x = return, color = trader)) +
    geom_density() +
    xlim(-15, 15) +
    ylim(0, 0.2)
  
sub_3 <- long_samples$trader %in% unique(long_samples$trader)[11:16]
long_samples[sub_3,]
  
  ggplot(long_samples[sub_3,], aes(x = return, color = trader)) +
    geom_density() +
    xlim(-15, 15) +
    ylim(0, 0.2)
  
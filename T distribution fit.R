library(dplyr)

fit <- data.frame(nu = rep(0, 18), mu = rep(0, 18), sigma = rep(0,18))
output <- c()

for (i in 1:ncol(etorocopy)){
    if(i == 4 | i == 14){
      next
    }
    output <- fit.st(na.omit(etorocopy[,i]))
    print(output$par.ests)
    fit[i,] <- output$par.ests
}

names <- colnames(etorocopy)
rownames(fit) <- names

attach(fit)
ranking <- fit[order(-nu),]
print(ranking)

samples <- matrix(nrow = 10000, ncol = 18)

for(i in 1:nrow(ranking)){
 samples[,i] <- (rt(10000, df = ranking$nu[i])*ranking$sigma[i] + ranking$mu[i])
}
colnames(samples) <- rownames(ranking)

exp_pos <- matrix(nrow = 18)

for (i in 1:16){
  exp_pos[i,] <- sum(samples[,i] > 0)/length(samples[,i])
}

rownames(exp_pos) <- colnames(samples)

long_samples <- pivot_longer(samples, cols = 1:16, names_to = "trader", values_to = "return")
long_samples <- long_samples[,3:4]

sub_1 <- long_samples$trader %in% unique(long_samples$trader)[1:5]
long_samples[sub_1,]

  ggplot(long_samples[sub_1,], aes(x = return, color = trader)) +
  geom_density() +
  xlim(-15, 15) +
  ylim(0, 0.175)
  
sub_2 <- long_samples$trader %in% unique(long_samples$trader)[6:10]
long_samples[sub_2,]
  
  ggplot(long_samples[sub_2,], aes(x = return, color = trader)) +
    geom_density() +
    xlim(-15, 15) +
    ylim(0, 0.175)
  
sub_3 <- long_samples$trader %in% unique(long_samples$trader)[11:16]
long_samples[sub_3,]
  
  ggplot(long_samples[sub_3,], aes(x = return, color = trader)) +
    geom_density() +
    xlim(-15, 15) +
    ylim(0, 0.175)
  
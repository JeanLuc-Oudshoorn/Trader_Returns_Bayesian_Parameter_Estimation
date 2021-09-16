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
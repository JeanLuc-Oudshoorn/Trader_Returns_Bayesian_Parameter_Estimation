library(BEST)
library(dplyr)

bayes_fit <- list()
bayes_fitsp <- list()

for(i in 1:15){
    df <- as.data.frame(etorocopy) %>%
      select(i, nasdaq) %>%
      na.omit() 
  
  BESTout <- BESTmcmc(df[,1], df$nasdaq)
    print(BESTout)
    plot(BESTout)

  bayes_fit[[i]] <- BESTout
}

traders <- colnames(etorocopy)
par(mfrow = c(2, 2))

for(i in 1:15){
   plot(bayes_fit[[i]], main = paste(traders[i], "performance vs. nasdaq"))
  }

for(i in 1:15){
  df <- as.data.frame(etorocopy) %>%
    select(i, sp) %>%
    na.omit() 
  
  BESTout <- BESTmcmc(df[,1], df$sp)
  print(BESTout)
  plot(BESTout)
  
  bayes_fitsp[[i]] <- BESTout
}

for(i in 1:15){
  plot(bayes_fitsp[[i]], main = paste(traders[i], "performance vs. S&P 500"))
}
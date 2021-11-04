library(BEST)
library(dplyr)

bayes_fit <- list()
bayes_fitsp <- list()

## 1.) Estimate outperformance of log returns vs NASDAQ
for(i in 1:15){
    df <- as.data.frame(popreturn) %>%
      select(i, nasdaq) %>%
      na.omit() 
  
  BESTout <- BESTmcmc(df[,1], df$nasdaq)
    print(BESTout)
    plot(BESTout)

  bayes_fit[[i]] <- BESTout
}

## 2.) Set up a plotting grid and generate the probability distributions
traders <- colnames(popreturn)
par(mfrow = c(2, 2))

for(i in 1:15){
   plot(bayes_fit[[i]], main = paste(traders[i], "performance vs. nasdaq"))
  }

## 3.) Estimate outperformance of log returns vs S&P500
for(i in 1:15){
  df <- as.data.frame(popreturn) %>%
    select(i, sp) %>%
    na.omit() 
  
  BESTout <- BESTmcmc(df[,1], df$sp)
  print(BESTout)
  plot(BESTout)
  
  bayes_fitsp[[i]] <- BESTout
}

## 4.) Plot outperformance against S&P500
for(i in 1:15){
  plot(bayes_fitsp[[i]], main = paste(traders[i], "performance vs. S&P 500"))
}


## 5.) Create a correlation matrix of returns of popular investors and those of indices
cormatrix <- matrix(nrow = 15, ncol = 2) 

for(i in 1:15){
  correl <- as.data.frame(popreturn) %>%
                  select(i, nasdaq) %>%
                  na.omit() %>%
                  cor()
cormatrix[i, 1] <- correl[1,2]
  
  correl <- as.data.frame(popreturn) %>%
                  select(i, sp) %>%
                  na.omit() %>%
                  cor()
cormatrix[i, 2] <- correl[1,2]
}

colnames(cormatrix) <- c("nasdaq", "s&p500")
rownames(cormatrix) <- c("jeppe", "mariano", "victor", "jurgen", "reinhardt", "martina", "wesley", "heloise",
                         "kieran", "harry", "richard", "lena", "eddy", "teoh", "libor")

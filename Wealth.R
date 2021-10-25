wealth <- function(principal = 53000, return = 1.20, inflation = 0.02, years = 20){
    df <- data.frame(year = 1:years, value = rep(0, years))
    for (i in 1:years){
      amount <- principal * (return * (1-inflation))^i
          if(amount <= 50000){
            df$value[i] <- amount
          } else if(amount <= 100000){
            df$value[i] <- amount - (amount - 50000)*0.01898*0.31
          } else if(amount <= 1000000){
            df$value[i] <- amount - (50000*0.01898*0.31) - (amount - 100000)*0.04501*0.31
          } else {
            df$value[i] <- amount - (50000*0.01898*0.31) - (900000*0.04501*0.31) - (amount - 1000000)*0.0569*0.31
          }
      }
    gain <- diff(df$value)   
    df$gain <- c(NA, gain)
    print(round(df, 0))
    print(paste("Inflation-adjusted millionaire after", min(which(df$value > 1000000)), "years!"))
    print(paste("You can safely stop working after", min(which(gain > 100000))+1, "years!"))
    plot(df$value, type = "l", col = "goldenrod")
    abline(h = 1000000, col = "lightgray")
    
}
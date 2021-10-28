wealth <- function(principal = 50000, return = 1.20, inflation = 0.02, years = 21){
    df <- data.frame(year = 1:(years+1), value = rep(principal, years+1), amount = rep(0, years+1),  tax = rep(0, years+1), ctax = rep(0, years+1), taxperct = rep(0, years+1), portf = rep(0, years + 1))
    for (i in 2:years){
      df$amount[i] <- df$value[(i-1)] * (return * (1-inflation))^(1)
      df$portf[i] <- principal * (return * (1 - inflation))^(i-1)
          if(df$amount[i] <= 50000){
            df$tax[i] <- 0
          } else if(df$amount[i] <= 100000){
            df$tax[i] <- (df$amount[i] - 50000)*0.01898*0.31
          } else if(df$amount[i] <= 1000000){
            df$tax[i] <- (50000*0.01898*0.31) + (df$amount[i] - 100000)*0.04501*0.31
          } else {
            df$tax[i] <- (50000*0.01898*0.31) + (900000*0.04501*0.31) + (df$amount[i] - 1000000)*0.0569*0.31
          }
      df$ctax[i] <- sum(df$tax[1:i])
      df$value[i] <- df$amount[i] - df$tax[i]
      df$taxperct[i] <- (1-(df$value[i] / df$portf[i]))*100
      }
    
    gain <- diff(df$value)   
    df$gain <- c(df$value[1] - principal, gain)
    rdf <- round(df[,-6], 0)
    taxperct <- round(df$taxperct, 2)
    rdf <- cbind(rdf, taxperct)
    print(rdf[1:years,])
    print(paste("Inflation-adjusted millionaire after", min(which(df$value > 1000000))-1, "years!"))
    print(paste("You can safely stop working after", min(which(gain > 100000)), "years!"))
    print(paste0("Total percent of portfolio value payed in taxes: ", max(taxperct), "%"))
    plot(df$value[-length(df$value)], type = "l", col = "goldenrod")
    abline(h = 1000000, col = "lightgray")
}
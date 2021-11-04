library(rvest)
library(stringr)
library(dplyr)
library(quantmod)
library(xts)


## 1.) Fetching the Nasdaq and S&P500 returns from Yahoo Finance
nasdaq <- getSymbols(Symbols = "^IXIC", src = "yahoo", auto.assign = FALSE)
sp <- getSymbols(Symbols = "^GSPC", src = "yahoo", auto.assign = FALSE)

nasdaq <- to.period(nasdaq, period = "months", k = 1, OHLC = FALSE)
sp <- to.period(sp, period = "months", k = 1, OHLC = FALSE)

nasdaq <- nasdaq["2013-07/2021-10"]
sp <- sp["2013-07/2021-10"]

idx <- index(nasdaq)

## 2.) Defining links to websites that contain return data for the popular investors
links <- c("https://factsheets.fundpeak.com/Report/473D3034AE5913E912265730BE689D6D707FA111F2B061DB4F473B892D69F1EAC3B08304018D5E90",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9896E0F6384B3ED10F5C2E5DBF7C6F06EF188F31677E4633FE79D364DFB1B1FE4",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9109B162D8ECFD3118D9D19EEB8B95D54F5B6B838F06055C79FC4C2F0D769E225",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9B18346822950C65D278266233E2B7BEF273197E704D25B2BA3B64565A31953D3",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9B18346822950C65D2225A49B71B9714BB23630CEAE4006E9FC757166B159D96E",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9109B162D8ECFD31182AB71E291E4B04B34CF0742481E80CD4E4A1A2DDDFE8142",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9896E0F6384B3ED10FB41C9C07425485BCAD0B69DECB5826138B63B73C77C9847",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9CB05597DFCB15842C2D8B04E7E7C653943047E140ED621CBB26513E5FA458973",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9B691508D769B17F4FA65D9DFE325EF95D2F6EBF4FE3CE2324322963A738273F9",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E93E82DE55FA2A5F76538DF3A83BED77CBEF2FA39C3597C366AD0605D56D07DD25",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9109B162D8ECFD3118765618479284A81620172FBA30804B2B5BF7A770A15A10E",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9B18346822950C65DB5D196E9CBEA145B4D7248CE5F16967163EA226B72DFB592",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9CB05597DFCB158425A194070CCEFAB3A5BAAAC04897205F77CCD820C8162F0A4",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9896E0F6384B3ED10A5D1C8B25061C87E70CC0E319A51387C05C37EDE5020FCDC",
           "https://factsheets.fundpeak.com/Report/473D3034AE5913E9896E0F6384B3ED106AC444D5B6E675BDA300E7B2858FA2233356598E13CD0646")


traderreturns <- list()

## 3.) Scraping and cleaning return values of popular investors from the website links
for(i in 1:length(links)){

  link <- read_html(links[i])
    
      txt <- link %>%
        html_nodes(xpath = '//*[@class = "monthlyPerf"]') %>%
        html_text() %>%
        str_extract_all("-?[0-9]+\\.[0-9]{2}") %>%
        unlist() %>%
        as.numeric()

## 4.) Reordering the returns and generating XTS-objects using the Nasdaq Index
l <- length(txt)

reorder <- c(10:1, 22:11, 34:23, 46:35, 58:47, 70:59, 82:71, 94:83, 106:95)

txt <- txt[order(reorder[1:l])]

traderreturns[[i]] <- xts(txt, order.by = rev(idx)[1:l])

}

## 5.) Calculating monthly returns for the Nasdaq and S&P 500 and merging them together with the popular investor returns into a single XTS-object
traderreturns[[16]] <- round(monthlyReturn(nasdaq)*100,2)
traderreturns[[17]] <- round(monthlyReturn(sp)*100,2)

popinvestor <- Reduce(merge, traderreturns)

popinvestor <- round(popinvestor,2)
colnames(popinvestor) <- c("jeppe", "mariano", "victor", "jurgen", "reinhardt", "martina", "wesley", "heloise",
                           "kieran", "harry", "richard", "lena", "eddy", "teoh", "libor", "nasdaq", "sp")


## 6.) Creating a separate matrix detailing total portfolio value by month based on monthly percentage returns 
tradervalue <- traderreturns

for (i in 1:length(traderreturns)){
  
    for (j in 1:length(traderreturns[[i]])){
        
      if(j == 1){
      tradervalue[[i]][j] <- (1 + (traderreturns[[i]][j]/100))*1000
      } else {
      tradervalue[[i]][j] <- coredata(tradervalue[[i]][(j - 1)]) * (1 + (coredata(traderreturns[[i]][j])/100))
      }
    }
}

popvalue <- Reduce(merge, tradervalue)

popvalue <- round(popvalue, 2)
colnames(popvalue) <- c("jeppe", "mariano", "victor", "jurgen", "reinhardt", "martina", "wesley", "heloise",
                        "kieran", "harry", "richard", "lena", "eddy", "teoh", "libor", "nasdaq", "sp")

## 7.) Using the portfolio value matrix to calculate log returns instead of percentage returns 
popreturn <- diff(log(popvalue))


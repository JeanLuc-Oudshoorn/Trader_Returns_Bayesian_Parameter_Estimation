library(httr)
library(rvest)
library(dplyr)
library(stringr)

response <- GET('https://ycharts.com/indicators/sp_500_monthly_return')
sp <- content(response)

sp %>%
  html_nodes(xpath = '//div[@class = "col-6"]//table/tbody//tr//td[@class = "text-right"]') %>%
  html_text() %>%
  str_extract("-?[0-9]+\\.[0-9]+")
  
# load packages
library(anytime)
library(tidyverse)
library(dplyr)
library(zoo)
library(TTR)

#read in csv files
coinbase <- read.csv("C:/Users/georg/Documents/Pytorch_btc/coinbaseUSD_1-min_data_2014-12-01_to_2019-01-09.csv")

# View first rows
head(coinbase)

# Change unix to utc time
coinbase_utc <- coinbase %>%
  mutate(
    Timestamp = anytime(Timestamp)
  )

#omit na values
c_nona <- na.omit(coinbase_utc)

# aggregate days by seperate columns
c_low <- aggregate(c_nona[ ,4], FUN="min", by=list(as.Date(c_nona$Timestamp, "%Y-%m-%d")))
c_high <- aggregate(c_nona[ ,3], FUN="max", by=list(as.Date(c_nona$Timestamp, "%Y-%m-%d")))

c_open <- c_nona %>%
  group_by(as.Date(Timestamp)) %>%
  slice(1) %>%
  select(Open)

c_close <- c_nona %>%
  group_by(as.Date(Timestamp)) %>%
  slice(c(n())) %>%
  select(Close)

c_volumeb <- aggregate(c_nona[ ,6], FUN="sum", by=list(as.Date(c_nona$Timestamp, "%Y-%m-%d")))
c_volumed <- aggregate(c_nona[ ,7], FUN="sum", by=list(as.Date(c_nona$Timestamp, "%Y-%m-%d")))

c_wa <- aggregate(c_nona[ ,8], FUN="mean", by=list(as.Date(c_nona$Timestamp, "%Y-%m-%d")))

# rename columns
c_high <- rename(c_high, Timestamp = Group.1, High = x)
c_low <- rename(c_low, Timestamp = Group.1, Low = x)
c_open <- rename(c_open, Timestamp = `as.Date(Timestamp)`)
c_close <- rename(c_close, Timestamp = `as.Date(Timestamp)`)
c_volumeb <- rename(c_volumeb, Timestamp = Group.1, Volume_BTC = x)
c_volumed <- rename(c_volumed, Timestamp = Group.1, Volume_USD = x)
c_wa <- rename(c_wa, Timestamp = Group.1, Weighted_Price = x)

# join datasets
c_1 <- left_join(c_open, c_high, by = NULL)
c_2 <- left_join(c_1, c_low, by = NULL)
c_3 <- left_join(c_2, c_close, by = NULL)
c_4 <- left_join(c_3, c_volumeb, by = NULL)
c_5 <- left_join(c_4, c_volumed, by = NULL)
c_6 <- left_join(c_5, c_wa, by = NULL)

testf <- function(column) {
  index = 0
  v = c()
  while (index < (nrow(c_9) - 2)) {
    x = ifelse(column[index] < column[index + 1], 1, 0)
    v <- append(v, x)
    index = index + 1
  }
  return(v)
}

# Add conditional 1 or 0 for if tomorrows wp is higher than todays
c_7 <- c_6 %>%
  mutate(
    WP_Increase = testf(Weighted_Price)
  )


# add rolling averages
c_7$ma10 = rollmean(c_7$Weighted_Price,10,mean,align='right',fill=NA)
c_7$ma50 = rollmean(c_7$Weighted_Price,50,mean,align='right',fill=NA)
c_7$ma100 = rollmean(c_7$Weighted_Price,100,mean,align='right',fill=NA)

c_8 <- c_7 %>%
  mutate(
    Ma10_WP_Increase = ifelse(Weighted_Price>ma10, 1, 0),
    Ma50_WP_Increase = ifelse(Weighted_Price>ma50, 1, 0),
    Ma100_WP_Increase = ifelse(Weighted_Price>ma100, 1, 0)
  )

# add RSI
c_8$rsi = RSI(c_8$Weighted_Price)

c_9 <- c_8 %>%
  mutate(
    RSI_GreaterThan_50 = ifelse(rsi>50, 1, 0)
  )

c_10 <- na.omit(c_9)
c_11 <- c_10[-nrow(c_10),]

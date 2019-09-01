library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)

setwd("C:/Users/Neerajk4/Documents/R files/practice files/accenture/dailyhistoricalstockprices19702018")

dataset <- read.csv("historical_stock_prices.csv", header = TRUE, nrows = 30000)
title <- read.csv("historical_stocks.csv", header = TRUE)

## merging and cleaning data sets
total <- merge(dataset, title, by = "ticker", all.x = TRUE)
total$date <- as.Date(total$date)
total$sector <- as.character(total$sector)


apple <- filter(total, ticker == "AAPL")
ggplot(apple, aes(x=date, y=close, group = 1)) + geom_line() + scale_x_date(breaks = date_breaks("3 years"))

##scale_x_datetime(date_breaks = "2 day", labels = date_format("%b %d"))

industry <- select(title, industry) %>% group_by (industry) %>% summarize(count = n())

##pie chart
sector <- select(title, sector) %>% group_by (sector) %>% summarize(count = n())
pie(sector$count, labels = sector$sector, main="Pie Chart of Sectors")


## plotting average price by sector in a scatter plot
sector2 <- select(total, ticker, sector, date, close) %>% group_by (sector, date) %>% summarize(average = mean(close))
sector2$sector <- as.character(sector2$sector)

ggplot(data = sector2, aes(x=sector2$date, y=sector2$average, colour=sector2$sector)) + geom_line() + scale_x_date(breaks = date_breaks("6 years"))



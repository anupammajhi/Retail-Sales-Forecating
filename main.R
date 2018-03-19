


#____________________________________________________________________________
#                           Case Study - TIME SERIES
#____________________________________________________________________________




# Clear All Variables

rm(list=ls())




#Loading required libraries

library(lubridate)
library(cowplot)
library(forecast)
library(tseries)
library(graphics)

#Reading data in
store <- read.csv("Global Superstore.csv")

summary(store$Market)
summary(store$Segment)

length(levels(store$Market))

str(store$Order.Date)



#------------------------
#  DATA CLEANING
#------------------------


#Converting Order.Date to a date time object

store$Order.Date <- as.Date(store$Order.Date, format = '%d-%m-%Y')

store$Date <- floor_date(store$Order.Date, unit = 'month')

str(store$Date)


# Checking for NA values

sort(sapply(store, function(x) sum(is.na(x))), decreasing = T)

# There are missing Values in Postal Code. There are no missing values in our variables of interest, 'Sales', 'Quantity' and 'Profit'.
# Therefore, there is no need to treat the missing values


# Checking for Outliers

quantile(store$Sales, seq(0,1,0.01))

sales_out <- data.frame(store[which(store$Sales > 4000), ])

quantile(store$Quantity, seq(0,1,0.01))
quantile(store$Profit, seq(0,1,0.01))

profit_out <- data.frame(store[which(store$Profit > 2000),])


# We will Impute the Outliers in Sales and Profit with appropriate values

store_clean <- store
store_clean[which(store_clean$Sales > 4000), 'Sales'] <- 4000
quantile(store_clean$Sales, seq(0,1,0.01))


store_clean[which(store_clean$Profit > 1500), 'Profit'] <- 1500
quantile(store_clean$Profit, seq(0,1,0.02))



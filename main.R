


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



# Splitting for the 3 levels of Segment, for each of the 7 levels of Market


seg <- split(store_clean, interaction(store_clean$Market,store_clean$Segment))

list2env(seg, .GlobalEnv)   #shows the datasets present in the list in the Global Environment





# Aggregating Profit, Quantity, Sales, monthwise across the list

s <-lapply(seg, function(x) {aggregate(cbind(Profit,Quantity,Sales) ~ Date, data = x, sum)})

s

list2env(s, .GlobalEnv)


#Creating multiple time series objects

tslist <- ts(s)



#Finding the Coeeficient of Variation for the 21 buckets. We calculate CV on Profits to detrmine the most profitable zones

cv <- lapply(s, function(x) {100*sd(x$Profit)/mean(x$Profit)})

cv

profit <- lapply(s, function(x) sum(x$Profit))

avgprofit <-  lapply(s, function(x) mean(x$Profit))


top <- data.frame(cbind(cv, profit, avgprofit))


top <- top[order(-unlist(profit)),]



write.csv(APAC.Consumer, 'APAC.Consumer.csv')
write.csv(EU.Consumer, 'EU.Consumer.csv')


# From the CV calculations we can see that the two most consistent and most profitable buckets are, 
#   1. APAC.Consumer
#   2. EU.Consumer



#Quick View of Both segments
ts.plot(EU.Consumer, gpars = list(col = rainbow(4)), main = 'EU Consumer')
ts.plot(APAC.Consumer, gpars = list(col = rainbow(5)), main = 'APAC Consumer')


#____________________________________________________________________________-


#---------------------------------------------
#   Forecasting for Sales - APAC.Consumer
#---------------------------------------------



#Naming Convention 

# This is the naming convention that will be used to name the data frames

# APAC.Consumer Sales - apacs
# APAC.Consumer Quantity - apacq
# EU.Consumer Sales - eus
# EU.Consumer Quantity - euq


# So we need to create 8 models.
# We will start by by predicting Sales for APAC Consumer

apacs <- data.frame(cbind(as.numeric(1:nrow(APAC.Consumer)),APAC.Consumer$Sales))

colnames(apacs) <- c('Months', 'Sales')

apacs_total <- ts(apacs$Sales)
apacs_in <- apacs[1:42,]
apacs_out <- apacs[43:48,]

apacs_ts <- ts(apacs_in$Sales)
plot(apacs_ts)



#Smoothing the series - Moving Average Smoothing

w <- 1
apacs_smooth <- filter(apacs_ts, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- apacs_smooth[w+2] - apacs_smooth[w+1]
for (i in seq(w,1,-1)) {
    apacs_smooth[i] <- apacs_smooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(apacs_ts)
diff <- apacs_smooth[n-w] - apacs_smooth[n-w-1]
for (i in seq(n-w+1, n)) {

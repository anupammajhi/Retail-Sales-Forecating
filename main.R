


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
    apacs_smooth[i] <- apacs_smooth[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- apacs_in$Months

lines(apacs_smooth, col="red", lwd=2)


#Trying Holt Winters

plot(apacs_ts)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.1, 0.3,0.5,0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
    apacs_smoothhw <- HoltWinters(apacs_ts, alpha=alphas[i],
                                  beta=FALSE, gamma=FALSE)
    
    lines(fitted(apacs_smoothhw)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

plot(apacs_ts)
apacs_smoothhw <- HoltWinters(apacs_ts, alpha=0.5,
                            beta=FALSE, gamma=FALSE)

lines(fitted(apacs_smoothhw)[,1], col='red', lwd=2)


# Clearly Moving average does better smoothing as compared to Holt Winter.Hence, we will use Moving Average Smoothing


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

apacs_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(apacs_smooth)))
colnames(apacs_smoothdf) <- c('Months', 'Sales')

#Now, let's fit a  model with trend and seasonality to the data
#There appears to be little seasonality in the data. Trying various degree equations

lmfit <- lm(Sales ~  sin(0.5*Months) * poly(Months,1) +cos(0.5*Months)*poly(Months,1), data=apacs_smoothdf)
global_pred <- predict(lmfit, Months=timevals_in)
summary(global_pred)

plot(apacs_ts)
lines(timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- apacs_in$Sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

# Both tests confirm the series is Strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- apacs_out[-2]

# Local Component 

# We add the local modelled component(model from arimafit) back to the global predicted part to predict final forecast

armafit
f_local <-  predict(armafit, n.ahead = 6)
f_local[[2]]


global_pred_out <- predict(lmfit,timevals_out) 
global_pred_out1 <- global_pred_out+f_local[[1]]
fcast <- global_pred_out1


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,apacs_out[,2])[5]
MAPE_class_dec

#Mape Value - 26.73
#             -----


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out1))
plot(apacs_total, col = "black", main = "Forecast for Sales - APAC.Consumer", ylab = 'Sales', xlab = 'Months')
lines(class_dec_pred, col = "green")
rect(xleft = 42, xright= 48, ybottom = 10000, ytop = 85000, density = 10, col = 'grey')


# These are the forecasted Sales for the last six Months.




#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(apacs_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- apacs_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,apacs_out[,2])[5]
MAPE_auto_arima

#Mape Value - 27.78
#             -----


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(apacs_total, col = "black", main = "Forecast for Sales - APAC.Consumer", ylab = 'Sales', xlab = 'Months')
lines(auto_arima_pred, col = "red")
rect(xleft = 42, xright= 48, ybottom = 10000, ytop = 85000, density = 10, col = 'grey')




# The fit from Classical Decomposition looks better, so we will use that model for the final forecast.


# Forecasting for the next 6 Months

#Local Component

f_local <-  predict(armafit, n.ahead = 12)  

f_local$pred

f_fut <- f_local$pred[7:12]


# Global Component

future <- data.frame(Months = 49:54)

global <- predict(lmfit, future)


# Final Model = Local + Global

Forecast <- global +f_fut

final_forecast_apacs <- data.frame(cbind(Months = 49:54, Forecast))


# Visualising the Forecasted Sales

colnames(final_forecast_apacs)[2] <- 'Sales'
final <- rbind(apacs, final_forecast_apacs)
plot(final, type = 'l', main = 'Forecasted Sales for APAC Consumer')
rect(xleft = 49, xright= 54, ybottom = 10000, ytop = 85000, density = 10, col = 'red')






#___________________________________________________________________________
#
#--------------------------------------------------
# 2) Forecasting for APAC.Consumer - Quantity
#--------------------------------------------------


apacq <- data.frame(cbind(as.numeric(1:nrow(APAC.Consumer)),APAC.Consumer$Quantity))

colnames(apacq) <- c('Months', 'Quantity')

apacq_total <- ts(apacq$Quantity)
apacq_in <- apacq[1:42,]
apacq_out <- apacq[43:48,]

apacq_ts <- ts(apacq_in$Quantity)
plot(apacq_ts)



#Smoothing the series - Moving Average Smoothing

w <- 1
apacq_smooth <- filter(apacq_ts, 
                       filter=rep(1/(2*w+1),(2*w+1)), 
                       method='convolution', sides=2)

#Smoothing left end of the time series

diff <- apacq_smooth[w+2] - apacq_smooth[w+1]
for (i in seq(w,1,-1)) {
    apacq_smooth[i] <- apacq_smooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(apacq_ts)
diff <- apacq_smooth[n-w] - apacq_smooth[n-w-1]
for (i in seq(n-w+1, n)) {
    apacq_smooth[i] <- apacq_smooth[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- apacq_in$Months

lines(apacq_smooth, col="red", lwd=2)


#Trying Holt Winters

plot(apacq_ts)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.02, 0.1, 0.3,0.5,0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
    apacq_smoothhw <- HoltWinters(apacq_ts, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
    
    lines(fitted(apacq_smoothhw)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

plot(apacq_ts)
apacq_smoothhw <- HoltWinters(apacq_ts, alpha=0.5,
                            beta=FALSE, gamma=FALSE)

lines(fitted(apacq_smoothhw)[,1], col='red', lwd=2)


# Clearly Moving average does better smoothing as compared to Holt Winter. Hence, we will use Moving Average Smoothing



#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

apacq_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(apacq_smooth)))
colnames(apacq_smoothdf) <- c('Months', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#There appears to be little seasonality in the data. Trying various degree equations

lmfit <- lm(Quantity ~  sin(0.5*Months) * poly(Months,1) *cos(0.5*Months), data=apacq_smoothdf)
global_pred <- predict(lmfit, Months=timevals_in)
summary(global_pred)

plot(apacq_ts)
lines(timevals_in, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- apacq_in$Quantity-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

# Both tests confirm the series is Strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- apacq_out[-2]

# Local Component 

# We add the local modelled component(model from arimafit) back to the global predicted part to predict final forecast


armafit
f_local <-  predict(armafit, n.ahead = 6)
f_local[[2]]


global_pred_out <- predict(lmfit,timevals_out) 
global_pred_out1 <- global_pred_out+f_local[[1]]
fcast <- global_pred_out1


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,apacq_out[,2])[5]
MAPE_class_dec


#Mape Value - 29.98
#             -----


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out1))
plot(apacq_total, col = "black", main = "Forecast for Quantity - APAC.Consumer", ylab = 'Quantity', xlab = 'Months')
lines(class_dec_pred, col = "green")
rect(xleft = 42, xright= 48, ybottom = 100, ytop = 900, density = 10, col = 'grey')


# These are the forecasted quantities for the last six months.




#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(apacq_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- apacq_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,apacq_out[,2])[5]
MAPE_auto_arima

#Mape Value - 26.24
#             -----


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))


plot(apacq_total, col = "black", main = "Forecast for Quantity - APAC.Consumer", ylab = 'Quantity', xlab = 'Months')
lines(auto_arima_pred, col = "red")
rect(xleft = 42, xright= 48, ybottom = 100, ytop = 900, density = 10, col = 'grey')


# The Red Line in the grey rectangle predicts the Quantity for the months 49:54. 



# Although the MAPE values of Auto Arima are slightly better, the fit from Classical Decomposition looks better, so we will use that model for the final forecast.


# Forecasting for the next 6 Months

#Local Component

f_local <-  predict(armafit, n.ahead = 12)  
f_local$pred

f_fut <- f_local$pred[7:12]


# Global Component

future <- data.frame(Months = 49:54)

global <- predict(lmfit, future)

# Final Model = Local + Global

Forecast <- global +f_fut


final_forecast_apacq <- data.frame(cbind(Months = 49:54, Forecast))


# Visualising the Forecasted Quantity

colnames(final_forecast_apacq)[2] <- 'Quantity'
final <- rbind(apacq, final_forecast_apacq)
plot(final, type = 'l', main = 'Forecasted Quantity for APAC Consumer')
rect(xleft = 49, xright= 54, ybottom = 100, ytop = 950, density = 10, col = 'red')




#___________________________________________________________________________






#--------------------------------------------------
# 3) Forecasting for EU.Consumer - Quantity
#--------------------------------------------------


euq <- data.frame(cbind(as.numeric(1:nrow(EU.Consumer)),EU.Consumer$Quantity))

colnames(euq) <- c('Months', 'Quantity')

euq_total <- ts(euq$Quantity)
euq_in <- euq[1:42,]
euq_out <- euq[43:48,]

euq_ts <- ts(euq_in$Quantity)
plot(euq_ts)



#Smoothing the series - Moving Average Smoothing

w <- 0.6
euq_smooth <- filter(euq_ts, 
                       filter=rep(1/(2*w+1),(2*w+1)), 
                       method='convolution', sides=2)

#Smoothing left end of the time series

diff <- euq_smooth[w+2] - euq_smooth[w+1]
for (i in seq(w,1,1)) {
    euq_smooth[i] <- euq_smooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(euq_ts)
diff <- euq_smooth[n-w] - euq_smooth[n-w-1]
for (i in seq(n-w+1, n)) {
    euq_smooth[i] <- euq_smooth[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- euq_in$Months

lines(euq_smooth, col="yellow", lwd=2)


#Trying Holt Winters

plot(euq_ts)

cols <- c("red", "blue", "green", "black", "grey")
alphas <- c(0.02, 0.1, 0.3,0.5,0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
    euq_smoothhw <- HoltWinters(euq_ts, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
    
    lines(fitted(euq_smoothhw)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

plot(euq_ts)
euq_smoothhw <- HoltWinters(euq_ts, alpha=0.7,
                            beta=FALSE, gamma=FALSE)

lines(fitted(euq_smoothhw)[,1], col='green', lwd=2)

vals <- as.data.frame(fitted(euq_smoothhw))

euq_smoothhw_total  <- data.frame(c(euq_in[1,2], (vals[,2])))

#  MA does better smoothing as compared to HW, so we will use MA smoothing.


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

euq_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(euq_smooth)))
colnames(euq_smoothdf) <- c('Months', 'Quantity')

#Now, let's fit a  model with trend and seasonality to the data
#There appears to be little seasonality in the data. Trying various degree equations

lmfit <- lm(Quantity ~  (sin(0.5*Months) * poly(Months,2) + cos(0.5*Months) * poly(Months,2))
            *exp(0.00005*Months),data=euq_smoothdf)


global_pred <- predict(lmfit, Months=timevals_in)
summary(global_pred)

plot(euq_ts)
lines(timevals_in, global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- euq_in$Quantity-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)
qqnorm(resi) 


# We see that the KPSS test Fails. Therefore, we use another test, the qq plot, and we can see that the plot is along the 45 degree line

# Two tests confirm the series is Strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- euq_out[-2]

# Local Component 

# We add the local modelled component(model from arimafit) back to the global predicted part to predict final forecast.
armafit
f_local <-  predict(armafit, n.ahead = 6)
f_local[[2]]

f_local

global_pred_out <- predict(lmfit,timevals_out) 
global_pred_out1 <- global_pred_out+f_local[[1]]
fcast <- global_pred_out1


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,euq_out[,2])[5]
MAPE_class_dec

#Mape Value - 31.07
#             -----


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out1))
plot(euq_total, col = "black", main = "Forecast for Quantity - EU.Consumer", ylab = 'Quantity', xlab = 'Months')
lines(class_dec_pred, col = "green")
rect(xleft = 42, xright= 48, ybottom = 100, ytop = 950, density = 10, col = 'grey')





#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(euq_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- euq_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,euq_out[,2])[5]
MAPE_auto_arima

#Mape Value - 30.13
#             -----


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))


plot(euq_total, col = "black", main = "Forecast for Quantity - EU.Consumer", ylab = 'Quantity', xlab = 'Months')
lines(auto_arima_pred, col = "red")
rect(xleft = 42, xright= 48, ybottom = 100, ytop = 950, density = 10, col = 'grey')


# The Red Line in the grey rectangle predicts the Quantity for the last six months. 


# The fit from Classical Decomposition looks better, so we will use the results of classical decomposition for the final forecast.


# Forecasting for the next 6 Months

#Local Component

f_local <-  predict(armafit, n.ahead = 12)  

f_local$pred

f_fut <- f_local$pred[7:12]


# Global Component

future <- data.frame(Months = 49:54)

global <- predict(lmfit, future)

# Final Model = Local + Global

Forecast <- global +f_fut


final_forecast_euq <- data.frame(cbind(Months = 49:54, Forecast))


# Visualising the Forecasted Quantity

colnames(final_forecast_euq)[2] <- 'Quantity'
final <- rbind(euq, final_forecast_euq)
plot(final, type = 'l', main = 'Forecasted Quantity for EU Consumer')
rect(xleft = 49, xright= 54, ybottom = 100, ytop = 950, density = 10, col = 'red')




#______________________________________________________________________________




#--------------------------------------------------
# 4) Forecasting for EU.Consumer - Sales
#--------------------------------------------------


eus <- data.frame(cbind(as.numeric(1:nrow(EU.Consumer)),EU.Consumer$Sales))

colnames(eus) <- c('Months', 'Sales')

eus_total <- ts(eus$Sales)
eus_in <- eus[1:42,]
eus_out <- eus[43:48,]

eus_ts <- ts(eus_in$Sales)
plot(eus_ts)



#Smoothing the series - Moving Average Smoothing

w <- 0.7
eus_smooth <- filter(eus_ts, 
                     filter=rep(1/(2*w+1),(2*w+1)), 
                     method='convolution', sides=2)

#Smoothing left end of the time series

diff <- eus_smooth[w+2] - eus_smooth[w+1]
for (i in seq(w,1,1)) {
    eus_smooth[i] <- eus_smooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(eus_ts)
diff <- eus_smooth[n-w] - eus_smooth[n-w-1]
for (i in seq(n-w+1, n)) {
    eus_smooth[i] <- eus_smooth[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- eus_in$Months

lines(eus_smooth, col="yellow", lwd=2)


#Trying Holt Winters

plot(eus_ts)

cols <- c("red", "blue", "green", "black", "grey")
alphas <- c(0.02, 0.1, 0.3,0.5,0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
    eus_smoothhw <- HoltWinters(eus_ts, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
    
    lines(fitted(eus_smoothhw)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

plot(eus_ts)
eus_smoothhw <- HoltWinters(eus_ts, alpha=0.6,
                            beta=FALSE, gamma=FALSE)

lines(fitted(eus_smoothhw)[,1], col='red', lwd=2)


# Again, Moving average does better smoothing as compared to Holt Winter, so we will use Moving Average smoothing.


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

eus_smoothdf <- as.data.frame(cbind(timevals_in, as.vector(eus_smooth)))
colnames(eus_smoothdf) <- c('Months', 'Sales')

#Now, let's fit a  model with trend and seasonality to the data
#There appears to be little seasonality in the data. Trying various degree equations

lmfit <- lm(Sales ~  sin(0.5*Months) * poly(Months,3) + cos(0.5*Months) * poly(Months,2)
                       + sin(0.5*Months)*exp(0.0008*Months) + cos(0.5*Months)*exp(0.0008*Months),data=eus_smoothdf)

global_pred <- predict(lmfit, Months=timevals_in)
summary(global_pred)

plot(eus_ts)
lines(timevals_in, global_pred, col='green', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- eus_in$Sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

qqnorm(resi)

# We see that the KPSS test Fails. Therefore, we use another test, the qq plot, and we can see that the plot is along the 45 degree line

# Two tests confirm the series is Strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- eus_out[-2]

# Local Component 

# We add the local modelled component(model from arimafit) back to the global predicted part to predict final forecast

armafit
f_local <-  predict(armafit, n.ahead = 6)
f_local[[2]]


global_pred_out <- predict(lmfit,timevals_out) 
global_pred_out1 <- global_pred_out+f_local[[1]]
fcast <- global_pred_out1


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,eus_out[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out1))
plot(eus_total, col = "black", main = "Forecast for Sales - EU.Consumer", ylab = 'Sales', xlab = 'Months')
lines(class_dec_pred, col = "green")
rect(xleft = 42, xright= 48, ybottom = 8000, ytop = 70000, density = 10, col = 'grey')





#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(eus_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- eus_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,eus_out[,2])[5]
MAPE_auto_arima

#Mape Value - 28.50
#             -----



#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))


plot(eus_total, col = "black", main = "Forecast for Sales - EU.Consumer", ylab = 'Sales', xlab = 'Months')
lines(auto_arima_pred, col = "red")
rect(xleft = 42, xright= 48, ybottom = 8000, ytop = 70000, density = 10, col = 'grey')


# The Red Line in the grey rectangle predicts the Sales for the last six months for the EU Consumer region. 



# The fit from Classical Decomposition looks better, so we will use the results of classical decomposition for the final forecast.


#Forecasting for the next 6 Months

#Local Component

f_local <-  predict(armafit, n.ahead = 12)  

f_local$pred

f_fut <- f_local$pred[7:12]


# Global Component

future <- data.frame(Months = 49:54)

global <- predict(lmfit, future)

# Final Model = Local + Global

Forecast <- global +f_fut


final_forecast_eus <- data.frame(cbind(Months = 49:54, Forecast))


# Visualising the Forecasted Sales

colnames(final_forecast_eus)[2] <- 'Sales'
final <- rbind(eus, final_forecast_eus)
plot(final, type = 'l', main = 'Forecasted Sales for EU Consumer')
rect(xleft = 49, xright= 54, ybottom = 8000, ytop = 75000, density = 10, col = 'red')



write.csv(final_forecast_apacs, 'final_forecast_apacs.csv')

write.csv(final_forecast_apacq, 'final_forecast_apacq.csv')

write.csv(final_forecast_eus, 'final_forecast_eus.csv')

write.csv(final_forecast_euq, 'final_forecast_euq.csv')







#___________________________________________________________________________
#                               END
#___________________________________________________________________________


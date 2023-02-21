# Objective: To predict the airline ticket' sales of 1961 using Time Series Analysis.

# Data Description: 10 year air-ticket sales data of airline industry from 1949-1960

# Install and load parkages
#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)

#### Exploratory Data Analysis
data("AirPassengers")
class(AirPassengers)

start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)

sum(is.na(AirPassengers))

summary(AirPassengers)

AirPassengers

### INSIGHTS
# 1. Trend: The passenger numbers increase over time indicating an increasing linear trend.
# 2. Seasonality: In the boxplot, there are more passengers travelling in months 6 to 9, indicating seasonality with an apparent cycle of 12 months.

## Decomposing Time Series into its components
tsdata<-ts(AirPassengers, frequency = 12)
ddata <- decompose(tsdata, "multiplicative")

plot(ddata)

## trend component
plot(ddata$trend)

## seasonality component
plot(ddata$seasonal)

## Irregularity component
plot(ddata$random)

### Plotting components together
plot(AirPassengers)
abline(reg = lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)

#### Boxplot by cycle
boxplot(AirPassengers~cycle(AirPassengers, xlab="Date"))

## Test for Stationarity
plot(AirPassengers)

#### Find the best ARIMA Model
mymodel <- auto.arima(AirPassengers)
mymodel

#### Run best model with `trace` to compare the information criteria
auto.arima(AirPassengers, ic="aic", trace=TRUE)


#adf.test(mymodel)

## Checking residuals which shows stationarity
plot.ts(mymodel$residuals)

#### Plot Autocorrelation Function (ACF) and Partial ACF
acf(ts(mymodel$residuals), main="ACF Residual")
pacf(ts(mymodel$residuals), main="PACF Residual")

## Forecast for the next 10 years
myforecast<-forecast(mymodel, level=c(95), h=10*12)
plot(myforecast)

## Validating the findings of the ARIMA model by running Ljung-Box test
Box.test(mymodel$resid, lag=5, type="Ljung-Box")
Box.test(mymodel$resid, lag=10, type="Ljung-Box")
Box.test(mymodel$resid, lag=15, type="Ljung-Box")

# Result: The p-values are quite insignificant, indicating that the model is free of autocorrelation.

# Conclusion
## From the ARIMA output, the model using (2,1,1) has been shown to adequately fit the data. 



library(hts)
data <- htseg2

# Split data into training and test sets
hts_train <- window(data, end=2004)
hts_test <- window(data, start=2005)

# Fit models and compute forecasts on all nodes using training data
train <- aggts(hts_train)
fmodels <- list()
fc <- matrix(0, ncol=ncol(train), nrow=3)
for(i in 1:ncol(train))
{
  fmodels[[i]] <- auto.arima(train[,i])
  fc[,i] <- forecast(fmodels[[i]],h=3)$mean
}
forecast <- combinef(fc, nodes=data$nodes)
accuracy <- accuracy.gts(forecast, hts_test)

# Forecast on full data set without re-estimating parameters
full <- aggts(data)
fcfull <- matrix(0, ncol=ncol(full), nrow=15)
for(i in 1:ncol(full))
{
  fcfull[,i] <- forecast(Arima(full[,i], model=fmodels[[i]]),
                         h=15)$mean
}
forecast.full <- combinef(fcfull, nodes=data$nodes)
accuracy <- accuracy.gts(forecast.full, hts_test)
# Forecast on full data set with same models but re-estimated parameters
full <- aggts(data)
fcfull <- matrix(0, ncol=ncol(full), nrow=15)
for(i in 1:ncol(full))
{
  fcfull[,i] <- forecast(Arima(full[,i], 
                               order=fmodels[[i]]$arma[c(1,6,2)],
                               seasonal=fmodels[[i]]$arma[c(3,7,4)]),
                         h=15)$mean
}
forecast.full <- combinef(fcfull, nodes=data$nodes)
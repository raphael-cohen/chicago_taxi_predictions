library(data.table)
library(ggmap)
library(xts)
library(forecast)
library(tseries)



######################################################
########Using last 7 days vs last 7 Mondays###########
######################################################


df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016/1.csv')

pickup_data <- df[, .(Pickup.Centroid.Latitude, Pickup.Centroid.Longitude)]
nrow(pickup_data[is.na(Pickup.Centroid.Latitude)])
nrow(pickup_data[is.na(Pickup.Centroid.Longitude)])
pickup_data <- pickup_data[!is.na(Pickup.Centroid.Latitude),]

kfit <- kmeans(x = pickup_data, centers = 70)

pickup_data$cluster <- kfit$cluster
head(pickup_data)

load("Chicago_map.Rdata")
ggmap(chicago, extent = "panel", maprange=FALSE) + 
  geom_point(data = as.data.table(kfit$centers), aes(x = as.data.table(kfit$centers)$Pickup.Centroid.Longitude, y = as.data.table(kfit$centers)$Pickup.Centroid.Latitude))


forecastTimeSeries <- function(agg){
  myts <- ts(agg$N, frequency = 96)
  #Auto forecast
  ARIMAfit <- auto.arima(myts, d=1, approximation=FALSE,trace=TRUE)
  d.forecast <- forecast(ARIMAfit, level = c(90), h = 96)
  return(d.forecast)
}


##Forecast on last 7 Mondays

#Aggregate trips for last 7 Mondays

df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016/1.csv')
df <- df[!is.na(Pickup.Centroid.Latitude),]
df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
df <- df[Trip.DayText == "Monday"]
agg <- df[,.N,by= Trip.Start.Time]
agg <- agg[order(Trip.Start.Time)]

#Forecasting for 8th Monday of 2016, so taking first 7 Mondays' Data
for(wk in 2:7){
  filename <- paste(wk,'csv', sep = '.')
  filename <- paste(data_dir, filename, sep = "/")
  df <- fread(filename)
  df <- df[!is.na(Pickup.Centroid.Latitude),]
  df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
  df <- df[Trip.DayText == "Monday"]
  temp <- df[,.N,by= Trip.Start.Time]
  temp <- temp[order(Trip.Start.Time)]
  agg <- rbind(agg, temp)
}

forecast_7_mondays <- forecastTimeSeries(agg)

plt <- autoplot(forecast_7_mondays)
png(filename="last_7_mondays.png", width = 2000, height = 1000)
print(plt)
dev.off() 

plotForecastErrors <- function(forecasterrors) {
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd 
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed ˓→data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(forecast_7_mondays$residuals)


##Forecast based on last 7 days
#Forecasting for 8th Monday of 2016, so taking 7th week Data

df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016/7.csv')
df <- df[!is.na(Pickup.Centroid.Latitude),]
df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
agg <- df[,.N,by= c("Trip.Start.Time", "Trip.DayNo")]
agg <- agg[order(Trip.DayNo,Trip.Start.Time)]
agg

forecast_7_day <- forecastTimeSeries(agg)

plt <- autoplot(forecast_7_day)
plt
png(filename="last_7_days.png", width = 2000, height = 1000)
print(plt)
dev.off() 

d.summary <- summary(forecast_7_day)

summary(forecast_7_day)
forecast_7_day$model$aic
forecast_7_day$model$sigma2
plot(forecast_7_day$residuals)

#Actual demands on the 8th Monday:

df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016/8.csv')
df <- df[!is.na(Pickup.Centroid.Latitude),]
df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
df <- df[Trip.DayText == "Monday"]
agg <- df[,.N,by= Trip.Start.Time]
agg <- agg[order(Trip.Start.Time)]
agg


myts <- ts(agg$N, frequency = 96)

ts.plot(myts, 
        ts(predict(forecast_7_day)$mean, frequency = 96), 
        ts(predict(forecast_7_mondays)$mean, frequency = 96),  
        gpars = list(col = c("black", "red", "blue")))
legend("topleft", legend = c("Actual", "7 Days", "7 Mondays"), col = c("black", "red", "blue"), lty = 1)
title(main = "Comparing forecasts")



######################################################
######Finding optimal no of Mondays to consider#######
######################################################




#Predicting for 15th Monday
errors <- c()
base_dir <- '/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017'
n <- 14

predictedts <- c()

#for j in 1:12
for(j in 1:12){
  f <- n -j
  print(f)
  #Initialize aggregator with details of 14th Monday:
  
  df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017/14.csv')
  df <- df[!is.na(Pickup.Centroid.Latitude),]
  df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
  df <- df[Trip.DayText == "Monday"]
  agg <- df[,.N,by= Trip.Start.Time]
  agg <- agg[order(Trip.Start.Time)]
  agg$DayNo <- mean(df$Trip.DayNo)
  agg$Month <- mean(df$Trip.Month)
  agg[, TripDate := paste("2017",Month, sep = "-")]
  agg[, TripDate := paste(TripDate,DayNo, sep = "-")]
  agg[,c("DayNo", "Month") := NULL]
  
  #Iterate and get details of last j Mondays
  for(i in f:n-1){
    filename <- paste(base_dir, paste(i,"csv", sep = "."), sep = "/")
    df <- fread(filename)
    df <- df[!is.na(Pickup.Centroid.Latitude),]
    df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
    df <- df[Trip.DayText == "Monday"]
    temp <- df[,.N,by= Trip.Start.Time]
    temp <- temp[order(Trip.Start.Time)]
    temp$DayNo <- mean(df$Trip.DayNo)
    temp$Month <- mean(df$Trip.Month)
    temp[, TripDate := paste("2017",Month, sep = "-")]
    temp[, TripDate := paste(TripDate,DayNo, sep = "-")]
    temp[,c("DayNo", "Month") := NULL]
    agg <- rbind(agg, temp)
  }
  agg <- agg[order(TripDate, Trip.Start.Time)]
  print(agg)
  myts <- ts(agg$N, frequency = 96)
  ARIMAfit <- auto.arima(myts, d=1, approximation=FALSE,trace=TRUE)
  errors <- rbind(errors,summary(ARIMAfit))
  arima_forecast <- forecast(ARIMAfit, level = c(90), h = 96)
  pred <- ts(predict(arima_forecast)$mean, frequency = 96)
  predictedts <- rbind(predictedts, pred)
}

ts(predictedts[2,], frequency = 96)

plot(y = errors[,2], x = seq(3:14), type = 'p')
title(main = "Train MSE")

ts.plot(ts(predictedts[1,], frequency = 96),
        ts(predictedts[2,], frequency = 96),
        ts(predictedts[3,], frequency = 96),
        ts(predictedts[3,], frequency = 96),
        ts(predictedts[4,], frequency = 96),
        ts(predictedts[5,], frequency = 96),
        ts(predictedts[6,], frequency = 96),
        ts(predictedts[7,], frequency = 96),
        ts(predictedts[8,], frequency = 96),
        ts(predictedts[9,], frequency = 96),
        ts(predictedts[10,], frequency = 96),
        ts(predictedts[11,], frequency = 96),
        ts(predictedts[12,], frequency = 96),
        gpars = list(col = palette(rainbow(12))))

#Actual demands on the 15th Monday:

df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016/15.csv')
df <- df[!is.na(Pickup.Centroid.Latitude),]
df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
df <- df[Trip.DayText == "Monday"]
nrow(df)
act <- df[,.N,by= Trip.Start.Time]
nrow(act)
act <- act[order(Trip.Start.Time)]
actts <- ts(act$N, frequency = 96)

#Plot train and test MSEs:


testerrors <- c()
for(i in 1:12){
  testerrors <- cbind(testerrors, sqrt(mean((act$N - predictedts[i,])^2)))
}

plot(y = testerrors, x = seq(3,14), type = "l", ylab = "Test MSE", xlab = "No. of past Mondays")
title(main = "Test MSE Comparison")
plot(y = errors[,2], x = seq(3,14), type = "l", ylab = "Train MSE", xlab = "No. of past Mondays")
title(main = "Train MSE Comparison")
save(predictedts,file = "predictedts.Rdata")
save(errors, file = "errors.Rdata")

load("errors.Rdata")
load("predictedts.Rdata")


#Plot with actual data:

ts.plot(actts, 
        ts(predictedts[1,], frequency = 96), 
        ts(predictedts[12,], frequency = 96), 
        gpars = list(col = c("black", "red", "blue")))
legend("topleft", legend = c("Actual", "Last 3 Mondays", "Last 14 Mondays"), col = c("black", "red", "blue"), lty = 1)
title(main = "Comparing forecasts")


######################################################
########Using last 3 days vs last 3 Mondays###########
######################################################

##Forecast based on last 3 days
#Forecasting for 15th Monday of 2016, so taking 14th week Data

df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016/14.csv')
df <- df[!is.na(Pickup.Centroid.Latitude),]
df[, Trip.Start.Time := as.factor(Trip.Start.Time)]
df <- df[Trip.DayNo > 4,]
summary(df)
agg <- df[,.N,by= c("Trip.Start.Time", "Trip.DayNo")]
agg <- agg[order(Trip.DayNo,Trip.Start.Time)]
agg

myts <- ts(agg$N, frequency = 96)
ARIMAfit <- auto.arima(myts, d=1, D = 1, approximation=FALSE,trace=TRUE)

summary(ARIMAfit)
arima_forecast <- forecast(ARIMAfit, level = c(90), h = 96)
summary(ARIMAfit)
pred_3day <- ts(predict(arima_forecast)$mean, frequency = 96)

#Comparing the predictions with actual demand and demand predicted by taking last 3 Mondays
load("Predictedts.Rdata")
ts.plot(actts, 
        ts(predictedts[1,], frequency = 96), 
        ts(pred_3day, frequency = 96), 
        gpars = list(col = c("black", "red", "blue")))
legend("topleft", legend = c("Actual", "Last 3 Mondays", "Last  Days"), col = c("black", "red", "blue"), lty = 1)
title(main = "Comparing forecasts")





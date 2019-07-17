library(data.table)
library(ggmap)
library(xts)
library(forecast)
library(tseries)
library(dplyr)

#Read data for last 3 Mondays

df <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017/14.csv')
df <- df[!is.na(Pickup.Centroid.Latitude),]
df <- df[Trip.DayText == "Monday"]
temp <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017/13.csv')
temp <- temp[!is.na(Pickup.Centroid.Latitude),]
temp <- temp[Trip.DayText == "Monday"]
df <- rbind(temp, df)
temp <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017/12.csv')
temp <- temp[!is.na(Pickup.Centroid.Latitude),]
temp <- temp[Trip.DayText == "Monday"]
df <- rbind(temp, df)


downtown <- df[Pickup.Centroid.Latitude >= 41.85 & Pickup.Centroid.Longitude >= -87.66 & Pickup.Centroid.Latitude <= 41.9,]


#Overall time-series, not including region data. 

agg_downtown <- downtown[,.N,by= c("Trip.Month", "Trip.DayNo", "Trip.Start.Time")]
agg_downtown <- agg_downtown[order(Trip.Month,Trip.DayNo,Trip.Start.Time)]
agg_downtown
myts <- ts(agg_downtown$N, frequency = 96)
ARIMAfit <- auto.arima(myts, d=1, approximation=FALSE,trace=TRUE)
arima_forecast <- forecast(ARIMAfit, level = c(90), h = 96)
autoplot(arima_forecast, ylab = "Demand", main = "Predicting overall demand in Downtown Chicago")
title(main = "Predicting overall demand in Downtown Chicago")

#Prepare regionwise split
agg_regionwise <- downtown[,.(count=.N, lat = mean(Pickup.Centroid.Latitude), lon = mean(Pickup.Centroid.Longitude)), 
                           by = c("Trip.Month", "Trip.DayNo", "Trip.Start.Time","Pickup.Census.Tract")]
agg_regionwise <- agg_regionwise[order(Pickup.Census.Tract, Trip.Month,Trip.DayNo,Trip.Start.Time)]
agg_regionwise[,Pickup.Census.Tract := as.factor(Pickup.Census.Tract)]


agg_regionwise_complete <- as.data.table(data.frame(Trip.Month = c(),
                                                    Trip.DayNo = c(),
                                                    Trip.Start.Time = c(),
                                                    Pickup.Census.Tract = c(),
                                                    count = c(),
                                                    lat = c(),
                                                    lon = c()))
predictions <- c()
errors <- c()
region_predicted <- data.frame(Census.Tract <- c(),
                               lat <- c(),
                               lon <- c())
for(region in levels(agg_regionwise$Pickup.Census.Tract)){
  print(region)
  temp <- left_join(agg_downtown, agg_regionwise[Pickup.Census.Tract == region,], by = c("Trip.Month", "Trip.DayNo", "Trip.Start.Time"))
  temp <- as.data.table(temp)
  temp[,N := NULL]
  temp[is.na(Pickup.Census.Tract), Pickup.Census.Tract := region]
  temp[is.na(count), count := 0]
  temp[is.na(lat), lat := temp[!is.na(lat), lat]]
  temp[is.na(lon), lon := temp[!is.na(lon), lon]]
  agg_regionwise_complete <- rbind(agg_regionwise_complete, temp)
  myts <- ts(temp$count, frequency = 96)
  ARIMAfit <- auto.arima(myts, d=1, D=1, approximation=FALSE,trace=TRUE)
  errors <- rbind(errors,summary(ARIMAfit))
  arima_forecast <- forecast(ARIMAfit, level = c(90), h = 96)
  pred <- ts(predict(arima_forecast)$mean, frequency = 96)
  predictions <- rbind(predictions, pred)
  region_predicted <- rbind(region_predicted, data.table(Census.Tract = region,
                                                         lat = mean(temp$lat),
                                                         lon = mean(temp$lon)))
}
save(region_predicted,file = "region_predicted.Rdata")
save(predictions,file = "predictions.Rdata")





#Test data.
test <- fread('/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017/15.csv')
test <- test[!is.na(Pickup.Centroid.Latitude),]
test <- test[Trip.DayText == "Monday"]
test_downtown <- test[Pickup.Centroid.Latitude >= 41.85 & Pickup.Centroid.Longitude >= -87.66 & Pickup.Centroid.Latitude <= 41.9,]
test_agg_downtown <- test_downtown[,.N,by= c("Trip.Month", "Trip.DayNo", "Trip.Start.Time")]
test_agg_downtown <- test_agg_downtown[order(Trip.Month,Trip.DayNo,Trip.Start.Time)]
test_agg_downtown

#Test regionwise split
test_agg_regionwise <- test_downtown[,.(count=.N, lat = mean(Pickup.Centroid.Latitude), lon = mean(Pickup.Centroid.Longitude)), 
                                     by = c("Trip.Month", "Trip.DayNo", "Trip.Start.Time","Pickup.Census.Tract")]
test_agg_regionwise <- test_agg_regionwise[order(Pickup.Census.Tract, Trip.Month,Trip.DayNo,Trip.Start.Time)]
test_agg_regionwise[,Pickup.Census.Tract := as.factor(Pickup.Census.Tract)]
nlevels(test_agg_regionwise$Pickup.Census.Tract)
test_agg_regionwise <- as.data.table(test_agg_regionwise)

#Testing on a Census Tract

t <- test_agg_regionwise[Pickup.Census.Tract == 0,]
summary(t$count)
#Just to get all the timeperiod rows
length(agg_regionwise_complete[Pickup.Census.Tract == 0 & Trip.DayNo == 20 & Trip.Month == 3, Trip.Start.Time])
timeperiods <- agg_regionwise_complete[Pickup.Census.Tract == 0 & Trip.DayNo == 20 & Trip.Month == 3, Trip.Start.Time]
timeperiods
demands <- data.frame(actual = rep(0,96),
                      predicted = rep(0,96),
                      timeperiod = timeperiods[1:96])
demands <- as.data.table(demands)

#For the actual values, if the time period doesn't exist in the data table, it's count val. is set to 0
for(i in 1:96){
  if(length(t[t$Trip.Start.Time == demands[i, timeperiod], count] > 0)){
    demands[i, actual := t[t$Trip.Start.Time == demands[i, timeperiod], count]]
  } else{
    demands[i, actual := 0]
  }
  demands[i, predicted := predictions[2,i]]
}

#Plot the demands

ts.plot(ts(demands$actual, frequency = 96), 
        ts(demands$predicted, frequency = 96),
        gpars = list(col = c("black", "red")))
legend("topleft", legend = c("Actual", "Predicted"), col = c("black", "red"), lty = 1)
title(main = "Comparing Forecasts For Census Tract '8.41447213294678e-314'")


#install this for api key
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
#library(ggmap)
# ggmap(get_googlemap())
# register_google(key = "AIzaSyBoJ1aI3DsDP8BiAH1pw33Kb17pfW0fIRo")
# 
# lat <- 41.88466
# lon <- -87.63301
# bwmap <- get_map(location = c(lon = lon, lat =lat), color = "bw",
#                          zoom = 14, maptype = "roadmap", source = "google", api_key = "AIzaSyA1OIjkDvA9DqP8mSOwbsDuYlv-7EosZYY")#"watercolor")#, scale = 1)
# save(bwmap, file = "BWDowntown.Rdata")
# ggmap(bwmap)

#Save the demand predictions for each time period.
library(ggmap)
load("region_predicted.Rdata")
load("predictions.Rdata")
load("Downtown.Rdata")
for(i in 1:96){
  demand <- predictions[,i]
  #Just in case we have negative predictions
  demand[demand < 0] = 0
  demand_map <- ggmap(bwmap, extent = "panel", maprange=FALSE) +
    geom_point(data = region_predicted, aes(x = lon, y = lat,  col = demand, size = demand)) +
    scale_colour_gradient(low = "yellow", high = "red", limits = c(0,207)) +
    scale_size_continuous(guide = FALSE)
  filename <- paste(getwd(), "/5_profit_maximiser/Results/BWPreds/",toString(i), ".png", sep = "")
  ggsave(filename = filename, plot = demand_map)
}




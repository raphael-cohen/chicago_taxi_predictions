require("RPostgreSQL")
library(data.table)
library(ggplot2)
library(zoo)
library(xts)
library(forecast)
library(tseries)
library(lubridate)
library(anytime)

pw <- {
  "azery"
}

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "taxi_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)

dbExistsTable(con, "taxi_trips")

payment_type <- dbGetQuery(con, "SELECT COUNT(taxi_id), payment_type FROM taxi_trips GROUP BY payment_type;")


tbm <- dbGetQuery(con, "SELECT COUNT(taxi_id), Extract(MONTH FROM start_time) AS month,
                  Extract(YEAR FROM start_time) AS year FROM taxi_trips GROUP BY year, month")


tbm <- setDT(tbm)
tbm <- tbm[year!=2017 | month <= 1,]

tbm[, date := paste(year,month, sep = "-")]

ggplot(data = tbm, aes(date, count))+
# ggplot(data = tbm, aes(parse_date_time(paste(year, month, sep = "-"), "%Y-%m"), count))+
# ggplot(data = tbm, aes(anydate(paste(year, month, sep = "-"), "%Y-%m"), count))+
  geom_line(color = tbm$year,size = 1, group = 1)+
  geom_line(aes(y=rollapplyr(count, 2, sd, fill = 0)), size = 0.5, group = 1)+
  geom_line(aes(y=rollmean(count, 2, na.pad=TRUE)), size = 0.5, group = 1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Create time serie
myts <- xts(tbm$count, order.by=as.yearmon(tbm$date, "%Y-%m"))

#plotting time serie decomposition
plot(stl(myts, s.window = 12, t.window = 12))

#Autocorrelation And Cross-Correlation Function Estimation
acf(myts, na.action = na.omit)

#Same on the diff serie
acf(diff(myts), na.action = na.omit)

#Tests to check if the modified ts is stationnary

myts_diff <- na.remove(diff(diff(myts)))

adf.test(myts_diff, alternative = "stationary", k =12)
kpss.test(x = myts_diff)


#Auto forecast
ARIMAfit <- auto.arima(myts, d=1, approximation=FALSE,trace=TRUE)
d.forecast <- forecast(ARIMAfit, level = c(90), h = 10)

autoplot(d.forecast)
# 
# xts::plot.xts(d.forecast)#myts)
# 
# plot.zoo(myts, main = "The price of AAPL", xaxt="n", xlab="")
# axis.Date(1,at=pretty(index(d.forecast)),
#           labels=format(pretty(index(d.forecast)),format="%Y-%b"),
#           las=2, cex.axis=0.7)
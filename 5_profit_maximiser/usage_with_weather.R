require("RPostgreSQL")
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(RColorBrewer)


pw <- {
  "azery"
}

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "taxi_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)

dbExistsTable(con, "taxi_trips")

dates <- dbGetQuery(con, "SELECT COUNT(taxi_id), EXTRACT(YEAR FROM start_time) as year, EXTRACT(MONTH FROM start_time) as month,
                     EXTRACT(DAY FROM start_time) as day
                     FROM taxi_trips
                     GROUP BY EXTRACT(YEAR FROM start_time), EXTRACT(MONTH FROM start_time),
                     EXTRACT(DAY FROM start_time);")

dates <- setDT(dates)
dates[, date := parse_date_time(paste(year,month,day, sep = "-"), "%Y-%m-%d")]

# dates <- fread("/media/Onedrive/IIT/Courses/CSP571 Data Prep and Analysis/Project/weather/dates_and_count.csv")


#get day of week
dates[, dayow := as.POSIXlt(dates$date)$wday]

fwrite(dates, file = "dates_and_count.csv")

#count by week day
count.bywd <- dates[year < 2017] %>% 
  group_by(dayow, year) %>%
  summarise(avg = mean(count), n = n()) %>%
  arrange(year)


c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[as.POSIXlt(dates$date)$wday + 1]

barplot(height = count.bywd$avg, 
        names.arg = paste(count.bywd$year,
                          c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[count.bywd$dayow + 1], sep = "-"),
        las=2, ylab = "average taxi trips", xlab = "day of week", 
        main = "Average number of taxi trips per day of week for each year",
        col = rep(brewer.pal(7, "Set1"), 1), cex.names=1.1)


weather_file <- "/media/Onedrive/IIT/Courses/CSP571 Data Prep and Analysis/Project/weather/Weather.csv"
weather <- fread(weather_file)
names(weather) <- sapply(names(weather), tolower)
weather$date <- parse_date_time(weather$date, "%Y-%m-%d")

merged <- setDT(merge(x = dates, y = weather, by = "date", all.x = TRUE))

# fwrite(merged, file = "trip_count_per_date_weather.csv")
# merged <- fread("/media/Onedrive/IIT/Courses/CSP571 Data Prep and Analysis/Project/weather/trip_count_per_date_weather.csv")

cni <- merged[name =="CHICAGO NORTHERLY ISLAND, IL US"]
c47ne <- merged[name =="CHICAGO 4.7 NE, IL US"]
airport <- merged[name =="CHICAGO OHARE INTERNATIONAL AIRPORT, IL US"]


count.tmax <- cni %>% 
  group_by(tmax) %>%
  summarise(count = mean(count), f = n())

barplot(height = count.tmax$count, names.arg = count.tmax$tmax)


count.snow <- airport %>% 
  group_by(wt18) %>%
  summarise(count = mean(count), f = n())


count.rain <- airport %>% 
  group_by(wt16) %>%
  summarise(count = mean(count), f = n())

count.ice_fog <- airport %>% 
  group_by(wt01) %>%
  summarise(count = mean(count), f = n())
  
# count.ice_fog$wt01 <- count.ice_fog$wt01 %>% replace_na(0)

## TODO : add a "bad weather" category ##
#Usage for different weather
c("ice_fog", "no ice_fog", "rain", "no rain")
barplot(height = c(count.ice_fog$count,count.rain$count, count.snow$count), 
        names.arg = c("ice_fog", "no ice_fog", "rain", "no rain", "snow", "no snow"), 
        col = c("red", "blue"), ylim = c(50000, 75000), beside=TRUE, xpd = FALSE,
        main = "average trip count for different weathers")




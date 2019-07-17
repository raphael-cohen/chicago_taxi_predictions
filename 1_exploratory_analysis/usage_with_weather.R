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

dates <- fread("/media/Onedrive/IIT/Courses/CSP571 Data Prep and Analysis/Project/weather/dates_and_count.csv")


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
merged <- fread("/media/Onedrive/IIT/Courses/CSP571 Data Prep and Analysis/Project/weather/trip_count_per_date_weather.csv")

cni <- merged[name =="CHICAGO NORTHERLY ISLAND, IL US"]
c47ne <- merged[name =="CHICAGO 4.7 NE, IL US"]
airport <- merged[name =="CHICAGO OHARE INTERNATIONAL AIRPORT, IL US"]

airport$wt01[is.na(airport$wt01)] <- 0
airport$wt18[is.na(airport$wt18)] <- 0
airport$wt22[is.na(airport$wt22)] <- 0

count.tmax <- cni %>% 
  group_by(tmax) %>%
  summarise(count = mean(count), f = n())%>%
  arrange(tmax)

barplot(height = count.tmax$count, names.arg = count.tmax$tmax, 
        col = c('aquamarine2', 'brown1'),
        main = "Average trip count for different temperatures (one day)", 
        cex.names=1.3, cex.axis = 1.3, cex.main=1.5)


count.snow <- airport %>% 
  group_by(wt18) %>%
  summarise(count = mean(count), f = n())


count.rain <- airport %>% 
  group_by(wt01) %>%
  summarise(count = mean(count), f = n())

count.ice_fog <- airport %>% 
  group_by(wt22) %>%
  summarise(count = mean(count), f = n())

airport[,bad_w:=rowSums (airport[,c('wt01', 'wt18', 'wt22')], na.rm = FALSE, dims = 1)]
airport$bad_w <- sapply(airport$bad_w, function(y) min(1,y))

count.bad_w <- airport %>% 
  group_by(bad_w) %>%
  summarise(count = mean(count), f = n())
  
# count.ice_fog$wt01 <- count.ice_fog$wt01 %>% replace_na(0)

## TODO : add a "bad weather" category ##
#Usage for different weather
leg <- c("good weather", "bad weather", "no ice fog", "ice fog", "no rain", "rain", "no snow", "snow")
barplot(height = c(count.bad_w$count, count.ice_fog$count,count.rain$count, count.snow$count), 
        names.arg = leg, 
        col = c('aquamarine2', 'brown1'), ylim = c(50000, 75000), beside=TRUE, xpd = FALSE,
        main = "Average trip count for different weathers (one day)", cex.names=1.3, cex.axis = 1.3, cex.main=1.5)


barplot(height = c(count.ice_fog$count,count.rain$count, count.snow$count), 
        names.arg = c("ice_fog", "no ice_fog", "rain", "no rain", "snow", "no snow"), 
        col = rep(brewer.pal(6, "Set1")), ylim = c(50000, 75000), beside=TRUE, xpd = FALSE,
        main = "average trip count for different weathers")


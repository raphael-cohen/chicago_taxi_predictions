library(data.table)
library(lubridate)
library(ggplot2)

data_dir <- '/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2017'
filename <- paste(data_dir, '1.csv', sep = "/")
df <- fread(filename)
head(df)
dt <- hour(hm(df$Trip.Start.Time[1]))
hour(dt)

combined <- df[,.N,by=Trip.Start.Time]
combined$Trip.Start.Time <- as.POSIXct(paste0("2010-01-01 ", combined$Trip.Start.Time), tz = "GMT")
combined$wkNo <- rep(1, nrow(combined))
head(combined)

names(combined)
ggplot(combined, aes(x = Trip.Start.Time, y = N)) +
  geom_line() +
  ggtitle('Number of trips (interval of 15 minutes)') + 
  xlab('') + ylab('')



for(wk in 2:5){
  filename <- paste(wk,'csv', sep = '.')
  filename <- paste(data_dir, filename, sep = "/")
  df <- fread(filename)
  temp <- df[,.N,by=Trip.Start.Time]
  temp$Trip.Start.Time <- as.POSIXct(paste0("2010-01-01 ", temp$Trip.Start.Time), tz = "GMT")
  temp$wkNo <- rep(wk, nrow(temp))
  
  combined <- rbind(combined, temp)
}
summary(combined$wkNo)
combined[,wkNo := as.factor(wkNo)]
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(combined, aes(x = Trip.Start.Time, y = N, group = wkNo, colour = wkNo)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") + 
  ggtitle('Number of trips (interval of 15 minutes)') + 
  xlab('') + ylab('') +
  scale_fill_gradient(low="blue", high="red")

ggsave('Weekwise_trend', plot = last_plot(), device = 'jpeg')


#To show that every time category has almost similar trips for a time range:


data_dir <- '/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016'
filename <- paste(data_dir, '1.csv', sep = "/")
df <- fread(filename)

head(df)
as.integer(hour(hm(df$Trip.Start.Time[2]))/2)
hour(hm(df$Trip.Start.Time[2]))

df$time_category <- as.integer(hour(hm(df$Trip.Start.Time))/2)
head(df$time_category)

plot(combined$time_category)
combined <- df[,.N,by=time_category]
combined$wkNo <- rep(1, nrow(combined))
head(combined)

for(wk in 2:53){
  filename <- paste(wk,'csv', sep = '.')
  filename <- paste(data_dir, filename, sep = "/")
  df <- fread(filename)
  df$time_category <- as.integer(hour(hm(df$Trip.Start.Time))/2)
  temp <- df[,.N,by=time_category]
  temp$wkNo <- rep(wk, nrow(temp))
  
  combined <- rbind(combined, temp)
}
combined[,wkNo := as.factor(wkNo)]
combined[,time_category := as.factor(time_category)]

ggplot(combined, aes(x = wkNo, y = N, group = time_category, colour = time_category)) +
  geom_line() +
  ggtitle('Number of trips in each time category') + 
  xlab('week number in 2016') + ylab('Number of Trips') +
  scale_x_discrete(breaks = seq(1, 53, by = 2))

ggsave('Time_category_trend', plot = last_plot(), device = 'jpeg')


#To show that every day of week has almost similar trends:


data_dir <- '/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016'
filename <- paste(data_dir, '1.csv', sep = "/")
df <- fread(filename)

head(df)
plot(combined$time_category)
combined <- df[,.N,by=Trip.DayText]
combined$wkNo <- rep(1, nrow(combined))
head(combined)

for(wk in 2:53){
  filename <- paste(wk,'csv', sep = '.')
  filename <- paste(data_dir, filename, sep = "/")
  df <- fread(filename)
  temp <- df[,.N,by=Trip.DayText]
  temp$wkNo <- rep(wk, nrow(temp))
  
  combined <- rbind(combined, temp)
}
combined[,wkNo := as.factor(wkNo)]
combined[,Trip.DayText := as.factor(Trip.DayText)]

ggplot(combined, aes(x = wkNo, y = N, group = Trip.DayText, colour = Trip.DayText)) +
  geom_line() +
  ggtitle('Number of trips for each day of week') + 
  xlab('week number in 2016') + ylab('Number of Trips') +
  scale_x_discrete(breaks = seq(1, 53, by = 2))

ggsave('Day_of_week_trend', plot = last_plot(), device = 'jpeg')


#Average No. of trips, by time category and day of week.
data_dir <- '/Users/pranavlal/Documents/Data Prep/Project/ChicagoTaxiAnalysis/Data/CleanData/2016'
filename <- paste(data_dir, '1.csv', sep = "/")
df <- fread(filename)
df$time_category <- as.integer(hour(hm(df$Trip.Start.Time))/2)
averages <- df[,.N,by= .(Trip.DayText, time_category)]

for(wk in 2:53){
  filename <- paste(wk,'csv', sep = '.')
  filename <- paste(data_dir, filename, sep = "/")
  df <- fread(filename)
  df$time_category <- as.integer(hour(hm(df$Trip.Start.Time))/2)
  averages <- rbind(averages, df[,.N,by= .(Trip.DayText, time_category)])
}

combined_avgs <- averages[, mean(N), by = .(Trip.DayText, time_category)]
combined_avgs

ggplot(combined_avgs, aes(x = time_category, y = V1, group = Trip.DayText, colour = Trip.DayText)) +
  geom_line() +
  ggtitle('Average No. of trips') + 
  xlab('Time Category') + ylab('Avg. No. of Trips') 

ggsave('Day_of_week_trend_2', plot = last_plot(), device = 'jpeg')


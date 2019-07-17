library(data.table)
library(ggplot2)
library(dplyr)


years = c('2013', '2014', '2015', '2016', '2017')
weekNumbers = 1:53

#Initialize an empty data.table to hold aggregated values

taxi.company.data <- data.table(Company = character(),
                                Earnings = numeric(),
                                noOfTrips = integer(),
                                Trip.Miles = numeric(),
                                year = integer())

#Iterate through all file in year-week hierarchy

for(i in 1:length(years)){
  yr <- years[i]
  for (wkNo in weekNumbers) {
    filePath = paste('Data',yr,toString(wkNo),sep = '/')
    filePath = paste(filePath,'csv', sep = '.')
    if(!file.exists(filePath)){
      next
    }
    df <- fread(file = filePath, header = TRUE)
    
    #Remove unnecessary columns
    df[,c('Taxi.ID', 'Trip.Seconds', 'Pickup.Census.Tract', 'Pickup.Community.Area', 'Pickup.Centroid.Latitude', 'Pickup.Centroid.Longitude', 'Dropoff.Census.Tract', 'Dropoff.Community.Area', 'Dropoff.Centroid.Latitude', 'Dropoff.Centroid.Longitude', 'Community.Areas') := NULL]
    
    #Assign Company Name as a factor
    df[,Company := as.factor(Company)]
    
    #Calculate earnings, number of trips, and total miles for reach company per year
    earnings <- df[, .(Earnings = sum(Trip.Total, na.rm = TRUE), noOfTrips = .N, Trip.Miles = sum(Trip.Miles, na.rm = TRUE), year = as.integer(yr)), by = .(Company)]
    
    #Append information to primary aggregate data.table
    taxi.company.data <- rbind(taxi.company.data, earnings)
    
  }
  
  #Aggregate data collected for each year
  taxi.company.data[,Company := as.factor(Company)]
  taxi.company.data <- taxi.company.data[, .(Earnings = sum(Earnings, na.rm = TRUE), noOfTrips = sum(noOfTrips, na.rm = TRUE), Trip.Miles = sum(Trip.Miles, na.rm = TRUE)), by = .(year, Company)]
}

#Verify that all trips have been accounted for
sum(taxi.company.data$noOfTrips)

#Write to disk for future reference
fwrite(taxi.company.data, file = 'TaxiCompanyData.csv')


######VISUALIZATIONS#########

#Set colour palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Filter top 5 companies by number of trips in each year
top.5.by.trips <- taxi.company.data[Company != "", ][order(-year, -rank(noOfTrips))] %>%
  group_by(year) %>%
  top_n(n = 5, wt = noOfTrips)

ggplot(top.5.by.trips, aes(x = year, y = noOfTrips / 100000)) +
  geom_col(aes(fill = reorder(Company, noOfTrips)), position = "dodge", show.legend = TRUE, colour = 'black') +
  labs(y = 'Number of Trips in 100,000s', x = 'Year', fill = 'Taxi Company') +
  scale_fill_manual(values=cbPalette)

ggsave('Taxi_Companies_By_Trips', plot = last_plot(), device = 'jpeg')

#Filter top 5 companies by earnings in each year
top.5.by.earnings <- taxi.company.data[Company != "", ][order(-year, -rank(Earnings))] %>%
  group_by(year) %>%
  top_n(n = 5, wt = Earnings)


ggplot(top.5.by.earnings, aes(x = year, y = Earnings / 100000)) +
  geom_col(aes(fill = reorder(Company, Earnings)), position = "dodge", show.legend = TRUE, colour = 'black') +
  labs(y = 'Earnings in million $', x = 'Year', fill = 'Taxi Company') +
  scale_fill_manual(values=cbPalette)

ggsave('Taxi_Companies_By_Earnings', plot = last_plot(), device = 'jpeg')

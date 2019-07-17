require("RPostgreSQL")
library(data.table)
library(ggplot2)
library(ggmap)



pw <- {
  "azery"
}

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "taxi_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)

dbExistsTable(con, "taxi_trips")


lat <- 41.875757
lon <- -87.636575

airport_lat <- 41.978568
airport_lon <- -87.884544

# lat <- 41.896580
# lon <- -87.631431
#install this for api key
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
ggmap(get_googlemap())
register_google(key = "google_key")

map_of_Chicago <- get_map(location = c(lon = airport_lon, lat = airport_lat),
                          zoom = 12, maptype = "roadmap", source = "google", api_key = "google_key")#"watercolor")#, scale = 1)


p <- ggmap(map_of_Chicago, extent = "panel", maprange=FALSE)
p

setwd(dir = "/media/Onedrive/IIT/Courses/CSP571 Data Prep and Analysis/Project/map_test")

# sample <- c(0,4,7,10,12,16,20)
# sample <- c(1,2,3,5,6,8,9,11,13,14,15,17,18,19,21,22,23)
sample <- 0:23

for(i in sample){
  print(i)
  
  request <- paste("SELECT pickup_centroid_latitude as lat, pickup_centroid_longitude as lon FROM taxi_trips WHERE EXTRACT(HOUR FROM start_time) = "
                    ,toString(i), " AND 0.05 >= random();", sep = "")
  
  # request <- paste("SELECT dropoff_centroid_latitude as lat, dropoff_centroid_longitude as lon FROM taxi_trips WHERE EXTRACT(HOUR FROM start_time) = "
  #                  ,toString(i), " AND 0.05 >= random();", sep = "")
  
  coord1 <- dbGetQuery(con, request)
  
  
  dir_to_save <- paste(getwd(), "/air_p_",toString(i), ".png", sep = "")
  
  p <- ggmap(map_of_Chicago, extent = "panel", maprange=FALSE) +
    geom_density2d(data = coord1, aes(x = lon, y = lat)) +
    stat_density2d(data =  coord1, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                   size = 2, bins = 30, geom = 'polygon') +
    scale_fill_gradient(low = "yellow", high = "red", limits = c(0,15000)) +
    scale_alpha(range = c(.1, .3), guide = FALSE) +
    guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
    theme(legend.text=element_text(size=20))  
  
  png(filename=dir_to_save, width = 2000, height = 2000)
    print(p)
  dev.off()  
}
  








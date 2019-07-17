require(maps)
require(ggmap)
library(raster)
library(sp)
library(maptools)
gpclibPermit()
library(maptools)
library(mapdata)
library(ggmap)
# library(geosphere)
library(maps)
library(ggplot2)
library(data.table)


lon <- c(-87.647248118, -87.647938205, -87.662544385, -87.751958893,-87.720089827, -87.756977729, -87.673019415, -87.740255128, -87.701586039) 
lat <- c(41.954492668, 41.774133601, 41.768086308, 41.879527422, 41.855245158, 41.89515102, 41.997922563, 41.881212919, 41.946538954) 
df <- as.data.frame(cbind(lon,lat))

filename = "/media/Download/taxi_split/100k_test.csv"
dt <- fread(filename)



map_of_Chicago <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 12, maptype = "roadmap")#"watercolor")#, scale = 1)



min.lat <- min(dt$`Pickup Centroid Latitude`, na.rm = TRUE)
max.lat <- max(dt$`Pickup Centroid Latitude`, na.rm = TRUE)


ggmap(map_of_Chicago, extent = "panel", maprange=FALSE) +
  geom_density2d(data = dt, aes(x = `Dropoff Centroid Longitude`, y = `Dropoff Centroid Latitude`)) +
  stat_density2d(data =  dt, aes(x = `Dropoff Centroid Longitude`, y = `Dropoff Centroid Latitude`,
                                 fill = ..level.., alpha = ..level..),size = 2, bins = 30, geom = 'polygon')+#, n = 10) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(.1, .3), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))#+
# theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))





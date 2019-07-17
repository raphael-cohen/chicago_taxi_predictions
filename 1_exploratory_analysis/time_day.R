require("RPostgreSQL")

pw <- {
  "password"
}

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)

dbExistsTable(con, "taxi_trips")


query <- "SELECT minute, sum(minute_ct) OVER (ORDER BY minute) AS running_ct
FROM  (
  SELECT date_trunc('minute', start_time::time) AS minute
  , count(*) AS minute_ct
  FROM   taxi_trips
  GROUP  BY 1
) sub
ORDER  BY 1;"
res <- dbGetQuery(con, query)

res$datetime <- as.POSIXct(paste0("2010-01-01 ", res$minute), tz = "GMT")
res$count <- res$running_ct
for(i in 2:nrow(res)) {
  res$count[i] = res$running_ct[i] - res$running_ct[i-1]
}

library(ggplot2)

ggplot(res, aes(x = datetime, y = count)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") + 
  ggtitle('Number of trips (interval of 15 minutes)') + 
  xlab('') + ylab('')

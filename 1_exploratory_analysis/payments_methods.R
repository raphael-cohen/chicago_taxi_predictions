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

payment_type <- dbGetQuery(con, "SELECT COUNT(id), payment_type FROM taxi_trips GROUP BY payment_type;")

payment_type$payment_type
sum(payment_type$count)
options(scipen=5)
names <- payment_type$payment_type
barplot(payment_type$count/sum(payment_type$count)*100, names=payment_type$payment_type, main="Payment methods (%)", names.arg = names, cex.names=.7,horiz=TRUE,las=1)


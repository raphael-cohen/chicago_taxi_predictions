library(data.table)

years = c('2013', '2014', '2015', '2016', '2017')
seasons = c('Spring', 'Summer', 'Fall', 'Winter')
weekNumbers = 1:52

counts <- data.table(c(rep('2013',4), rep('2014',4), rep('2015',4), rep('2016',4), rep('2017',4)),
                     seasons,
                     c(0,0,0,0))
colnames(counts) <- c('years', 'season', 'count')
for(i in 1:length(years)){
  yr <- years[i]
  for (wkNo in weekNumbers) {
    filePath = paste('Data',yr,toString(wkNo),sep = '/')
    filePath = paste(filePath,'csv', sep = '.')
    if(!file.exists(filePath)){
      next
    }
    df <- fread(file = filePath, header = TRUE)
    ssn <- 'Spring'
    if(wkNo >= 8 && wkNo <= 20){
      ssn <- 'Spring'
    }
    else if(wkNo >=21 && wkNo <=33){
      ssn <- 'Summer'
    }
    else if(wkNo >= 34 && wkNo <=46){
      ssn <- 'Fall'
    }
    else{
      ssn <- 'Winter'
    }
    print(filePath)
    print(yr)
    print(ssn)
    print(nrow(df))
    counts[years == yr & season == ssn, count := count + nrow(df)]
  }
}

counts
library(ggplot2)
ggplot(counts, aes(x = years, y = count, fill = season))+
  geom_col(position = "dodge")

ggsave('YearlyTrend', plot = last_plot(), device = 'jpeg')

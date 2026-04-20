#rewrite in clean code format

library(data.table)

temperatures <- c(3, 6, 10, 14)
weights      <- c(1, 0.8, 1.2, 1)

# function for multiplying
multiply <- function(x, y) {
  x * y
}

results <- multiply(temperatures, weights)

runoff_day <- readRDS('./data/runoff_day.rds')
dt_runoff_day <- data.table(runoff_day)

#percentage change between mean runoff of January, February and March for REES station
dt_runoff_mean_percent <- dt_runoff_day[month(date) %in% 1:3 & sname == 'REES', 
                                .(mean = mean(value)), 
                                by = .(month = month(date), sname)][order(month), 
                                                                    percent_change := (mean / shift(mean) - 1) * 100]

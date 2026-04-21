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


runoff_station <- readRDS('./data/raw/runoff_stations.rds')
# How big is the Rhine catchment (km2)?
runoff_station <- readRDS('./data/runoff_stations.rds')
# the data provided is only for the station catchment area, 
# not the whole rhine catchment,
# for the whole rhine's station catchment area
total_catchment_area <- runoff_station[,sum(area)]
# the Rhine catchment is 1266335 in km^2
  
#If it rained for one full day over the whole catchment area at 0.5mm/hour 
#and all the precipitated water ended up in the river, 
#how much would be the increase in the average river runoff? 
#Write a script that performs the calculation.
catchment_area_km2 <- 1266335  
rain_intensity_mm_hr <- 0.5 
duration_hr <- 24

area_m2 <- catchment_area_km2 * 1e6

# Convert Rain to meters (m)
total_rain_m <- (rain_intensity_mm_hr * duration_hr) / 1000

# Calculate Total Volume in cubic meters (m3)
total_volume_m3 <- area_m2 * total_rain_m

# Calculate Runoff Increase in m3/s
runoff_increase <- total_volume_m3 / (duration_hr * 3600)

# the increase in the average river runoff will be 175879.9 m^3/s
#(Optional) How much time does a rain drop falling at Alpine Rhine need to reach the ocean? 
#Write a script that performs the calculation.



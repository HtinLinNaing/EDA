list.files('./data/raw')

list.files("./data/raw/runoff_day")

library(data.table)
runoff_station <-  fread('./data/raw/runoff_stations.csv')
runoff_station

#overview of datatable
str(runoff_station)

#content of the column
runoff_station$area
summary(runoff_station$area)
summary(runoff_station$altitude)
fread('./data/raw/runoff_day/6335020_Q_Day.Cmd.txt')

runoff_station[, sname := factor(abbreviate(station))]

runoff_station[, id := factor(id)]
runoff_station[, lat := round(lat, 3)]
runoff_station[, lon := round(lon, 3)]
runoff_station[, altitude := round(altitude, 0)]
head(runoff_station)


raw_path <- './data/raw/runoff_day/'
fnames <- list.files(raw_path)
n_station <- length(fnames)
id_length <- 7
runoff_day_raw <- data.table()
id_sname <- runoff_station[, .(id, sname)]
head(id_sname)

saveRDS(runoff_station, './data/runoff_stations_raw.rds')



file_count <- 1
temp_dt <- fread(paste0(raw_path, fnames[file_count]))
temp_dt


station_id <- substr(fnames[file_count], 1, id_length)
temp_dt <- cbind(id = factor(station_id), temp_dt)


temp_dt <- id_sname[temp_dt, on = 'id']

runoff_day_raw <- rbind(runoff_day_raw, temp_dt)

colnames(runoff_day_raw)[3:4] <- c('date', 'value')
setnames(runoff_day_raw,"YYYY-MM-DD", 'date')
setnames(runoff_day_raw,'Value', 'value')

runoff_day_raw[, date := as.Date(date)]

saveRDS(runoff_day_raw, './data/runoff_day_raw.rds')
unique(runoff_day_raw$sname) == 'DIER'


runoff_coordinates <- runoff_station[, .(sname, lat, lon)]
runoff_coordinates <- melt(runoff_coordinates, id.vars = 'sname')
head(runoff_coordinates)

###############
######################
###############
library(ggplot2)

rees_runoff_day <- runoff_day_raw[sname == 'REES']
head(rees_runoff_day)


ggplot(data = rees_runoff_day) +
  geom_line(aes(x = date, y = value))

ggplot(data = rees_runoff_day, 
       aes(x = date, y = value)) +
  geom_point()

summary(rees_runoff_day)

rees_dier_runoff_day <- runoff_day_raw[sname == 'REES' | sname == 'DIER']
tail(rees_dier_runoff_day)
ggplot(data = rees_dier_runoff_day) +
  geom_line(aes(x = date, y = value, col = sname))


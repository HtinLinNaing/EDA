library(data.table)
library(ggplot2)
library(ggrepel)

runoff_stations <- readRDS('./data/runoff_stations_new.rds')
head(runoff_stations)
str(runoff_stations)

runoff_stations_short <- runoff_stations[, .(sname, area, altitude, size)]
runoff_stations_short[, altitude := as.integer(altitude)]

#task_1
#Create a new data.table from runoff_stations, containing sname, area, altitude. 
#Then, transform it to tidy format.
runoff_stations_tidy <- melt(runoff_stations_short, 
                             id.vars = "sname", 
                             measure.vars = c("area", "altitude", "size"),
                             variable.name = "station_info", 
                             value.name = "val")


#task_2 and 3
#Create a geom_point plot of area (x axis) vs. altitude (y axis).
#Try to reproduce the following graphs:
runoff_stations_wide <- dcast(runoff_stations_tidy, 
                              sname ~ station_info, 
                              value.var = "val")

p1 <- ggplot(data = runoff_stations_wide, aes(x = area, y = altitude, col = size)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_text_repel(aes(label = sname),
                  size = 3,
                  max.overlaps = 10,      
                  box.padding = 0.5,       
                  point.padding = 0.3,     
                  segment.color = 'grey50')+
  scale_color_gradientn(colors = c("darkblue", "lightblue"), name = "size") +
  labs(
    title = "Runoff Stations: Area & Altitude",
    x = "Area",
    y = "Altitude"
  ) +
  theme_minimal()

ggsave("./results/figures/assignment3/stations_area_and_altitude_size.png", 
       plot = p1,
       width = 10, 
       height = 6, 
       dpi = 300)

runoff_stations_location_wide <- runoff_stations[, .(sname, lat, lon, altitude)]

p2 <- ggplot(data = runoff_stations_location_wide, aes(x = lon, y = lat, col = altitude)) +
  geom_point(size = 2, alpha = 0.6) +
  # geom_text(aes(label = sname), 
  #           check_overlap = TRUE, 
  #           hjust = 3,
  #           # Move text slightly above the point
  #           size = 3) +
  geom_text_repel(aes(label = sname),
    size = 3,
    max.overlaps = 10,      
    box.padding = 0.5,       
    point.padding = 0.3,     
    segment.color = 'grey50')+
  scale_color_gradientn(colors = c("darkgreen", "darkred"), name = "Altitude") +
  labs(
    title = "Runoff Stations: Area & Altitude",
    x = "Area",
    y = "Altitude"
  ) +
  theme_minimal()

ggsave("./results/figures/assignment3/stations_area_and_altitude_altitude.png", 
       plot = p2,
       width = 10, 
       height = 6, 
       dpi = 300)

#task_4
#Create a graph comparing the periods of available data at each station 
#(assume that there are no missing values).

station_periods <- runoff_stations[, .(start, end),
                                   by = sname]

p3 <- ggplot(station_periods, aes(y = sname)) +
  geom_linerange(aes(xmin = start, xmax = end), 
                 color = "steelblue", 
                 linewidth = 2) +
  geom_point(aes(x = start), color = "darkblue", size = 3) + 
  geom_point(aes(x = end), color = "darkred", size = 3) + 
  labs(title = "Peiods of available data per station",
       x = "Year",
       y = "Station")+
  theme_minimal()

ggsave("./results/figures/assignment3/stations_data_available_period.png", 
       plot = p3,
       width = 10, 
       height = 6, 
       dpi = 300)

#Explorer’s Questions
# 1 Which are the units for area and runoff in our records?
# does not find any exactly description about that, 
# but usually square kilometers for area and
# cubic meters per second for runoff in hydrology standard.

# 2 Which is the average catchment area and runoff of Rhine, according to our data? 
# Write a script that performs the calculation.
mean_area <- runoff_stations[, mean(area, na.rm = TRUE)] 
# 74490.29

runoff_day <- readRDS('./data/runoff_day.rds')
mean_runoff <- runoff_day[, mean(value, na.rm = TRUE)]
# 1372.793

# 3 Which is the average runoff in each station? Present them in a graph.
station_means <- runoff_day[, 
                            .(mean_runoff = mean(value, na.rm = TRUE)), 
                            by = sname]

p4 <- ggplot(station_means, aes(x = sname, y = mean_runoff)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Average Runoff per Station",
       subtitle = "Rhine River Basin",
       x = "Station",
       y = "Mean Runoff") +
  theme_minimal()

ggsave("./results/figures/assignment3/stations_average_runoff.png", 
       plot = p4,
       width = 10, 
       height = 6, 
       dpi = 300)

# 4 Is there any relationship between station altitude and catchment area? Why?
cor_alt_area <- cor(x = runoff_stations$altitude, y = runoff_stations$area)
# -0.8611829, which means there is strong negative correlation 
# between station altitude and catchment area.



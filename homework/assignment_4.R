library(data.table)
library(ggplot2)
list.files("./data/")

#task_1
# Transform runoff_stats to tidy format 
# and then use it to plot mean, median, minimum and maximum for each location 
# as a scaterplot (x axis station, y axis runoff) with different colors 
# and point types for each statistic.
runoff_day <- readRDS("data/runoff_day.rds")
str(runoff_day)
head(runoff_day)

runoff_day_stats <- runoff_day[, 
                               .(min_runoff = min(value), 
                                 max_runoff = max(value),
                                 sd_runoff = sd(value),
                                 mean_runoff = mean(value),
                                 value),
                               by = sname]
head(runoff_day_stats)

runoff_day_stats_tidy <- melt(runoff_day_stats, 
                          id.vars = "sname", 
                          measure.vars = c("min_runoff", "max_runoff", "sd_runoff", "mean_runoff"),
                          variable.name = "statistics_day", 
                          value.name = "values")
head(runoff_day_stats_tidy)

p1 <- ggplot(data = runoff_day_stats_tidy, aes(x = sname, y = values, col = statistics_day)) +
  geom_point(size = 2, alpha = 0.6) +
  labs(
    title = "Runoff Stations: Mean, Median, Minimum & Maximum",
    x = "Stations",
    y = "Runoff"
  ) +
  theme_bw()

ggsave("./results/figures/assignment4/stations_statistics.png", 
       plot = p1,
       width = 10, 
       height = 6, 
       dpi = 300)

#task_2
# Estimate the skewness and coefficient of variation for each record 
# (a) as a new column in runoff_stats and 
# (b) as a new data.table.
#(a)
head(runoff_day_stats_tidy)

library(moments)
# Assuming your columns are named 'mean_day' and 'sd_day'
# or you are calculating them from raw data within the table

runoff_day_stats[, `:=`(
  coeff_variation = sd_runoff / mean_runoff,
  skewness = skewness(value)
), by = sname]

# p2 <- ggplot(runoff_day, aes(value)) +
#   geom_histogram(fill = "#97B8C2") +
#   facet_wrap(~sname, scales = 'free') +
#   labs(
#     title = "Runoff Stations: Mean, Median, Minimum & Maximum",
#     x = "Stations",
#     y = "Runoff"
#   ) +
#   theme_bw()
#   theme_bw()
# ggsave("./results/figures/assignment3/stations_average_runoff.png", 
#        plot = p2,
#        width = 10, 
#        height = 6, 
#        dpi = 300)

#(b)
runoff_day_coefficient_var <- melt(runoff_day_stats, 
                                   id.vars = "sname", 
                                   measure.vars = c("coeff_variation",
                                                    "skewness"),
                                   variable.name = "statistics_day", 
                                   value.name = "values")
head(runoff_day_coefficient_var)


#task_3
# Can you plot each boxplot (facet) of monthly runoff 
# with different fill colour according to the runoff class?
head(runoff_day_stats)
head(runoff_day_stats_tidy)

runoff_day_stats_class <- runoff_day_stats

runoff_day_stats_class[, runoff_class := factor('low')]

runoff_day_stats_class[mean_runoff >= 1000 & mean_runoff < 2000, 
           runoff_class := factor('medium')]

runoff_day_stats_class[mean_runoff >= 2000, 
           runoff_class := factor('high')]

to_merge <- unique(runoff_day_stats_class[, .(sname, runoff_class)])
runoff_summary <- runoff_day[to_merge, on = 'sname']
runoff_summary[, month := factor(month(date))]
head(runoff_summary)


p2 <- ggplot(data = runoff_summary, aes(x = sname, y = value, fill = runoff_class)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~month, scales = 'free', ncol = 3) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "bottom" 
  ) +
  labs(title = "Monthly Runoff by Station", 
       x = "Station", 
       y = "Runoff")

ggsave("./results/figures/assignment4/stations_monthly_runoff.png", 
       plot = p2,
       width = 10, 
       height = 6, 
       dpi = 300)

#task_4 
# Use boxplot to plot daily runoff per station. 
# What do you observe regarding outliers? 
# Why do you think this happens?
p3 <- ggplot(data = runoff_summary, aes(x = date, y = value, fill = runoff_class)) +
  geom_boxplot(outlier.size = 0.5, orientation = "x") + 
  facet_wrap(~sname, scales = 'free', ncol = 6) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "bottom" 
  ) +
  labs(title = "Daily Runoff by Station", 
       x = "Date", 
       y = "Runoff")

ggsave("./results/figures/assignment3/stations_daily_runoff.png", 
       plot = p3,
       width = 10, 
       height = 6, 
       dpi = 300)

# All the outliers are above the boxes.
# because of the increased runoff event like storm, 
# or some natural event occurred


#task_5
# Create your own classes for area and altitude and 
# plot them in a scater plot similar to this one.
runoff_stations <- readRDS("./data/runoff_stations.rds")
str(runoff_stations)

runoff_stations_class <- runoff_stations[, .(sname, area, altitude, size)]

runoff_stations_class[, area_class := factor('small')]

runoff_stations_class[area >= 7000 & area < 120000, 
                      area_class := factor('medium')]

runoff_stations_class[area >= 120000, 
                      area_class := factor('large')]


runoff_stations_class[, alt_class := factor('low')]

runoff_stations_class[altitude >= 50 & altitude < 300, 
                      alt_class := factor('medium')]

runoff_stations_class[altitude >= 300, 
                      alt_class := factor('high')]

to_merge1 <- runoff_day_stats[, .(sname, mean_runoff)]
runoff_stations_summary <- runoff_stations_class[to_merge1, on = 'sname']
runoff_stations_summary

p4 <- ggplot(runoff_stations_summary, 
             aes(x = mean_runoff, 
                 y = area, 
                 col = area_class, 
                 cex = alt_class)) +
  geom_point() +
  labs(
    title = "Runoff Stations: Area & Altitude",
    x = "mean_day",
    y = "area"
  ) +
  theme_bw() +
  guides(
    col = guide_legend(order = 1), 
    cex = guide_legend(order = 2)
  ) 

ggsave("./results/figures/assignment3/stations_average_runoff.png", 
       plot = p4,
       width = 10, 
       height = 6, 
       dpi = 300)

#1 Which is the difference between the median and the 0.5 quantile?
# No difference.

#2 Why the median and the mean are not the same in Rhine runoff?
# Median is the middle value of softed values while mean is the average of the values.

#3 Do you notice something strange regarding the location of the stations LOBI and REES? Can you think of a possible explanation?
# they have maximum values for all monthly periods, and nearly constant runoff.

#4 Which were the months, seasons, years with the highest/lowest runoff at each location? 
# Try to present them in comprehensive way. Feel free to improvise!

ggplot(data = runoff_summary, aes(x = month, y = value, fill = runoff_class)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~sname, scales = 'free', ncol = 3) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "bottom" 
  ) +
  labs(title = "Monthly Runoff by Station", 
       x = "month", 
       y = "Runoff")
# 


#5 (Optional) Which is the average distance between each station in km? Which are the two closest and farest adjacent stations?

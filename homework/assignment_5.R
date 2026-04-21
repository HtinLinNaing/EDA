library(data.table)
library(ggplot2)
runoff_summary <- readRDS('./data/runoff_summary.rds')
runoff_stats <- readRDS('./data/runoff_stats.rds')
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_summer <- readRDS('./data/runoff_summer.rds')
runoff_winter <- readRDS('./data/runoff_winter.rds')
runoff_year <- readRDS('./data/runoff_year.rds')

head(runoff_month)
head(runoff_year)
head(runoff_summer)

# task1
# In our boxplot comparison of DOMA, BASR and KOEL.
# we have used summer and winter period.
# Can you repeat it for annual and monthly data? 
# Is there is some useful new information presented?

key_stations <- c('DOMA', 'BASR', 'KOEL')

runoff_summary_key <- runoff_summary[sname %in% key_stations]
runoff_month_key <- runoff_month[sname %in% key_stations]
runoff_winter_key <- runoff_winter[sname %in% key_stations]
runoff_summer_key <- runoff_summer[sname %in% key_stations]
runoff_year_key <- runoff_year[sname %in% key_stations]

year_thres <- 2000
runoff_month_key[year < year_thres, period := factor('pre_2000')]
runoff_month_key[year >= year_thres, period := factor('aft_2000')]
runoff_year_key[year < year_thres, period := factor('pre_2000')]
runoff_year_key[year >= year_thres, period := factor('aft_2000')]

to_plot <- rbind(cbind(runoff_month_key, time = factor('annual')), 
                 cbind(runoff_year_key, time = factor('monthly')), 
                 fill = T) 

p1 <- ggplot(to_plot, aes(time, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  labs(
    title = "Station (DOMA, BASR, KOEL) Average Runoff",
    x = "Time",
    y = "Runoff (m3/s)"
    ) +
  theme_bw()

ggsave("./results/figures/assignment5/stations_average_runoff.png", 
       plot = p1,
       width = 10, 
       height = 6, 
       dpi = 300)


#task_2
# We define high runoff as the daily runoff above the 0.9 quantile and 
# low runoff as the daily runoff below the 0.1 quantile. 
# Then we can estimate the mean high/low runoff per station. 
# Finally, we also compare the number of days with values above/below 0.9 and 
# 0.1 correspondingly 

runoff_day <- readRDS('./data/runoff_day.rds')
str(runoff_day)

runoff_day[, q10 := quantile(value, 0.1, na.rm = TRUE), 
           by = sname]

runoff_day[, q90 := quantile(value, 0.9, na.rm = TRUE), 
           by = sname]

# Mean high runoff (flooding potential)
high_mean <- runoff_day[value > q90, 
                       mean_high := mean(value), 
                       by = sname]

# Mean low runoff (drought potential)
low_mean <- runoff_day[value < q10, 
                      mean_low := mean(value), 
                      by = sname]

#have to extract only summary line to clean and to avoid duplicate sname error 
low_summary <- unique(low_mean[!is.na(mean_low), .(sname, mean_low)])

high_summary <- unique(high_mean[!is.na(mean_high), .(sname, mean_high)])

extremes_summary <- merge(low_summary, high_summary, by = "sname")
head(extremes_summary)

#count occurrences
days_count <- runoff_day[, 
                         .(days_low = .SD[value < q10, .N],
                          days_high = .SD[value > q90, .N],
                          total_days = .N), 
                         by = sname]

head(days_count)


#task_3
# How sensitive are slopes to adding new data? 
# extremely sensitive

p2 <- ggplot(runoff_year[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  labs(
    title = 'Stations Runoff : 1950 to today',
    x = "Year",
    y = "Runoff (m³/s)"
    ) +
  theme_bw()

ggsave("./results/figures/assignment5/stations_runoff_1950_2016_loess_method.png", 
       plot = p2,
       width = 10, 
       height = 6, 
       dpi = 300)

p3 <- ggplot(runoff_year[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  labs(
    title = 'Stations Runoff : 1950 to 2010',
    x = "Year",
    y = "Runoff (m³/s)"
  ) +
  theme_bw()

ggsave("./results/figures/assignment5/stations_runoff_1950_2010_loess_method.png", 
       plot = p3,
       width = 10, 
       height = 6, 
       dpi = 300)

p4 <- ggplot(runoff_year[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  labs(
    title = 'Staions Runoff : 1950 to today',
    x = "Year",
    y = "Runoff (m³/s)"
  ) +
  theme_bw()

ggsave("./results/figures/assignment5/stations_runoff_1950_2016_lm_method.png", 
       plot = p4,
       width = 10, 
       height = 6, 
       dpi = 300)

p5 <- ggplot(runoff_year[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  labs(
    title = 'Stations Runoff : 1950 to 2010',
    x = "Year",
    y = "Runoff (m³/s)"
  ) +
  theme_bw()

ggsave("./results/figures/assignment5/stations_runoff_1950_2010_lm_method.png", 
       plot = p5,
       width = 10, 
       height = 6, 
       dpi = 300)

# What do you observe? 
# What if you used linear regression instead of loess?
# the loess method can show the detail changes of trend for small time period.
# the lm method can show the trend for all the time period.

#Explorer’s questions
# 1.In retrospect, is DOMA a representative station? 
# Why do you think its behaviour is so different than the other stations?
# No, DOMA is not a representative station. 
# It's trend line is not align with others stations.

# 2.In our analysis, we have used only river runoff. 
# Precipitation is a factor strongly linked with runoff. 
# Can you perform a similar analysis (boxplots and regression) for precipitation? 
# Precipitation data averaged over the whole Rhine region can be found in the file precip_day.rds in folder data. 
# What do you observe?
#   
precip_day <- readRDS('./data/raw/precip_day.rds')

#format date
precip_day[, `:=`(year = year(date), month = month(date))]

# Precipitation annual
precip_year <- precip_day[, .(value = sum(value)), by = year]

p_precip <- ggplot(precip_year[year > 1950 & year <= 2010], aes(x = year, y = value)) +
  geom_line(col = "grey") +
  geom_smooth(method = 'loess', color = "blue", se = FALSE) + 
  geom_smooth(method = 'lm', color = "red", se = FALSE) +
  labs(
    title = "Annual Precipitation Trend (Rhine Region)",
    subtitle = "Blue = LOESS, Red = Linear Regression",
    x = "Year",
    y = "Annual Precipitation (mm)"
    ) +
  theme_bw()

ggsave("./results/figures/assignment5/precipitation_1950_2010_loess_lm__compared_method.png", 
       plot = p_precip,
       width = 10, 
       height = 6, 
       dpi = 300)

# 3.What are your thoughts about the changes in Rhine runoff after completing EDA?
# Runoff are at decreasing trend.

# 4.Which are some future analyses or other factors that should be examined? 
# Present some arguments related to the findings so far.


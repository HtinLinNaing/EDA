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

ggplot(to_plot, aes(time, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()



#task_2
runoff_day <- readRDS('./data/runoff_day.rds')
str(runoff_day)

runoff_day[, q10 := quantile(value, 0.1, na.rm = TRUE), 
           by = sname]

runoff_day[, q90 := quantile(value, 0.9, na.rm = TRUE), 
           by = sname]

# Mean high runoff (flooding potential)
high_mean <- runoff_day[value > q90, 
                       mean_high = mean(value), 
                       by = sname]

# Mean low runoff (drought potential)
low_mean <- runoff_day[value < q10, 
                      mean_low = mean(value), 
                      by = sname]

extremes_summary <- merge(low_mean, high_mean, by = "sname")
head(extremes_summary)


# Count occurrences
days_count <- runoff_day[, 
                         .(days_low = .SD[value < q10, .N],
                          days_high = .SD[value > q90, .N],
                          total_days = .N), 
                         by = sname]

head(days_count)


#task_3
# How sensitive are slopes to adding new data? 
# extremely sensitive

ggplot(runoff_year[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  labs(
    title = 'Stations Runoff : 1950 to today',
    x = "Year",
    y = "Runoff"
    ) +
  theme_bw()

ggplot(runoff_year[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  labs(
    title = 'Stations Runoff : 1950 to 2010',
    x = "Year",
    y = "Runoff"
  ) +
  theme_bw()

ggplot(runoff_year[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  labs(
    title = 'Staions Runoff : 1950 to today',
    x = "Year",
    y = "Runoff"
  ) +
  theme_bw()

ggplot(runoff_year[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  labs(
    title = 'Stations Runoff : 1950 to 2010',
    x = "Year",
    y = "Runoff"
  ) +
  theme_bw()

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
# Precipitation data averaged over the whole Rhine region can be found in the file precip_day.rds in folder data. What do you observe?
#   
# 3.What are your thoughts about the changes in Rhine runoff after completing EDA?
# Runoff are at decreasing trend.

# 4.Which are some future analyses or other factors that should be examined? 
# Present some arguments related to the findings so far.
# 
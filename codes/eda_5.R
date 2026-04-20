library(data.table)
library(ggplot2)
library(reshape2)

runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_month <- readRDS('data/runoff_month.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')
runoff_year <- readRDS('data/runoff_year.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())

dt <- runoff_summary[, .(sname, area)]
for_cor <- runoff_stats[dt, on = 'sname']
cor(for_cor$mean_day, for_cor$area)


for_cor_mat <- for_cor[, c('mean_day', 'area')]
cor(for_cor_mat)

runoff_month_mat <- dcast(runoff_month, date~sname)
runoff_month_cor <- cor(runoff_month_mat[, -1], use = "pairwise.complete.obs")
to_plot <- reshape2::melt(runoff_month_cor)

ggplot(data = to_plot, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(col = 'black') +
  scale_fill_gradient2(low = colset_4[4], 
                       high = colset_4[1], 
                       mid = colset_4[3],
                       midpoint = 0.5,
                       limits = c(-0.1, 1)) +
  geom_text(aes(label = round(value, 1))) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab(label = "") +
  ylab(label = "")

runoff_summary[, category := 'downstream']
runoff_summary$category[3:10] <- ('mid')
runoff_summary$category[1:2] <- ('upstream')
runoff_summary[, category := factor(category, levels = c('upstream', 'mid', 'downstream'))]

runoff_month_mean <- runoff_month[, .(value = mean(value)), .(month, sname)]
to_plot <- runoff_month[runoff_summary[, .(sname, category)], on = 'sname']

ggplot(to_plot, aes(x = factor(month), y = value, fill = category, group = month)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free') +
  scale_fill_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

key_stations <- c('DOMA', 'BASR', 'KOEL')

runoff_summary_key <- runoff_summary[sname %in% key_stations]
runoff_month_key <- runoff_month[sname %in% key_stations]
runoff_winter_key <- runoff_winter[sname %in% key_stations]
runoff_summer_key <- runoff_summer[sname %in% key_stations]
runoff_year_key <- runoff_year[sname %in% key_stations]

ggplot(runoff_year_key[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)")

saveRDS(runoff_summary, './data/runoff_summary.rds')
saveRDS(runoff_summary_key, './data/runoff_summary_key.rds')
saveRDS(runoff_month_key, './data/runoff_month_key.rds')
saveRDS(runoff_winter_key, './data/runoff_winter_key.rds')
saveRDS(runoff_summer_key, './data/runoff_summer_key.rds')
saveRDS(runoff_year_key, './data/runoff_year_key.rds')


#Comparison
library(data.table)
library(ggplot2)

runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_summary_key <- readRDS('data/runoff_summary_key.rds')
runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_summer_key <- readRDS('data/runoff_summer_key.rds')
runoff_winter_key <- readRDS('data/runoff_winter_key.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())

year_thres <- 2000

runoff_winter_key[year < year_thres, period := factor('pre_2000')]
runoff_winter_key[year >= year_thres, period := factor('aft_2000')]
runoff_summer_key[year < year_thres, period := factor('pre_2000')]
runoff_summer_key[year >= year_thres, period := factor('aft_2000')]

to_plot <- rbind(cbind(runoff_winter_key, season = factor('winter')), 
                 cbind(runoff_summer_key, season = factor('summer'))) 

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(to_plot[year >= 1983], aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

dt <- runoff_summary[, .(sname, area, category)]
to_plot <- runoff_stats[dt, on = 'sname']

ggplot(to_plot, aes(x = mean_day, y = area, col = category)) +
  geom_point(cex = 3) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(to_plot, aes(x = mean_day, y = area)) +
  geom_point(aes(col = category), cex = 3) +
  geom_smooth(method = 'lm', formula = y ~ x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot() +
  geom_point(data = to_plot, aes(x = mean_day, y = area, col = category), cex = 3) +
  geom_smooth(data = to_plot[c(1:7)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(data = to_plot[c(8:11)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(data = to_plot[c(12:17)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer_key, aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_winter_key, aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)

ggplot(runoff_winter[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

year_thres <- 1980
to_plot <- rbind(cbind(runoff_winter, season = factor('winter')), 
                 cbind(runoff_summer, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]

to_plot <- to_plot[year >= 1950]

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()


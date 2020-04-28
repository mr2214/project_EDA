library(data.table)
library(ggplot2)
getwd()
daily_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_day.rds')

daily_runoff$month <- format(as.Date(daily_runoff$date), "%m")
daily_runoff$year <- format(as.Date(daily_runoff$date), "%Y")
daily_runnoff_descriptive_statistics <- daily_runoff[, .(mean(value), max(value), min(value), sd(value), median(value)), by = .(id,year)]
colnames(daily_runnoff_descriptive_statistics) <-  c("id","year","mean","max","min","sd","median")
daily_runnoff_descriptive_statistics
daily_runnoff_descriptive_statistics$COF <- daily_runnoff_descriptive_statistics$sd/daily_runnoff_descriptive_statistics$mean
daily_runnoff_descriptive_statistics$skew <- 3*(daily_runnoff_descriptive_statistics$mean - daily_runnoff_descriptive_statistics$median)/daily_runnoff_descriptive_statistics$sd
daily_runnoff_descriptive_statistics$meanmaxratio <- daily_runnoff_descriptive_statistics$mean/daily_runnoff_descriptive_statistics$max
daily_runnoff_descriptive_statistics$meanminratio <- daily_runnoff_descriptive_statistics$mean/daily_runnoff_descriptive_statistics$min
daily_runnoff_descriptive_statistics

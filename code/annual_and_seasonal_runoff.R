library(data.table)
library(ggplot2)
getwd()
daily_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_day.rds')
annual_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_year.rds')
info_for_project <- readRDS(file = './data/Data for final project-20200428/runoff_eu_info.rds')

daily_runoff <- daily_runoff[value >= 0]
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
daily_runnoff_descriptive_statistics$range <- daily_runnoff_descriptive_statistics$max - daily_runnoff_descriptive_statistics$min
characersitic_of_station <- daily_runnoff_descriptive_statistics[, .(max(range), min(range)), by = id]
colnames(characersitic_of_station) <- c("ID","max_range", "min_range")
characersitic_of_station$ID <- as.numeric(characersitic_of_station$ID)
characersitic_of_station_2 <- merge(characersitic_of_station,info_for_project)
characersitic_of_station_2 <- characersitic_of_station_2[,c(1,2,3,10)]
characersitic_of_station_2
ggplot(characersitic_of_station_2, aes(x=1:202, y = max_range)) +
  geom_point()
# data could be splti to 0:2500 = low, 2500:5000 = midian, >5000 = high
ggplot(characersitic_of_station_2, aes(x=1:202, y = min_range)) +
  geom_point()
# data could be split to 0:500 = low,   500:1000 = median, >1000 high
ggplot(characersitic_of_station_2, aes(x=1:202, y = Alt)) +
  geom_point()
# data could be split 0:200 = low, 200:600 = mediam, >600 = high
characersitic_of_station_2[max_range < 2500, max_range_class := factor('low')]
characersitic_of_station_2[max_range > 2500 & max_range < 5000, max_range_class := factor('medium')]
characersitic_of_station_2[max_range > 5000, max_range_class := factor('high')]
characersitic_of_station_2[min_range < 500, min_range_class := factor('low')]
characersitic_of_station_2[min_range > 500 & min_range < 1000, min_range_class := factor('medium')]
characersitic_of_station_2[min_range > 1000, min_range_class := factor('high')]
characersitic_of_station_2[Alt < 200, alt_range_class := factor('low')]
characersitic_of_station_2[Alt > 200 & Alt < 600, alt_range_class := factor('medium')]
characersitic_of_station_2[Alt > 600, alt_range_class := factor('high')]
characersitic_of_station_2
daily_runnoff_descriptive_statistics
daily_runnoff_descriptive_statistics[year >= 1980, year_class := factor('Post 1980')]
daily_runnoff_descriptive_statistics[year < 1980, year_class := factor('Pre 1980')]
change_in_ratio <- daily_runnoff_descriptive_statistics[, .(mean(meanmaxratio), mean(meanminratio)), by = .(id, year_class)]
change_in_ratio
change_in_ratio[, year_class]
change_in_ratio <- change_in_ratio[-1,]
change_in_ratio <- change_in_ratio[-238,]
meanmax_difference <- c()
length(rownames(change_in_ratio))
for (i in 1:length(rownames(change_in_ratio))) {
  meanmax_difference[i] <- change_in_ratio$V1[(2 * i)] - change_in_ratio$V1[((2 * i)-1)]
}
meanmax_difference <- meanmax_difference[1:201]
meanmin_difference <- c()
for (i in 1:length(rownames(change_in_ratio))) {
  meanmin_difference[i] <- change_in_ratio$V1[(2 * i)] - change_in_ratio$V1[((2 * i)-1)]
}
meanmin_difference <- meanmin_difference[1:201]
meanmin_difference
print(change_in_ratio[,1], topn = 400)
change_in_ratio
print(unique(change_in_ratio[,1]), topn = 201)
length(meanmax_difference)
results <- data.table(meanmax_difference, meanmin_difference, unique(change_in_ratio[,1]))
results
print(results, topn = 201)

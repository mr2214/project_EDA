library(data.table)
library(ggplot2)
getwd()
daily_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_day.rds')
annual_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_year.rds')
info_for_project <- readRDS(file = './data/Data for final project-20200428/runoff_eu_info.rds')
daily_runoff <- readRDS(file = "./data/daily_runoff.rds")
annual_runoff
daily_runoff <- daily_runoff[value >= 0]
#daily_runoff$month <- format(as.Date(daily_runoff$date), "%m")
#daily_runoff$year <- format(as.Date(daily_runoff$date), "%Y")
#savedaily_runoff <- saveRDS(daily_runoff, file = "./data/daily_runoff.rds")

str(daily_runoff)
daily_runoff$value
daily_runoff <- daily_runoff[value >= 0]
daily_runoff
daily_runnoff_descriptive_statistics <- daily_runoff[, .(mean(value), max(value), min(value), sd(value), median(value)), by = .(id,year)]
colnames(daily_runnoff_descriptive_statistics) <-  c("id","year","mean","max","min","sd","median")
daily_runnoff_descriptive_statistics
daily_runnoff_descriptive_statistics$COF <- daily_runnoff_descriptive_statistics$sd/daily_runnoff_descriptive_statistics$mean
daily_runnoff_descriptive_statistics$skew <- 3*(daily_runnoff_descriptive_statistics$mean - daily_runnoff_descriptive_statistics$median)/daily_runnoff_descriptive_statistics$sd
daily_runnoff_descriptive_statistics$meanmaxratio <- daily_runnoff_descriptive_statistics$mean/daily_runnoff_descriptive_statistics$max
daily_runnoff_descriptive_statistics$meanminratio <- daily_runnoff_descriptive_statistics$mean/daily_runnoff_descriptive_statistics$min
daily_runnoff_descriptive_statistics
daily_runnoff_descriptive_statistics$range <- daily_runnoff_descriptive_statistics$max - daily_runnoff_descriptive_statistics$min
daily_runnoff_descriptive_statistics
#savedaily_runoff <- saveRDS(daily_runnoff_descriptive_statistics, file = "./data/daily_runoffds.rds")
yearly_summary <- daily_runnoff_descriptive_statistics[, .(mean(mean), mean(max), mean(min), mean(sd), mean(median)), by = id]
colnames(yearly_summary) <-  c("id","mean","max","min","sd","mediam")
yearly_summary
characersitic_of_station <- daily_runnoff_descriptive_statistics[, .(max(range), min(range)), by = id]
colnames(characersitic_of_station) <- c("ID","max_range", "min_range")
characersitic_of_station$ID <- as.numeric(characersitic_of_station$ID)
characersitic_of_station
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
# some altitude data is missing
daily_runnoff_descriptive_statistics
daily_runnoff_descriptive_statistics[year >= 1980, year_class := factor('Post 1980')]
daily_runnoff_descriptive_statistics[year < 1980, year_class := factor('Pre 1980')]
daily_runnoff_descriptive_statistics
change_in_ratio <- daily_runnoff_descriptive_statistics[, .(mean(meanmaxratio), mean(meanminratio)), by = .(id, year_class)]
# calculates mean/max and mean/min averages over time period pre and post 1980
unique(change_in_ratio[,1])
change_in_ratio[, year_class]
change_in_ratio <- change_in_ratio[-1,]
change_in_ratio$id
change_in_ratio[237]
change_in_ratio <- change_in_ratio[-237,]
meanmax_difference <- c()
change_in_ratio
length(rownames(change_in_ratio))
for (i in 1:length(rownames(change_in_ratio))) {
  meanmax_difference[i] <- change_in_ratio$V1[(2 * i)] - change_in_ratio$V1[((2 * i)-1)]
}
meanmax_difference <- meanmax_difference[1:200]
meanmin_difference <- c()
for (i in 1:length(rownames(change_in_ratio))) {
  meanmin_difference[i] <- change_in_ratio$V1[(2 * i)] - change_in_ratio$V1[((2 * i)-1)]
}
meanmin_difference <- meanmin_difference[1:200]
meanmin_difference

print(unique(change_in_ratio[,1]), topn = 201)
length(meanmax_difference)
results <- data.table(meanmax_difference, meanmin_difference, unique(change_in_ratio[,1]))
str(results)
colnames(results) <- c("meanmax_difference", "meanmin_difference", "ID")
characersitic_of_station_2[120,]
characersitic_of_station_2 <- characersitic_of_station_2[-c(1,120),]
characersitic_of_station_2
results$ID <- as.numeric(results$ID)
results_2 <- merge(characersitic_of_station_2,results)
results_2
results_2 <- results_2[,c(1,5,6,7,8,9)]
results_2
maxrangeclass_change <- results_2[, mean(meanmax_difference), by = max_range_class]
minrangeclass_change <- results_2[, mean(meanmax_difference), by = min_range_class][1:3,]
altrangeclass_change <- results_2[, mean(meanmax_difference), by = alt_range_class][c(1,3,4),]
maxrangeclass_change_min <- results_2[, mean(meanmin_difference), by = max_range_class]
minrangeclass_change_min <- results_2[, mean(meanmin_difference), by = min_range_class][1:3]
altrangeclass_change_min <- results_2[, mean(meanmin_difference), by = alt_range_class][c(1,3,4),]
daily_runoff$month <- as.numeric(daily_runoff$month)
daily_runoff[(month < 4), season := factor('winter')]
daily_runoff[(month > 6) & (month < 10), season := factor('summer')]
daily_runoff[300:350,]
summer_winter_runnoff <- daily_runoff[,mean(value), by = .(id,season)]
a <- which(summer_winter_runnoff$season == "winter")
b <- which(summer_winter_runnoff$season == "summer")
summer_winter_runnoff[c(a, b),]
daily_runoff$year <- as.numeric(daily_runoff$year)
daily_runoff[year >= 1980, year_class := factor('Post 1980')]
daily_runoff[year < 1980, year_class := factor('Pre 1980')]
daily_runoff
summer_winter <- daily_runoff[,mean(value), by = .(id,season,year_class)]
d <- which(summer_winter$season == "winter")
e <- which(summer_winter$season == "summer")
summer_winter <- summer_winter[c(d, e),]
summer_winter
unique(summer_winter$id)
summer_winter$year_class
summer_winter <- summer_winter[-1,]
summer_winter <- summer_winter[-237,]
summer_winter <- summer_winter[-401,]
summer_winter <- summer_winter[-637,]
summer_winter
percentage_change <- c()
for (i in 1:length(rownames(summer_winter))) {
  percentage_change[i] <- 100*(summer_winter$V1[(2 * i)] - summer_winter$V1[((2 * i)-1)])/(summer_winter$V1[((2 * i)-1)])
}
winter <- percentage_change[1:200]
summer <- percentage_change[201:400]
results_3 <- data.table(unique(summer_winter[,1]), winter, summer)
results_3
results_3$ID_type_winter <- ifelse(winter < 0, "below", "above")
ggplot(results_3, aes(x=id, y=winter, label=winter)) + 
  geom_bar(stat='identity', aes(fill=ID_type_winter), width=.5)  +
                    scale_fill_manual(name="percentage change", 
                                      labels = c("Positive", "Negitive"), 
                                      values = c("above"="#00ba38", "below"="#f8766d")) + 
                      labs(subtitle="percentage change of mean runnoff at stations before and after 1980", 
                           title= "Diverging Bars") + 
                      coord_flip()
                    
results_3$ID_type_summer <- ifelse(summer < 0, "below", "above")
ggplot(results_3, aes(x=id, y=summer, label=summer)) + 
  geom_bar(stat='identity', aes(fill=ID_type_summer), width=.5)  +
  scale_fill_manual(name="percentage change", 
                    labels = c("Positive", "Negitive"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="percentage change of mean runnoff at stations before and after 1980", 
       title= "Diverging Bars") + 
  coord_flip()

library(data.table)
library(ggplot2)
library(zoo)
library(corrplot)
getwd()
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
daily_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_day.rds')
annual_runoff <- readRDS(file = './data/Data for final project-20200428/runoff_eu_year.rds')
info_for_project <- readRDS(file = './data/Data for final project-20200428/runoff_eu_info.rds')
unique(info_for_project$River)
info_for_project$River
plot(info_for_project$ID,info_for_project$Alt)
annual_runoff
#accuilation will take placemat lower altitudes, hence will be easier to see trends in data, due to climate or human factors
#some rivers heavilyy influnced by snowmelt during summer months, while some rivers recieve little/no snow melt
characersitic_of_station_2$alt_range_class
#river elbe, rivr danube, neckar, saale, dee (scotland)
info_for_project
elbe_position <- which(info_for_project$River == "ELBE RIVER")
danube_position <- which(info_for_project$River == "DANUBE RIVER")
neckar_position <- which(info_for_project$River == "NECKAR")
saale_position <- which(info_for_project$River == "SAALE")
dee_position <- which(info_for_project$River == "DEE (SCOTLAND)")
dee_position
new <- info_for_project[c(elbe_position,danube_position,neckar_position,saale_position,dee_position),]
ids <- new[c(1,16,19,20,22),]
ids
ids$ID
position_in_annual_runoff <- which(daily_runoff$id == ids$ID)
position_in_annual_runoff
runoff_5_rivers <- daily_runoff[position_in_annual_runoff,]
runoff_5_rivers
runoff_5_rivers$month <- format(as.Date(runoff_5_rivers$date), "%m")
runoff_5_rivers$year <- format(as.Date(runoff_5_rivers$date), "%Y")
str(runoff_5_rivers$month)

roll_mean_discharge
runoff_5_rivers[,by = id]
ids
elbe_river <- which(runoff_5_rivers$id == ids$ID[1])
elbe_river <- runoff_5_rivers[elbe_river,]
elbe_river
runoff_5_rivers[]
elbe_river_place <- which(elbe_river$year == 1994)
elbe_river_place
runoff_5_rivers <- as.data.table(runoff_5_rivers)
runoff_5_rivers[,sum(value), by = .(year, id)]
sum_yearly_discharge <- runoff_5_rivers[,sum(value), by = .(year, id)]
sum_yearly_discharge
elbe_roll_mean <- sum_yearly_discharge[1:107, .(zoo::rollmean(V1, 5),zoo::rollmean(V1, 10), zoo::rollmean(V1, 20))]
elbe_roll_mean$V2 <- elbe_roll_mean$V2[98:103] <- NA
elbe_roll_mean$V3 <- elbe_roll_mean$V3[88:103] <- NA
elbe_roll_mean[98:103,2] <- NA
elbe_roll_mean[88:103,3] <- NA
elbe_roll_mean

ggplot(data = elbe_roll_mean , aes(x = 1:103, y = V1))+
  geom_point()
ggplot(data = elbe_roll_mean , aes(x = 1:103, y = V2))+
  geom_point()
ggplot(data = elbe_roll_mean , aes(x = 1:103, y = V3))+
  geom_point()
runoff_5_rivers
elbe_roll_mean <- runoff_5_rivers[1:107, .(zoo::rollmean(value, 5),zoo::rollmean(value, 10), zoo::rollmean(value, 20))]

elbe_roll_mean[98:103,2] <- NA
elbe_roll_mean[88:103,3] <- NA
elbe_roll_mean
ggplot(data = elbe_roll_mean , aes(x = 1:103, y = V1))+
  geom_point()
ggplot(data = elbe_roll_mean , aes(x = 1:103, y = V2))+
  geom_point()
ggplot(data = elbe_roll_mean , aes(x = 1:103, y = V3))+
  geom_point()
runoff_5_rivers <- runoff_5_rivers[value >= 0]
monthly_average_discharge <- runoff_5_rivers[,mean(value) ,by = .(id,month,year)]
monthly_average_discharge
ggplot(monthly_average_discharge, aes(x = factor(month), y = V1, group = month)) +
  geom_boxplot() +
  facet_wrap(~id, scales = 'free') +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
runoff_5_rivers
yearly_annual_discharge_5_rivers <- runoff_5_rivers[, sum(value), by = .(id,year)]
runnoff_year_mat <- dcast(yearly_annual_discharge_5_rivers, year~id)
runnoff_year_mat
runnoff_year_mat <- runnoff_year_mat[45:113,]
runnoff_year_mat
runoff_year_mat <- runnoff_year_mat[9202:24008,]
runoff_year_cor <- cor(runnoff_year_mat[, -1], use = "pairwise.complete.obs")
runoff_year_cor <- round(runoff_year_cor,2)
to_plot <- melt(runoff_year_cor)
to_plot


col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")
corrplot(runoff_year_cor, order = "hclust", addrect = 2, col = col4(10), method = "number")

runoff_5_rivers <- runoff_5_rivers[,-6]
runoff_5_rivers
runoff_5_rivers_2 <- runoff_5_rivers
runoff_5_rivers_2
runoff_5_rivers_2$month <- as.numeric(factor((runoff_5_rivers_2$month)))
runoff_5_rivers_2[month == 1 | month == 2 | month == 3, season := factor('winter')]
runoff_5_rivers_2[month == 7 | month == 8 | month == 9, season := factor('summer')]
a <- runoff_5_rivers_2[,which(id != "6604800")]
runoff_5_rivers_2
runoff_4_rivers <- runoff_5_rivers_2[a,]
runoff_4_rivers
summer_runnof <- runoff_4_rivers[season == "summer" ]
summer_runnof <- summer_runnof[,sum(value), by = .(year,id)]
summer_runnof
winter_runnof <- runoff_4_rivers[season == "winter" ]
winter_runnof
winter_runnof <- winter_runnof[,sum(value), by = .(year,id)]
winter_runnof
summer_runnof$year <- as.numeric(factor((summer_runnof$year)))
winter_runnof$year <- as.numeric(factor((winter_runnof$year)))


ggplot(summer_runnof, aes(x = year, y = V1)) +
  geom_line(col = colset_4[3], aes(group = 1))+
  geom_point(col = colset_4[3])+
  facet_wrap(~id, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 3, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
ggplot(winter_runnof, aes(x = year, y = V1)) +
  geom_line(col = colset_4[3], aes(group = 1))+
  geom_point(col = colset_4[3])+
  facet_wrap(~id, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 3, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#=======================================================
info_for_project
summer_runnof
daily_runoff
daily_runoff <- daily_runoff[value >= 0]
unique(daily_runoff$id)
number_of_entries <- daily_runoff[, .N, by = id]
number_of_entries <- number_of_entries[N > 36500]
number_of_entries


daily_runoff$month <- as.numeric(factor((daily_runoff$month)))
daily_runoff[month == 1 | month == 2 | month == 3, season := factor('winter')]
daily_runoff[month == 7 | month == 8 | month == 9, season := factor('summer')]
all_summer_runoff <- daily_runoff[season == "summer" ]  
all_summer_runoff <- all_summer_runoff[,sum(value), by = .(year,id)]
all_summer_runoff
unique(all_summer_runoff$id)
ggplot(all_summer_runoff, aes(x = year, y = V1)) +
  geom_line(col = colset_4[3], aes(group = 1))+
  geom_point(col = colset_4[3])+
  facet_wrap(~id, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 3, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
daily_runnoff_descriptive_statistics
info_for_project
#high mean value, #high sd, #high max, #high long, #high altitude
# take 20 random samples(so assess similarity to other data)
#==============================

daily_runoff
daily_runoff <- daily_runoff[value >= 0]
unique(daily_runoff$id)
number_of_entries <- daily_runoff[, .N, by = id]
number_of_entries <- number_of_entries[N > 36500]
number_of_entries
analysis_of_means <- daily_runnoff_descriptive_statistics[, mean(mean), by = id]
analysis_of_means
daily_runnoff_descriptive_statistics
test <- daily_runnoff_descriptive_statistics[, mean(sd), by = id][order(-V1)][1:12,1]
test
test$id
info_for_project[ID %in% test$id]
info_for_project
daily_runoff[id == 6970250]
analysis_of_means
analysis_of_means[order(-V1)]
five_rivers <- data.table()
str(analysis_of_means[order(-V1)])
analysis_of_means[order(-V1)][10]
high_means <- analysis_of_means[order(-V1)][c(1:10),1]
(high_means)
high_means[1]
number_of_entries
a <- data.table()
for (i in 1:10) {
  print(high_means[i] %in% number_of_entries$id)
  print(i)
}
high_means[6]
five_rivers$id <- high_means[6]
daily_runnoff_descriptive_statistics
sd <- daily_runnoff_descriptive_statistics[, mean(sd), by = id]
sd[order((-V1))][1:10, 1][9]
for (i in 1:10) {
  print(sd[order((-V1))][1:10, 1][i] %in% number_of_entries$id)
  print(i)
}
merge(number_of_entries, sd[order((-V1))][1:20, 1])
five_rivers$id2 <- sd[order((-V1))][1:10, 1][9]
daily_runnoff_descriptive_statistics[, mean(max), by = id][order(-V1)]
for (i in 1:10) {
  print(daily_runnoff_descriptive_statistics[, mean(max), by = id][order(-V1)][1:10,][i] %in% number_of_entries$id)
  print(i)
}
daily_runnoff_descriptive_statistics[, mean(max), by = id][order(-V1)][5,]
five_rivers$id3 <- daily_runnoff_descriptive_statistics[, mean(max), by = id][order(-V1)][5,1]
info_for_project[,.(ID,Lat)][order(-Lat)][199:208,1]
for (i in 1:10) {
  print(info_for_project[,.(ID,Lat)][order(-Lat)][199:208,1][i] %in% number_of_entries$id)
  print(i)
}
info_for_project
five_rivers$id4 <- info_for_project[,.(ID,Lat)][order(-Lat)][201,1]
for (i in 1:10) {
  print(info_for_project[,.(ID,Lat)][order(-Lat)][199:208,1][i] %in% number_of_entries$id)
  print(i)
}
info_for_project[,.(ID,Alt)][order(-Alt)][1:10,1]
for (i in 1:10) {
  print(info_for_project[,.(ID,Alt)][order(-Alt)][1:10,1][i] %in% number_of_entries$id)
  print(i)
}
five_rivers$id5 <- info_for_project[,.(ID,Alt)][order(-Alt)][5,1]
  info_for_project[,.(ID,Alt)][order(-Alt)][10,1]
five_rivers
daily_runoff[id %in% five_rivers]
info_for_project

chosen_stations <- info_for_project[ID %in% five_rivers]
chosen_stations

position_in_annual_runoff <- daily_runoff[id == chosen_stations$ID[1] | id == chosen_stations$ID[2] | id == chosen_stations$ID[3] | id == chosen_stations$ID[4] | id == chosen_stations$ID[5]]
position_in_annual_runoff

runoff_5_rivers <- runoff_5_rivers[value >= 0]
monthly_average_discharge <- position_in_annual_runoff[,mean(value) ,by = .(id,month,year)]
monthly_average_discharge
ggplot(monthly_average_discharge, aes(x = factor(month), y = V1, group = month)) +
  geom_boxplot() +
  facet_wrap(~id, scales = 'free') +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
runoff_5_rivers
yearly_annual_discharge_5_rivers <- position_in_annual_runoff[, sum(value), by = .(id,year)]
yearly_annual_discharge_5_rivers
runnoff_year_mat <- dcast(yearly_annual_discharge_5_rivers, year~id)
runnoff_year_mat[97:193, ]
runnoff_year_mat <- runnoff_year_mat[97:193,]
runnoff_year_mat
runoff_year_cor <- cor(runnoff_year_mat[, -1], use = "pairwise.complete.obs")
runoff_year_cor <- round(runoff_year_cor,2)
to_plot <- melt(runoff_year_cor)
to_plot


col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")
corrplot(runoff_year_cor, order = "hclust", addrect = 2, col = col4(10), method = "number")
position_in_annual_runoff
#runoff_5_rivers <- runoff_5_rivers[,-6]
#runoff_5_rivers
runoff_5_rivers_2 <- position_in_annual_runoff
#runoff_5_rivers_2
runoff_5_rivers_2
runoff_5_rivers_2$month <- as.numeric(factor((runoff_5_rivers_2$month)))
runoff_5_rivers_2
runoff_5_rivers_2[month == 1 | month == 2 | month == 3, season := factor('winter')]
runoff_5_rivers_2[month == 7 | month == 8 | month == 9, season := factor('summer')]
runoff_5_rivers_2

summer_runnof <- runoff_5_rivers_2[season == "summer" ]
summer_runnof <- summer_runnof[,sum(value), by = .(year,id)]
summer_runnof
winter_runnof <- runoff_5_rivers_2[season == "winter" ]
winter_runnof
winter_runnof <- winter_runnof[,sum(value), by = .(year,id)]
winter_runnof
summer_runnof$year <- as.numeric(factor((summer_runnof$year)))
winter_runnof$year <- as.numeric(factor((winter_runnof$year)))


ggplot(summer_runnof, aes(x = year, y = V1)) +
  geom_line(col = colset_4[3], aes(group = 1))+
  geom_point(col = colset_4[3])+
  facet_wrap(~id, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 3, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
ggplot(winter_runnof, aes(x = year, y = V1)) +
  geom_line(col = colset_4[3], aes(group = 1))+
  geom_point(col = colset_4[3])+
  facet_wrap(~id, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 3, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
number_of_entries$id
all_data_100plusyears <- daily_runoff[id %in% number_of_entries$id]
daily_runnoff_descriptive_statistics
factor(all_data_100plusyears$month)
all_data_100plusyears$month <- as.numeric(factor(all_data_100plusyears$month))
all_data_100plusyears[month == 01 | month == 02 | month == 03, season := factor('winter')]
all_data_100plusyears[month == 07 | month == 08 | month == 09, season := factor('summer')]
all_data_100plusyears[month == 04 | month == 05 | month == 06, season := factor('spring')]
all_data_100plusyears[month == 10 | month == 11 | month == 12, season := factor('autumn')]
all_data_100plusyears[, value_norm := scale(value), by = .(id, year, season)]
yearly_annual_data <- all_data_100plusyears[, sum(value), by = .(id,season,year)]
yearly_annual_data[, value_norm := scale(V1), by = .(id,season)]
yearly_annual_data
all_data_100plusyears[, mean(value), by = .(id,year,season)]
summer <- yearly_annual_data[season == "summer"]
summer
n_stations <- unique(summer$id)

summer_test <- summer[id %in% n_stations[1:20]]
summer_test
low_altittude <- (info_for_project[order(-Alt)])
low_alt_stations <- unique(low_altittude[120:165,]$ID)
test <- unique(summer[id %in% low_alt_stations]$id)
summer_test <- summer[id %in% test]
summer_test
unique(summer_test$id)
ggplot(summer_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
summer_test[year == "2009"][value_norm > 0.2]$id
info_for_project[ID %in% summer_test[year == "2009"][value_norm > 0.2]$id] #sugests high lattidude experiance differencet conditions
info_for_project[ID %in% summer_test[year == "2009"][value_norm > 0]$id]
low_altittude <- (info_for_project[order(-Alt)])
high_alt_stations <- unique(low_altittude[1:50,]$ID)
test <- unique(summer[id %in% high_alt_stations]$id)
summer_test <- summer[id %in% test]
summer_test
unique(summer_test$id)
ggplot(summer_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

low_lat <- (info_for_project[order(-Lat)])
low_lat_stations <- unique(low_lat[170:208,]$ID)
test <- unique(summer[id %in% low_lat_stations]$id)
test
summer_test <- summer[id %in% test]
summer_test
ggplot(summer_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
low_lat <- (info_for_project[order(-Lat)])
high_lat_stations <- unique(low_lat[1:100,]$ID)
test <- unique(summer[id %in% high_lat_stations]$id)
test
summer_test <- summer[id %in% test]
summer_test
ggplot(summer_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
winter <- yearly_annual_data[season == "winter"]
final_year <- summer_test[year == 2009]
str(final_year$value_norm)
final_year[value_norm >= 0.2, level := factor('high')]
final_year[value_norm <= -0.2, level := factor('low')]
final_year[value_norm < 0.2 && value_norm > -0.2, level := factor('medium')]
final_year[level == "high"]$id
info_for_project
info_for_project[ID %in% final_year[level == "high"]$id]
info_for_project[ID %in% final_year[level == "medium"]$id]
info_for_project[ID %in% final_year[level == "low"]$id]

n_stations_winter <- unique(winter$id)

winter_test <- winter[id %in% n_stations[1:20]]

low_altittude <- (info_for_project[order(-Alt)])
low_alt_stations <- unique(low_altittude[120:165,]$ID)
test <- unique(winter[id %in% low_alt_stations]$id)
winter_test <- winter[id %in% test]
ggplot(winter_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

n_stations_winter <- unique(winter$id)

winter_test <- winter[id %in% n_stations[1:20]]

low_altittude <- (info_for_project[order(-Alt)])
high_alt_stations <- unique(low_altittude[1:50,]$ID)
test <- unique(winter[id %in% high_alt_stations]$id)
winter_test <- winter[id %in% test]
ggplot(winter_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
###########
low_long <- (info_for_project[order(-Lon)])
low_long
high_long_stations <- unique(low_lat[1:50,]$ID)
test <- unique(summer[id %in% high_long_stations]$id)
test
summer_test <- summer[id %in% test]
summer_test
ggplot(summer_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
low_long <- (info_for_project[order(-Lon)])
low_long
low_long_stations <- unique(low_lat[160:208,]$ID)
test <- unique(summer[id %in% low_long_stations]$id)
test
summer_test <- summer[id %in% test]
summer_test
ggplot(summer_test[year > 1910 | year < 2000], aes(x = year, y = value_norm, col = id, group = id)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
?scale_color_manual
colorRampPalette(colset_4)
?geom_smooth
info_for_project
daily_runnoff_descriptive_statistics
unique(summer$id)
winter
winter2 <- winter[year > 1950]
winter2
summer2 <- summer[year >1950]
summer2
year_thres <- 1980
to_plot <- rbind(cbind(winter2, season = factor('winter')), 
                 cbind(summer2, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot
to_plot <- to_plot[year >= 1950]
to_plot <- to_plot[id %in% unique(to_plot$id)]
to_plot
colnames(to_plot) <- make.unique(names(to_plot))

ggplot(to_plot, aes(season.1, V1, fill = period)) +
  geom_boxplot() +
  facet_wrap(~id, scales = 'free_y') +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()


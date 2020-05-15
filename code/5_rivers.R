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
info_for_project$River
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
data(mtcars)
corr <- round(cor(mtcars), 1)
corr
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

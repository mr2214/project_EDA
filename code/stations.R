install.packages(c("leaflet", "sp"))
library(data.table)
library(sp)
library(leaflet)
library(ggplot2)
getwd()
info_for_project <- readRDS(file = './data/Data for final project-20200428/runoff_eu_info.rds')
df <- data.frame(longitude = info_for_project$Lon, 
                 latitude = info_for_project$Lat)

coordinates(df) <- ~longitude+latitude
leaflet(df) %>% addMarkers() %>% addTiles()
#stations are located in europe, particularly dense in mainland western europe and scandinavia, there are some stations sparely distributed in Russia and eastern europe, and the British Isles and Iceland.
info_for_project[, .N, by = Country]
info_for_project[, .N, by = .(River,Country)][, .N, by = Country]
info_for_project[, .N, by = River]
info_for_project[,0]
lon_lat_alt <- ggplot(data = info_for_project, aes(x = lat, y = lon, size = Alt)) +
  geom_point()
lon_lat_alt
lon_lat_alt + coord_cartesian(xlim = c(45, 55), ylim = c(0, 20))
ggplot(data = info_for_project, aes(x = 1:208, y = N.Years)) +
   geom_point()

theme_set(theme_bw())

# Draw plot
ggplot(info_for_project, aes(x = 1:208, y = N.Years)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Number of years of observation", 
       subtitle="distribution of recorder data", 
       caption="source: https://www.bafg.de/GRDC/EN/01_GRDC/grdc_node.html") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
# with a few exceptions most stations have around 100 years of observations
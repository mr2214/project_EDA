install.packages(c("leaflet", "sp"))
library(data.table)
library(sp)
library(leaflet)
library(ggplot2)
getwd()
info_for_project <- readRDS(file = './data/Data for final project-20200428/runoff_eu_info.rds')
df <- data.frame(longitude = info_for_project$Lon, 
                 latitude = info_for_project$Lat)

info_for_project
coordinates(df) <- ~longitude+latitude
leaflet(df) %>% addMarkers(lng = info_for_project$Lon, lat = info_for_project$Lat, popup = paste("Region", info_for_project$Continent, "<br>",
                           "river:", info_for_project$River, "<br>",
                           "country:", info_for_project$Country, "<br>",
                           "altitude", info_for_project$Alt)) %>% addTiles()
#stations are located in europe, particularly dense in mainland western europe and scandinavia, there are some stations sparely distributed in Russia and eastern europe, and the British Isles and Iceland.
info_for_project[, .N, by = Country] #number of stations per country
info_for_project[, .N, by = .(River,Country)][, .N, by = Country] #number of rivers in each country
info_for_project[, .N, by = River] # number of stations on each river
info_for_project[,0]
lon_lat_alt <- ggplot(data = info_for_project, aes(x = Lat, y = Lon, size = Alt)) +
  geom_point()
lon_lat_alt
lon_lat_alt + coord_cartesian(xlim = c(45, 55), ylim = c(0, 20)) #zooming into highest density of the plot
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
# with a few exceptions most stations have around 90to 100 years of observations

# https://rstudio.github.io/leaflet/json.html
# https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d
# https://rstudio.github.io/leaflet/markers.html
# http://data-communities.opendata.arcgis.com/datasets/indices-of-multiple-deprivation-imd-2019-1
# https://rpubs.com/rural_gis/259078

library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(lawn)
library(leaflet)
library(rjson)
library(plotly)
library(rgdal)
library(raster)
library(sf)
library(dplyr)
library(tidyverse)

### Load deprivation statistics
IMDstats <- read_csv("C:/Users/jpayne-gill/Documents/R Analyses/Mapped/Indices_of_Multiple_Deprivation_IMD_2019.csv", col_names = TRUE)

### Load lsoa London data
london1 <- sf::st_read("england_lsoa_2011.shp", quiet = TRUE)

london2 <- sf::st_read("london_ward/LSOA_2011_London_gen_MHW.shp", quiet = TRUE)

ohsel <- sf::st_read("london_ward/London_Borough_Excluding_MHW.shp", quiet = TRUE) %>%
  filter(NAME %in% c("Bexley", "Bromley", "Greenwich", "Lambeth", "Lewisham", "Southwark"))
  
ohsel <- st_transform(ohsel, "+proj=longlat +datum=WGS84")


### Merge lsoa regions with information on deprivation scores
lslc <- london2 %>%
  left_join(dplyr::select(IMDstats, lsoa11cd, IMDScore, IMDRank0, IMDDec0), by = c("LSOA11CD" = "lsoa11cd"))

### Transform coordinate system
lslc <- st_transform(lslc, "+proj=longlat +datum=WGS84")

### Write the file
geojson_write(lslc, file = "C:/Users/jpayne-gill/Documents/R Analyses/Mapped/test.geojson")
testimport <- rgdal::readOGR("C:/Users/jpayne-gill/Documents/R Analyses/Mapped/test.geojson")



### Define colour palette
pal <- colorNumeric("viridis", NULL)

leaflet(lslc) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              color = 'black',
              opacity = 0.5,
              smoothFactor = 0.3, 
              fillOpacity = 0.5,
              fillColor = ~pal(IMDRank0)) %>%
  addLegend(pal = pal, values = ~IMDRank0, opacity = 1.0) %>%
  addCircleMarkers(lng = allpc$longitude, 
                   lat = allpc$latitude,
                   color = 'red',
                   fillColor = 'red',
                   opacity = 1,
                   fillOpacity = 1,
                   radius = 1) %>%
  setView(lng = -0.066, lat = 51.4, zoom = 11.3)




leaflet(lslc,
        options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              color = 'black',
              opacity = 0.7,
              smoothFactor = 0.3, 
              fillOpacity = 0.7,
              fillColor = ~pal(IMDRank0)) %>%
  addPolylines(data=ohsel,
              weight = 3,
              color = 'red',
              opacity = 1,
              smoothFactor = 0.3) %>%
  addLegend(pal = pal, values = ~IMDRank0, opacity = 1.0) %>%
  setView(lng = -0.066, lat = 51.4, zoom = 11.3)



leaflet() %>%
  addTiles(data=lslc) %>%
  addPolygons(data=lslc,
              stroke = TRUE, 
              weight = 0.5,
              color = 'black',
              opacity = 0.5,
              smoothFactor = 0.3, 
              fillOpacity = 0.5,
              fillColor = ~pal(IMDRank0)) %>%
  addPolygons() %>%
  addLegend(pal = pal, values = ~IMDRank0, opacity = 1.0) %>%
  setView(lng = -0.066, lat = 51.4, zoom = 7.25)




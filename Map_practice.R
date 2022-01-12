# https://geocompr.robinlovelace.net/adv-map.html

library(sf)
library(raster)
library(dplyr)
library(spData)
#library(spDataLarge) # not available in this version of R
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package


tm_shape(world[nz]) +
  tm_fill() 




library(maps)


# https://towardsdatascience.com/visualising-crime-in-london-using-r-part-i-de7853c92ba8
library(rgdal)
library(ggplot2)
london_boroughs <- readOGR(dsn = "Map_data/LondonBoroughs.shp") 
head(london_boroughs@data,2)

#passes the spatial data as a data.frame rather than a spatial object
london_boroughs_f <- fortify(london_boroughs)

# allocate an id variable to the sp data
london_boroughs$id <- row.names(london_boroughs)

# joins the data
london_boroughs_f <- left_join(london_boroughs_f, london_boroughs@data) 

head(london_boroughs_f,5)

#plots the boroughs 
ggplot(london_boroughs_f, aes(long, lat, group = group)) +
  geom_polygon() + geom_path(colour="white", lwd=0.05) + coord_equal() +
  labs(x = "lat", y = "lon") +
  ggtitle("London boroughs") #+
  #theme(axis.text = element_blank(), # change the theme options
  #      axis.title = element_blank(), # remove axis titles
  #      axis.ticks = element_blank()) # remove axis ticks






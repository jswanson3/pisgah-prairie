## This script creates a map of the sampling locations across 10 sites 
## within Willamette Valley

library(terra)
library(ggplot2)
library(dplyr)
library(sf)
library(ggspatial)
library(tidyverse)

setwd("C:/pisgah-prairie")

raw.data <- read_csv('data/raw/PPP_PollObs_OR_upto2022_NEW_IDs_20240108 (2).csv')

ecoRegion <- ORecoregion <- st_read(
  'data/spatial/OR-ecoregions/Ecoregions_OregonConservationStrategy.shp')

#filter lat and long
latlong <- raw.data %>% 
  dplyr::select(State, Country, site, Latitude, Longitude) %>% 
  na.omit()

unique(ecoRegion$Ecoregion)

#filter willamette ecoregion
willamette <- ecoRegion %>% 
  filter(Ecoregion == "Willamette Valley")

#create vector
sampLocations <- st_as_sf(latlong, coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

#align CRS
sampsLocations <- st_transform(sampLocations, st_crs(willamette))

#plot
spatialPlot <- ggplot2::ggplot() +
  #Ecoregion
  sf::geom_sf(data = willamette, fill = "grey85", color = "black") +
  #Sampling locations
  sf::geom_sf(data = sampsLocations, aes(color = site), size = 4) +
  annotation_scale(
    location = "bl",
    pad_x = unit(-0.5, "in"),   # move outside map
    pad_y = unit(0.2, "in")) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(-0.5, "in"),   # move outside map
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering) +
  theme_void() +
  coord_sf(clip = "off") +   #allows arrows to be in margin
  theme(
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Sampling Locations in Willamette Valley, Oregon") +
  scale_color_discrete(name = "Site")

# Save plot
ggplot2::ggsave(filename = "spatialPlot.png",
                plot = spatialPlot,
                path = "analysis/figures",      
                width = 10, height = 8, dpi = 300)
          
  

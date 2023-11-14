# 1. Setting up
# Loading all the following libraries else install.packages accordingly
library("GISTools")
library("raster")
library("tmap")
library("maptools")
library("raster")
library(raster)
library(sp)
library(sf)
library(tidyverse)
library(ggplot2)
library(psych)
library(spatstat)
library(spdep)
library(jsonlite)


# 2. Handling Data
# 2.1 Loading Raw Datafiles
# Base Map
base_map <- st_read(dsn = "dataset/basemap", layer = "MP14_SUBZONE_NO_SEA_PL")

# Population + HDB Data
data <- read.csv("dataset/filteredSG_population_density.csv")
hdb <- read.csv("dataset/hdb.csv") %>%
    mutate(addr = paste(blk_no, street)) %>%
    select(addr, lat, lng)
yth_data <- read.csv("dataset/filteredSG_population_density.csv") %>% select(longitude, latitude, youth)


# Nightlife (Bars + Clubs) Crowd Density Data
rawdata_bars <- fromJSON("dataset/besttime/bars.json", flatten = TRUE)
rawdata_clubs <- fromJSON("dataset/besttime/clubs.json", flatten = TRUE)
relevant_columns <- c("venue_lat", "venue_lon", "venue_name")
bars <- rawdata_bars$venues[relevant_columns]
clubs <- rawdata_clubs$venues[relevant_columns]


# Restaurant Data
restaurants <- read.csv("dataset/besttime/restaurants.csv") %>% select(name, lat, lon, midnight_restaurant, fri_cd, sat_cd)

# 2.2 Setting up DataFrame
# Population DataFrame:

# HDB DataFrame:

# Nightlife DataFrame: lat, lng, name of place, crowd density
nightlife <- rbind(bars, clubs)
new_column_names <- c("lat", "lng", "name")
nightlife <- setNames(nightlife, new_column_names)


# 3. Preprocessing Data to Vectors/Rasters
# 3.1 Nightlife Locations
# Create a spatial data frame with points
coordinates <- cbind(nightlife$lng, nightlife$lat)

# Convert the dataframe to a SpatialPointsDataFrame
nightlife_sp_data <- SpatialPointsDataFrame(coordinates, data = data.frame(nightlife), proj4string = CRS("+proj=longlat +datum=WGS84"))
# convert to sf
nightlife_sf_data <- st_as_sf(nightlife_sp_data)
nightlife_buffer <- st_buffer(nightlife_sf_data, dist = 2500)

# 3.2 Restaurants Locations
restaurant_coordinates <- cbind(restaurants$lon, restaurants$lat)

# Convert the dataframe to a SpatialPointsDataFrame
restaurant_sp_data <- SpatialPointsDataFrame(restaurant_coordinates, data = data.frame(restaurants), proj4string = CRS("+proj=longlat +datum=WGS84"))
# convert to sf
restaurant_sf_data <- st_as_sf(restaurant_sp_data)

# Convert crs of basemap to be the same as restaurant data
basemap <- st_transform(base_map, crs = st_crs(restaurant_sf_data))

# Correct invalid geometries
basemap <- st_make_valid(basemap)
restaurant_sf_data <- st_make_valid(restaurant_sf_data)

# Look for intersections between restaurant and regional polygons and filter polygons with restaurants
intersections <- st_intersects(basemap, restaurant_sf_data)
basemap_with_restaurants <- basemap[apply(intersections, 1, any), ]

# Filter for nightlife restaurants that exist in the polygons
within_results <- st_within(nightlife_sf_data, basemap_with_restaurants)
matches_count <- apply(within_results, 1, function(x) sum(x) > 0)
nightlife_filtered_sf <- nightlife_sf_data[matches_count, ]

#
tm_shape(basemap_with_restaurants) + tm_borders() +
    tm_basemap("OpenStreetMap") +
    tm_shape(nightlife_filtered_sf) + tm_bubbles(size = 0.1, col = "red") +
    tm_shape(restaurant_sf_data) + tm_bubbles(size = 0.04, col = "midnight_restaurant", palette = c("yellow", "blue"))

# tm_compass(type = "8star", size = 2) +
# tm_scale_bar(width = 0.15) +
# tm_layout(
#   legend.format = list(digits = 0),
#   legend.position = c("left", "bottom"),
#   legend.text.size = 0.25,
#   legend.title.size = 0.5,
#   title = "Nightlife location in Singapore",
#   title.position = c("left", "bottom")
# )

st_crs(nightlife_filtered_sf)
nightlife_ppp <- as.ppp(nightlife_filtered_sf)
nn_analysis <- nncross(nightlife_ppp)
plot(nn_analysis, main = "Nearest Neighbor Analysis")

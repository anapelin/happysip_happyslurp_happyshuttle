---
  title: "BT4015 Project Group 6"
author: "Chan Tse Ee (A0221197H)"
date: "24 October 2023"
output:
  html_document: default
word_document: default
pdf_document: default
---
  
# 1. Setting up 
# Loading all the following libraries else install.packages accordingly
  # install.packages(c("raster", "sp"))
  # install.packages("psych")
library(GISTools)
library(raster)
library(tmap)
library(rgdal) 
library(maptools) 
library(sp)
library(sf)
library(tidyverse)
library(rgeos)
library(ggplot2)
library(psych)
library(spatstat)
library(spdep)
library(jsonlite)
library(dplyr)

# 2. Loading Raw Datafiles as DataFrame

# 2.1 Basemap - Singapore Subzones (Polygons Vectors)
subzones <- st_read(dsn = "dataset/basemap", layer = "MP14_SUBZONE_NO_SEA_PL")

# 2.2 Nightlife (Bars + Clubs) Data (Points Vectors)
rawdata_bars <- fromJSON("dataset/besttime/bars.json", flatten=TRUE)
rawdata_clubs <- fromJSON("dataset/besttime/clubs.json", flatten=TRUE)
relevant_columns <- c("venue_lat", "venue_lon", "venue_name")
bars <- rawdata_bars$venues[relevant_columns]
clubs <- rawdata_clubs$venues[relevant_columns]
# Nightlife DataFrame: lat, lng, name of place, crowd density
nightlife <- rbind(bars, clubs)
new_column_names <- c("lat", "lng", "name")
nightlife <- setNames(nightlife, new_column_names)
nightlife_coordinates <- cbind(nightlife$lng, nightlife$lat) 

# 2.3 Midnight Restaurants Crowd Density Data
rawdata_restaurants <- read.csv("dataset/besttime/restaurants.csv")
# Restaurant DataFrame: lat, lng, name of restaurant, is_midnight, crowd density on fri and sat
restaurants <- rawdata_restaurants %>% dplyr::select(name, lat, lon, midnight_restaurant, fri_cd, sat_cd)
restaurants_coordinates <- cbind(restaurants$lon, restaurants$lat)

# 2.4 Population - Youth Data (Points Vectors)
population <- read.csv("dataset/filteredSG_population_density.csv")
yth_data <- read.csv("dataset/filteredSG_population_density.csv") %>% dplyr::select(longitude, latitude, youth)
coordinates(population) <- ~longitude+latitude

# 2.5 HDB Data (Points Vectors)
hdb <- read.csv("dataset/hdb.csv") %>% dplyr::mutate(addr = paste(blk_no,street)) %>% dplyr::select(addr,lat,lng)

# 2.6 Bus Stops Data (Points Vectors)
bus_data <- data.frame()
for(i in 0:10) {
  file_name <- paste0("dataset/busstops/response(", i, ").json")
  # Handle the case for response.json (without the index)
  if(i == 0) {
    file_name <- "dataset/busstops/response.json"
  }
  json_read <- fromJSON(file_name, flatten = TRUE)
  bus_data <- rbind(bus_data, json_read$value)
}


# 3. Preprocessing Data to DataFrames/Vectors/Rasters
# 3.1 Nightlife Locations

# Create a Nightlife Spatial Points DataFrame
nightlife_sp_data <- SpatialPointsDataFrame(nightlife_coordinates, data = data.frame(nightlife), proj4string = CRS("+proj=longlat +datum=WGS84"))
nightlife_sf <- st_as_sf(nightlife_sp_data) 
nightlife_sf <- st_transform(nightlife_sf, crs=st_crs(subzone)) # project to basemap's CRS

# Convert to Nightlife Point Pattern dataset
nightlife_ppp <- as.ppp(nightlife_sf)

# Combine with subzones for Nightlife Density DataFrame
nightlife_subzone <- st_join(subzones, nightlife_sf)
nightlife_density_subzone <- nightlife_subzone %>%
  group_by(SUBZONE_N) %>%
  summarize(bar_density = sum(!is.na(name)))

# 3.2 Midnight Restaurants

# Create a Restaurants Spatial Points DataFrame
restaurants_sp_data <- SpatialPointsDataFrame(restaurants_coordinates, data = data.frame(restaurants), proj4string = CRS("+proj=longlat +datum=WGS84"))
restaurants_sf <- st_as_sf(restaurants_sp_data)
restaurants_sf <- st_transform(restaurants_sf, crs = st_crs(subzones)) # project to basemap's CRS

# Convert to Restaurants Point Pattern dataset
restaurants_ppp <- as.ppp(restaurants_sf)

# Combine with subzones for Restaurants Density DataFrame
restaurants_subzone <- st_join(subzones, restaurants_sf)
restaurants_density_subzone <- restaurants_subzone %>%
  group_by(SUBZONE_N) %>%
  summarize(restaurants_density = sum(!is.na(name)))

# 3.3 Population - Youth Density

# Create a Population Raster Dataset
r <- raster(extent(population), resolution=c(0.001, 0.001)) # Here 0.001 is an example resolution. Adjust as necessary.
under5_raster <- rasterize(population, r, field="under5", fun=mean)

plot(under5_raster)

# Convert to Population Point Pattern dataset
ppp_data <- as.ppp(population)
plot(ppp_data)
# Create a Youth Spatial Polygon DataFrame
yth_sf <- st_as_sf(yth_data, coords = c("longitude", "latitude"), crs = 4326)
yth_sf <- st_transform(yth_sf, crs = st_crs(subzones)) # project to basemap's CRS

# 3.4 HDB


# 3.5 Bus Stops
bus_sf <- st_as_sf(bus_data, coords = c("Longitude", "Latitude"), crs = 4326)
bus_sf <- st_transform(bus_sf, crs = st_crs(subzones)) # project to basemap's CRS


# 4. Basic Spatial Operations

# Map 1: View of All Distributions (with toggles for layers)
tmap_mode('view')

tm_shape(subzones) + tm_borders() + tm_basemap('OpenStreetMap') + 
  tm_shape(nightlife_sf) + tm_dots(size = 0.0075, col = "red") + # Nightlife locations
  tm_shape(restaurant_sf) + tm_bubbles(size = 0.04, col = "midnight_restaurant", palette = c("yellow", "blue")) + # Restaurants
  tm_shape()
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="Nightlife location in Singapore",
            title.position = c('left', 'bottom'))



# Compute mean center
mean_center <- with(ppp_data, c(mean(x), mean(y)))
print(mean_center)

# Compute weighted mean center for 'under5' column
weights <- data$under5
weighted_mean_center <- with(ppp_data, c(weighted.mean(x, w=weights), weighted.mean(y, w=weights)))
print(weighted_mean_center)
plot(weighted_mean_center)

# Quadrat Test
qtest <- quadrat.test(ppp_data)
print(qtest)
plot(qtest)

# K-function
Kest <- envelope(ppp_data, fun=Kest)
plot(Kest)

#Frequency of all column values
table(data$under5)
table(data$above60)
table(data$men)
table(data$women)
table(data$repro)
table(data$youth)
table(data$gen)

#dispersion standard distance deviation #TIME TAKEN is about 15min
sdd <- sqrt(with(ppp_data, sum((x - mean_center[1])^2 + (y - mean_center[2])^2) / length(x)))
print(sdd)



# Group points with the same value and create convex hulls
grouped_points <- yth_sf %>%
  group_by(youth) %>%
  summarize()
yth_poly <- grouped_points %>%
  st_convex_hull()
yth_poly <- yth_poly[-14,] #remove the individual point which is out of place #or horh jeremy never draw ur boudnaries properly


# Plot or analyze the resulting polygons
plot(yth_poly) #polygon map of youth density
class(yth_poly)


#HDB points plotting
hdb_spatial <- st_as_sf(hdb, coords = c("lng", "lat"), crs = 4326)
class(hdb_spatial)
joined <- st_join(hdb_spatial,yth_poly)
plot(joined)

#I tried to plot both points and poly tgthr, looks OK 
ggplot() +
  geom_sf(data = yth_poly, aes(fill = youth), color = "black") +
  geom_point(data = joined, aes(x = st_coordinates(joined)[,1], y = st_coordinates(joined)[,2], color = youth), size = 1) +
  scale_color_gradient(name = 'HDB locations and density', low = "yellow", high = "red") + 
  scale_fill_gradient(name = 'Density polys', low = "blue", high = "green") +
  labs(title = "Overlay of Polygon and Point Plot") +  theme_void()

#This plot is to show how some HDB lies outside the given density polys.
#interesting to note that condos are not represented, shall we exclude them from analysis?

#Handling NAs and duplicates
yth_mean <- mean(joined$youth, na.rm = T)
joined <- joined %>% mutate(youth = ifelse(is.na(youth), yth_mean, youth)) #replace NAs with mean
joined <- joined %>% group_by(addr) %>% summarise(addr = addr, youth = mean(youth), geometry = geometry) #for rows with multiple densities, replace with mean of duplicates.
joined <- unique(joined)


#Plot with handled dataset
ggplot() +
  geom_sf(data = yth_poly, aes(fill = youth), color = "black") +
  geom_point(data = joined, aes(x = st_coordinates(joined)[,1], y = st_coordinates(joined)[,2], color = youth), size = 1) +
  scale_color_gradient(name = 'HDB locations and density', low = "yellow", high = "red") + 
  scale_fill_gradient(name = 'Density polys', low = "blue", high = "green") +
  labs(title = "Overlay of Polygon and Point Plot") +  theme_void()




# Bus Stop

  
#Plot overlay of polygon and point plot for bus stops with HDB points
ggplot() +
  geom_sf(data = yth_poly, aes(fill = youth), color = "black") +
  geom_point(data = joined, aes(x = st_coordinates(joined)[,1], y = st_coordinates(joined)[,2], color = youth), size = 1) +
  geom_point(data = bus_sf, aes(x = st_coordinates(bus_sf)[,1], y = st_coordinates(bus_sf)[,2]), size = 1) +
  scale_color_gradient(name = 'HDB locations and density', low = "yellow", high = "red") + 
  scale_fill_gradient(name = 'Density polys', low = "blue", high = "green") +
  labs(title = "Overlay of Polygon and Point Plot") +  theme_void()



# K-nearest neighbors (KNN) method: # Perform Moran's I test
w <- knn2nb(knearneigh(coordinates(data), k = 5))  # Adjust 'k' as needed
w_listw <- nb2listw(w)

#*W output
#*Neighbour list object:
#*Number of regions: 217054  
#*Number of nonzero links: 1085270 
#*Percentage nonzero weights: 0.002303574 
#*Average number of links: 5 


moran_test <- moran.test(x = data$youth, listw = w_listw)
print(moran_test)
#* output is p-value of < 2.2e-16
#* Moran I statistic       Expectation          Variance 
#* 9.974184e-01     -4.607170e-06      1.693194e-06


#Kernel Density estimation
ppp_data <- ppp(
  data$longitude,
  data$latitude,
  window = owin(range(data$longitude), range(data$latitude))
)

# Perform kernel density estimation
kde <- density(ppp_data)
plot(kde)


#Lets try here
#plot out all the points for youth density

yth_sf <- st_as_sf(yth_data, coords = c("longitude", "latitude"), crs = 4326)
yth_sf <- yth_sf[-14]

tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(yth_sf) + tm_bubbles(size = "youth", scale = 0.5)  +
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="Nightlife location in Singapore",
            title.position = c('left', 'bottom'))







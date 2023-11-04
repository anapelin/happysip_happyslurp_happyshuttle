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
library('GISTools')
library('raster')
library('tmap')
library('rgdal') 
library('maptools') 
library('raster') 
library(raster)
library(sp)
library(sf)
library(tidyverse)
library(rgeos)
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
hdb <- read.csv("dataset/hdb.csv") %>% mutate(addr = paste(blk_no,street)) %>% select(addr,lat,lng)
yth_data <- read.csv("dataset/filteredSG_population_density.csv") %>% select(longitude, latitude, youth)

# Read & Load Bus Stop Data
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
bus_spatial <- st_as_sf(bus_data, coords = c("Longitude", "Latitude"), crs = 4326)


# Nightlife (Bars + Clubs) Crowd Density Data
rawdata_bars <- fromJSON("dataset/besttime/bars.json", flatten=TRUE)
rawdata_clubs <- fromJSON("dataset/besttime/clubs.json", flatten=TRUE)
relevant_columns <- c("venue_lat", "venue_lon", "venue_name")
bars <- rawdata_bars$venues[relevant_columns]
clubs <- rawdata_clubs$venues[relevant_columns]


# Restaurant Data

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


tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(nightlife_sf_data) + tm_bubbles(size = 0.1, col = "red") +
  tm_shape(nightlife_buffer) + tm_polygons() +
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="Nightlife location in Singapore",
            title.position = c('left', 'bottom'))


coordinates(data) <- ~longitude+latitude

# Define the extent of the raster based on the spatial object
raster_extent <- extent(data)

# Create an empty raster
r <- raster(raster_extent, resolution=c(0.001, 0.001)) # Here 0.001 is an example resolution. Adjust as necessary.

#Rasterise the data
under5_raster <- rasterize(data, r, field="under5", fun=mean)

plot(under5_raster)

ppp_data <- as.ppp(data)
plot(ppp_data)

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

#Explore Point of Interest (POI) data
colnames(poi)
bar_club <- poi %>% filter(bar == "True" | night_club == "True")
bar_club %>% select(name,lat,lng)

#Youth polygon
yth_spatial <- st_as_sf(yth_data, coords = c("longitude", "latitude"), crs = 4326)

# Group points with the same value and create convex hulls
grouped_points <- yth_spatial %>%
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



  
#Plot overlay of polygon and point plot for bus stops with HDB points
ggplot() +
  geom_sf(data = yth_poly, aes(fill = youth), color = "black") +
  geom_point(data = joined, aes(x = st_coordinates(joined)[,1], y = st_coordinates(joined)[,2], color = youth), size = 1) +
  geom_point(data = bus_spatial, aes(x = st_coordinates(bus_spatial)[,1], y = st_coordinates(bus_spatial)[,2]), size = 1) +
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

#HDB with youth polygons
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(yth_poly) + tm_polygons(col = 'youth') + 
  tm_shape(hdb_spatial) + tm_bubbles(size = 0.1, scale = 0.5, col = 'black')  +
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="HDB location in Singapore",
            title.position = c('left', 'bottom'))

#KDE plot

hdb_points <- as(joined, "Spatial")
hdb_centers <- kde.points(hdb_points, h = 0.01) 
hdb_centers_sf <- st_as_sf(hdb_centers)

plot(hdb_centers) 
tm_shape(hdb_centers) + tm_raster()

#Reclassify values in raster to make contour lines as seen in KDE plot.
reclass_values <- c(0,50,1, #reclassify kde values from 0-50 in group 1 and so on
                    50,100,2,
                    100,150,3,
                    150,200,4,
                    200,250,5,
                    250,300,6)

reclass_hdb_centers <- reclassify(as(hdb_centers, "RasterLayer"), reclass_values) #reclassify kde values to groups
hdb_centers_poly <- rasterToPolygons(reclass_hdb_centers, dissolve = T) #to make a polygon layer
hdb_centers_poly <- st_as_sf(hdb_centers_poly) #to make an SF object
hdb_centers_poly <- hdb_centers_poly[-c(1,2),] #remove polys with low kde values 
hdb_centers_poly <- st_cast(hdb_centers_poly,'POLYGON') #to split multipolygon to polygon to obtain centers

#map to show how the kde looks like with all hdbs
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(joined) + tm_bubbles(size = 0.05, scale = 0.5, col = 'youth')  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="HDB location in Singapore",
            title.position = c('left', 'top'))

#Map to show how bustops fare with kdes
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(bus_spatial) + tm_bubbles(size = 0.15, scale = 0.5, col = 'black')  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Busstop location in Singapore \n with KDE of HDBs",
            title.size = 1,
            title.position = c('left', 'top'))

#remove busstops which are not inside the polygons
bus_inside_spatial <- st_intersection(bus_spatial, hdb_centers_poly)

#Show busstops which are inside the KDEs
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(bus_inside_spatial) + tm_bubbles(size = 0.15, scale = 0.5, col = 'black')  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Busstop location in Singapore \n with KDE of HDBs",
            title.size = 1,
            title.position = c('left', 'top'))


#get the centroids of each polygon
hdb_centers_points <- st_centroid(hdb_centers_poly)
#find closest busstop to each centroid
closest_busstops_index <- st_nearest_feature(hdb_centers_points, bus_inside_spatial)
closest_busstops <- bus_inside_spatial[closest_busstops_index,]

#busstops 
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(closest_busstops) + tm_bubbles(size = 0.2, scale = 0.5, col = 'black')  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Central busstop location in Singapore \n with KDE of HDBs",
            title.size = 1,
            title.position = c('left', 'top'))

#create buffers for each centroid
closest_busstops_buffer <- st_buffer(closest_busstops, dist = 1500) #1km buffer
closest_busstops_buffer <- st_union(closest_busstops_buffer) #join the buffers together
closest_busstops_buffer <- st_cast(closest_busstops_buffer,'POLYGON') #join the buffers together
closest_busstops_buffer <- st_make_valid(closest_busstops_buffer)

tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(closest_busstops_buffer) + tm_polygons() + 
  tm_shape(hdb_spatial) + tm_bubbles(size = 0.15, scale = 0.5, col = 'black')  +
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Central busstop buffers in\n Singapore  with  HDBs",
            title.size = 1,
            title.position = c('left', 'top'))

num_hdb_captured <- nrow(st_intersection(hdb_spatial, closest_busstops_buffer))
percentage_captured <- num_hdb_captured / nrow(hdb_spatial)
print(percentage_captured)
#1km radius: 68% HDB captured
#1.5km radius: 87% HDB captured
#2km radius: 94% HDB captured


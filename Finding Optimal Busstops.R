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
library(gstat)


# 2. Handling Data
# 2.1 Loading Raw Datafiles
# Base Map
subzones <- st_read(dsn = "dataset/basemap", layer = "MP14_SUBZONE_NO_SEA_PL")
# base_map <- st_read(dsn = "dataset/basemap", layer = "SGP_adm0")
# base_map <- base_map %>% select(ISO)
# base_map <- st_transform(base_map, crs=st_crs(subzones))
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


# # Nightlife (Bars + Clubs) Crowd Density Data

 
# # Nightlife DataFrame: lat, lng, name of place, crowd density

nightlife <- st_read('dataset/processed_datasets/nightlife_locations.geojson', crs = st_crs(subzones))

# Restaurant Data

# 2.2 Setting up DataFrame
# Population DataFrame:

# HDB DataFrame: 



# 3. Preprocessing Data to Vectors/Rasters
# 3.1 Nightlife Locations
# Create a spatial data frame with points
#coordinates <- cbind(nightlife$lng, nightlife$lat) 

# Convert the dataframe to a SpatialPointsDataFrame
#nightlife_sp_data <- SpatialPointsDataFrame(coordinates, data = data.frame(nightlife), proj4string = CRS("+proj=longlat +datum=WGS84"))
# convert to sf 
#nightlife_sf_data <- st_as_sf(nightlife_sp_data) 
#nightlife_buffer <- st_buffer(nightlife, dist = 100)

tmap_options(check.and.fix = TRUE)
tm_shape(base_map) + tm_borders(col='black') + tm_fill(col = "white") +
  tm_shape(nightlife) + tm_dots(size = 0.0075, col = "red") + # Nightlife locations
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="Nightlife location in Singapore",
            title.position = c('left', 'bottom'))

#coordinates(data) <- ~longitude+latitude

# Define the extent of the raster based on the spatial object
# raster_extent <- extent(data)

# Create an empty raster
# r <- raster(raster_extent, resolution=c(0.001, 0.001)) # Here 0.001 is an example resolution. Adjust as necessary.

#Rasterise the data
# under5_raster <- rasterize(data, r, field="under5", fun=mean)
# 
# plot(under5_raster)
# 
# ppp_data <- as.ppp(data)
# plot(ppp_data)
# 
# # Compute mean center
# mean_center <- with(ppp_data, c(mean(x), mean(y)))
# print(mean_center)
# 
# # Compute weighted mean center for 'under5' column
# weights <- data$under5
# weighted_mean_center <- with(ppp_data, c(weighted.mean(x, w=weights), weighted.mean(y, w=weights)))
# print(weighted_mean_center)
# plot(weighted_mean_center)

# # Quadrat Test
# qtest <- quadrat.test(ppp_data)
# print(qtest)
# plot(qtest)
# 
# # K-function
# Kest <- envelope(ppp_data, fun=Kest)
# plot(Kest)

#Frequency of all column values
table(data$under5)
table(data$above60)
table(data$men)
table(data$women)
table(data$repro)
table(data$youth)
table(data$gen)

#dispersion standard distance deviation #TIME TAKEN is about 15min
# sdd <- sqrt(with(ppp_data, sum((x - mean_center[1])^2 + (y - mean_center[2])^2) / length(x)))
# print(sdd)

#Explore Point of Interest (POI) data
# bar_club <- poi %>% filter(bar == "True" | night_club == "True")
# bar_club %>% select(name,lat,lng)

#Youth polygon
yth_spatial <- st_as_sf(yth_data, coords = c("longitude", "latitude"), crs = 4326)

#Plot the points to visualise how the values look #plot takes a while to load
tm_shape(base_map) + tm_borders() + 
  tm_shape(yth_spatial) + tm_bubbles(size = 0.05, col = 'youth', border.alpha = 0)  +
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="HDB location in Singapore",
            title.position = c('left', 'bottom'))

#We notice that a lot of the same values are congregated together, so lets make them into polygon
#Group points with the same value and create convex hulls
grouped_points <- yth_spatial %>%
  group_by(youth) %>%
  summarize()
yth_poly <- grouped_points %>%
  st_convex_hull()
yth_poly <- yth_poly[-14,] #remove the individual point which is out of place 



#HDB points 
hdb_spatial <- st_as_sf(hdb, coords = c("lng", "lat"), crs = 4326)
joined <- st_join(hdb_spatial,yth_poly) #combine the HDB data with the youth density to assign youth density values to each HDB

#I tried to plot both points and poly tgthr, looks OK 
ggplot() +
  geom_sf(data = yth_poly, aes(fill = youth), color = "black") +
  geom_point(data = joined, aes(x = st_coordinates(joined)[,1], y = st_coordinates(joined)[,2], color = youth), size = 1) +
  scale_color_gradient(name = 'HDB locations and density', low = "yellow", high = "red") + 
  scale_fill_gradient(name = 'Density polys', low = "blue", high = "green") +
  labs(title = "Overlay of Polygon and Point Plot") +  theme_void()

#This plot is to show how some HDB lies outside the given density polys.
#HDB with youth polygons
tm_shape(base_map) + tm_borders() + 
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



#Interpolation to fill in missing data

#First, we need to seperate out data with NA values and only interpolate on coordinates with points

joined_noNA <- joined %>% filter(!is.na(youth))

joined_spatial <- as(joined_noNA, "Spatial")

#Creating a raster layer for interpolation
grd              <- as.data.frame(spsample(joined_spatial, "regular", n=10000))
names(grd)       <- c("lat", "lng")
coordinates(grd) <- c("lat", "lng")
gridded(grd)     <- TRUE
fullgrid(grd)    <- TRUE

crs(grd) <- crs(joined_spatial)
yth.idw <- gstat::idw(youth ~ 1, joined_spatial, newdata=grd, idp=3.0) #idp of 3 chosen as we want closer values to mean more

r_hdb_idw       <- raster(yth.idw)


#Raster plot with idw values 
tm_shape(r_hdb_idw) + 
  tm_raster(n=10,palette = "Blues", stretch.palette = FALSE,title="Predicted youth density") + 
  tm_shape(joined_spatial) + tm_dots(size=0.005) +
  tm_legend(legend.outside=TRUE)

#EDA on youth density, making a histogram of youth densities
yth_hist <- r_hdb_idw@data@values

ggplot(data.frame(values = yth_hist), aes(x = values)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", aes(y = ..count..)) +
  ggtitle("Histogram of Youth Densities (%)") +
  xlab("Youth Density (%)") +
  ylab("Frequency") + theme_minimal()

#A couple of statistics for youth density
mean(yth_hist)
median(yth_hist)
quantile(yth_hist,0.25)
quantile(yth_hist,0.75)
max(yth_hist)



#Before we move on, we realise that with spatial interpolation, the assigning of values will take a really long time
#As such, we reclassify the values into groups before converting them nto polygons for analysis

reclass_value_idw <- c(0,0.25,0) #to reclass values into 25 different bins
for (i in seq(0,24,1)) {
  reclass_value_idw[3*i + 4] = 0.25 + 0.5*i
  reclass_value_idw[3*i + 5] = 0.75 + 0.5*i
  reclass_value_idw[3*i + 6] = 0.5 + 0.5*i
}

reclass_r_hdb_idw <- reclassify(as(r_hdb_idw, "RasterLayer"), reclass_value_idw) #reclassify kde values to groups


#Converting raster layer into polygons to view overlay over Singapore
threshold <- quantile(yth_hist,0.5) #define a threshold to determine what values of youth density we drop
hdb_idw <- rasterToPolygons(reclass_r_hdb_idw, fun = function(x) {x > threshold},dissolve = TRUE) #
hdb_idw <- st_as_sf(hdb_idw)
hdb_idw <- hdb_idw %>% rename(youth = var1.pred)
hdb_idw <- st_cast(hdb_idw,'MULTIPOLYGON')
st_crs(hdb_idw) <- st_crs("EPSG:4326") #set crs for new polygons

#overlaid raster plot with polygon plot (looks weird as every square has an individual value) 
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_idw) + tm_polygons(col = 'youth', palette = 'Blues') + 
  tm_shape(hdb_spatial) + tm_bubbles(size = 0.1, scale = 0.5, col = 'black')  + #plot with all hdb points
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="HDB location in Singapore",
            title.position = c('left', 'bottom'))


#assigning each hdb a youth value by intersecting the multipolygons to with all hdbs
hdb_yth_pts <- st_intersection(hdb_spatial, hdb_idw)

tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_idw) + tm_polygons(col = 'youth', palette = 'Blues') + 
  tm_shape(hdb_yth_pts) + tm_bubbles(size = 0.1, scale = 0.5, col = 'black')  + #plot with only hdbs points inside the filter
  tm_compass(type="8star", size = 2) +
  tm_scale_bar(width = 0.15) +
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.25, 
            legend.title.size = 0.5,
            title="HDB location in Singapore",
            title.position = c('left', 'bottom'))


#Handling NAs and duplicates #deprecated as NAs and duplicates were handled with interpolation
# yth_mean <- mean(joined$youth, na.rm = T)
# joined <- joined %>% mutate(youth = ifelse(is.na(youth), yth_mean, youth)) #replace NAs with mean
# joined <- joined %>% group_by(addr) %>% summarise(addr = addr, youth = mean(youth), geometry = geometry) #for rows with multiple densities, replace with mean of duplicates.
# joined <- unique(joined)

  
#Plot overlay of polygon and point plot for bus stops with HDB points
ggplot() +
  geom_sf(data = yth_poly, aes(fill = youth), color = "black") +
  geom_point(data = joined, aes(x = st_coordinates(joined)[,1], y = st_coordinates(joined)[,2], color = youth), size = 1) +
  geom_point(data = bus_spatial, aes(x = st_coordinates(bus_spatial)[,1], y = st_coordinates(bus_spatial)[,2]), size = 1) +
  scale_color_gradient(name = 'HDB locations and density', low = "yellow", high = "red") + 
  scale_fill_gradient(name = 'Density polys', low = "blue", high = "green") +
  labs(title = "Overlay of Polygon and Point Plot") +  theme_void()



# K-nearest neighbors (KNN) method: # Perform Moran's I test
# w <- knn2nb(knearneigh(coordinates(data), k = 5))  # Adjust 'k' as needed
# w_listw <- nb2listw(w)

#*W output
#*Neighbour list object:
#*Number of regions: 217054  
#*Number of nonzero links: 1085270 
#*Percentage nonzero weights: 0.002303574 
#*Average number of links: 5 

# 
# moran_test <- moran.test(x = data$youth, listw = w_listw)
# print(moran_test)
# #* output is p-value of < 2.2e-16
# #* Moran I statistic       Expectation          Variance 
# #* 9.974184e-01     -4.607170e-06      1.693194e-06
# 
# 
# #Kernel Density estimation
# ppp_data <- ppp(
#   data$longitude,
#   data$latitude,
#   window = owin(range(data$longitude), range(data$latitude))
# )
# 
# # Perform kernel density estimation
# kde <- density(ppp_data)
# plot(kde)


#KDE plot

hdb_points <- as(hdb_yth_pts, "Spatial")
hdb_centers <- kde.points(hdb_points, h = 0.013) #1.4km bandwidth 
hdb_centers_sf <- st_as_sf(hdb_centers)

plot(hdb_centers) 
tm_shape(hdb_centers) + tm_raster()

#Reclassify values in raster to make contour lines as seen in KDE plot.
reclass_values <- c(0,100,1, #reclassify kde values from 0-50 in group 1 and so on
                    100,200,2,
                    200,300,3,
                    300,400,4,
                    400,500,5,
                    500,600,6)

reclass_hdb_centers <- reclassify(as(hdb_centers, "RasterLayer"), reclass_values) #reclassify kde values to groups
hdb_centers_poly <- rasterToPolygons(reclass_hdb_centers, dissolve = T) #to make a polygon layer
hdb_centers_poly <- st_as_sf(hdb_centers_poly) #to make an SF object
hdb_centers_poly <- hdb_centers_poly[-c(1,2),] #remove polys with low kde values 
hdb_centers_poly <- st_cast(hdb_centers_poly,'POLYGON') #to split multipolygon to polygon to obtain centers

#map to show how the kde looks like with all hdbs
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(hdb_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(hdb_yth_pts) + tm_bubbles(size = 0.005, scale = 0.5)  +
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
closest_busstops_buffer <- st_buffer(closest_busstops, dist = 1500) #1.5km buffer
closest_busstops_buffer <- st_union(closest_busstops_buffer) #join the buffers together
closest_busstops_buffer <- st_cast(closest_busstops_buffer,'POLYGON') #join the buffers together
closest_busstops_buffer <- st_make_valid(closest_busstops_buffer)

#Busstop buffers with ALL HDBs
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


#Busstop buffers with Filtered HDBs
tm_shape(base_map) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(closest_busstops_buffer) + tm_polygons() + 
  tm_shape(hdb_yth_pts) + tm_bubbles(size = 0.15, scale = 0.5, col = 'black')  +
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
#1.5km radius: 47.8% HDB captured for all HDBs


num_hdb_captured <- nrow(st_intersection(hdb_yth_pts, closest_busstops_buffer))
percentage_captured <- num_hdb_captured / nrow(hdb_yth_pts)
print(percentage_captured)
#1.5km radius: 98.7% HDB captured for filtered HDBs

#Now we want to add the region attribute to each datapoint for each busstop for later
#First extract out the polygons with the regions only
combined_base_map <- base_map %>%
  group_by(REGION_N) %>%
  summarise(geometry = st_combine(geometry)) 
combined_base_map <- st_transform(combined_base_map, crs = 4326)
combined_base_map <- st_make_valid(combined_base_map)

final_closest_busstops <- st_intersection(closest_busstops,combined_base_map) #get the regions for each busstop 
final_closest_busstops <- distinct(final_closest_busstops) #remove duplicated rows
final_closest_busstops <- final_closest_busstops %>% rename(Region = REGION_N)

#Export as a geojson such for bus line analysis
st_write(final_closest_busstops, "bus_stops.geojson", driver = 'geoJSON',append = T) # DO NOT RUN AGAIN, IT WILL KEEP ADDING LAYERS 

#Onto finding ideal busstops for nightlife

nightlife_points <- as(nightlife, "Spatial")
nightlife_centers <- kde.points(nightlife_points, h = 750) #1km smoothing
nightlife_centers_sf <- st_as_sf(nightlife_centers)
nightlife_centers$kde <- nightlife_centers$kde * 1000000000 

tm_shape(nightlife_centers) + tm_raster()

max(nightlife_centers$kde)

reclass_values_2 <- c(0,max(nightlife_centers$kde) / 7,1, #reclassify kde values from 0-1000 in group 1 and so on
                      max(nightlife_centers$kde) / 7,2 * max(nightlife_centers$kde) / 7,2,
                      2* max(nightlife_centers$kde) / 7,3 * max(nightlife_centers$kde) / 7,3,
                      3* max(nightlife_centers$kde) / 7,4 * max(nightlife_centers$kde) / 7,4,
                      4* max(nightlife_centers$kde) / 7,5 * max(nightlife_centers$kde) / 7,5,
                      5* max(nightlife_centers$kde) / 7,6 * max(nightlife_centers$kde) / 7,6,
                      6* max(nightlife_centers$kde) / 7,max(nightlife_centers$kde),7)
?rasterToPolygons

reclass_nightlife_centers <- reclassify(as(nightlife_centers, "RasterLayer"), reclass_values_2) #reclassify kde values to groups
nightlife_centers_poly <- rasterToPolygons(reclass_nightlife_centers, dissolve = T, digits = 6) #to make a polygon layer
nightlife_centers_poly <- st_as_sf(nightlife_centers_poly) #to make an SF object
nightlife_centers_poly <- nightlife_centers_poly[-c(1,2),] #remove polys with low kde values 
nightlife_centers_poly <- st_cast(nightlife_centers_poly,'POLYGON') #to split multipolygon to polygon to obtain centers

#Central area of SG
central_area <- base_map %>% filter(PLN_AREA_N %in% c('DOWNTOWN CORE', 'BUKIT MERAH', 'SINGAPORE RIVER', 'MUSEUM', 'RIVER VALLEY', 'ORCHARD','NEWTON','ROCHOR','TANGLIN', 'KALLANG'))

#map to show how the kde looks like with all nightlife spots
tm_shape(central_area) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(nightlife_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(nightlife) + tm_bubbles(size = 0.01, col = "black")  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Nightlife locations in Singapore",
            title.position = c('left', 'top'))

#get the centroids of each polygon
nightlife_centers_points <- st_centroid(nightlife_centers_poly)
#find closest busstop to each centroid
nightlife_centers_points <- st_transform(nightlife_centers_points, crs = 4326)
closest_nightlife_busstops_index <- st_nearest_feature(nightlife_centers_points, bus_spatial)
closest_nightlife_busstops <- bus_spatial[closest_nightlife_busstops_index,]

tm_shape(central_area) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(nightlife_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(nightlife_centers_points) + tm_bubbles(size = 0.05, col = "black")  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Nightlife locations centers in Singapore",
            title.position = c('left', 'top'))

tm_shape(central_area) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(nightlife_centers_poly) + tm_fill(col = 'kde') + tm_borders() +
  tm_shape(closest_nightlife_busstops) + tm_bubbles(size = 0.05, col = "black")  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Busstops closest to centers in Singapore",
            title.position = c('left', 'top'))

closest_nightlife_busstops_buffer <- st_buffer(closest_nightlife_busstops, dist = 500) #500m buffer
closest_nightlife_busstops_buffer <- st_union(closest_nightlife_busstops_buffer) #join the buffers together
closest_nightlife_busstops_buffer <- st_cast(closest_nightlife_busstops_buffer,'POLYGON') #join the buffers together
closest_nightlife_busstops_buffer <- st_make_valid(closest_nightlife_busstops_buffer)

#map to show how the kde looks like with all nightlife spots
tm_shape(central_area) + tm_borders() + 
  tm_basemap('OpenStreetMap') + 
  tm_shape(closest_nightlife_busstops_buffer) + tm_fill(col = 'yellow') + tm_borders() +
  tm_shape(nightlife) + tm_bubbles(size = 0.025, col = "black")  +
  tm_compass(type="8star", size = 2) + 
  tm_layout(legend.format = list(digits = 0),
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5, 
            legend.title.size = 1,
            title="Busstop buffer for nightlife locations in Singapore",
            title.position = c('left', 'top'))

nightlife <- st_transform(nightlife, crs = 4326)
num_nightlife_captured <- nrow(st_intersection(nightlife, closest_nightlife_busstops_buffer))
percentage_captured <- num_nightlife_captured / nrow(nightlife)
print(percentage_captured)
#500m radius: 71.0% nightlife captured

combined_base_map_nightlife <- central_area %>%
  group_by(REGION_N) %>%
  summarise(geometry = st_combine(geometry)) 
combined_base_map_nightlife <- st_transform(combined_base_map_nightlife, crs = 4326)
combined_base_map_nightlife <- st_make_valid(combined_base_map_nightlife)

final_closest_busstops_nightlife <- st_intersection(closest_nightlife_busstops,combined_base_map_nightlife) #get the regions for each busstop 
final_closest_busstops_nightlife <- distinct(final_closest_busstops_nightlife) #remove duplicated rows
final_closest_busstops_nightlife <- final_closest_busstops_nightlife %>% rename(Region = REGION_N)

st_write(final_closest_busstops_nightlife, "bus_stops_nightlife.geojson")




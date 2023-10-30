---
  title: "BT4015 Project Group 6"
author: "Chan Tse Ee (A0221197H)"
date: "24 October 2023"
output:
  html_document: default
word_document: default
pdf_document: default
---
  

#load all the following libraries else install.packages accordingly
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


#Read data from file
data <- read.csv("dataset/filteredSG_population_density.csv")
poi <- read.csv('dataset/poi.csv')
hdb <- read.csv("dataset/hdb.csv") %>% mutate(addr = paste(blk_no,street)) %>% select(addr,lat,lng)
yth_data <- read.csv("dataset/filteredSG_population_density.csv") %>% select(longitude, latitude, youth)


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


# Read & Load Bus Stop Data
bus_data <- data.frame()
for(i in 0:10) {
  file_name <- paste0("busstops/response(", i, ").json")
  # Handle the case for response.json (without the index)
  if(i == 0) {
    file_name <- "busstops/response.json"
  }
  json_read <- fromJSON(file_name, flatten = TRUE)
  bus_data <- rbind(bus_data, json_read$value)
}
bus_spatial <- st_as_sf(bus_data, coords = c("Longitude", "Latitude"), crs = 4326)
  
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



install.packages("osmdata")
install.packages("leaflet")
install.packages("readxl")
library(readxl)
library(osmdata)
library(tidyr)
library(dplyr)
library(leaflet)
library(dplyr)
library(ggplot2)
library(jsonlite) 

# restaurants_data <- grab

#str(restaurants_data)

setwd("C:\\Users\\anape\\OneDrive\\Desktop\\Year  III\\MINOR\\BT4015\\Project")
food_outlets <- read_excel("datasets/grab_excel.xlsx")

takeaway_restaurants <- subset(food_outlets, delivery_options == "DELIVERY_TAKEAWAY")

mart_restaurants <- subset(food_outlets, loc_type == "MART")
food_restaurants <- subset(food_outlets, loc_type == "FOOD")

coordinates(food_outlets) <- ~lon + lat

outlets_spatial <- st_as_sf(food_outlets, coords = c("lon", "lat"), crs = 4326)
class(outlets_spatial)

ggplot() +
  geom_sf(data = outlets_spatial) +
  labs(title = "Food Outlets Locations")

keys_to_extract <- c("open", "displayedHours", "sun", "mon", "tue", "wed", "thu", "fri", "sat")

# Function to extract values from JSON
extract_values <- function(row) {
  values <- sapply(keys_to_extract, function(key) extract_values(row$opening_hours, key))
  return(values)
}

extracted_values <- lapply(1:nrow(food_outlets), function(i) extract_values(food_outlets[i, ]))
s
food_outlets_extracted <- as.data.frame(do.call(rbind, extracted_values))
colnames(food_outlets_extracted) <- keys_to_extract

food_outlets <- cbind(food_outlets, food_outlets_extracted)

head(food_outlets)

# Check if any outlet closes before 22:00 for all days
close_before_2200 <- apply(food_outlets[, keys_to_extract] < "22:00", 1, all)

# Get the names of outlets that close before 22:00 for all days
outlets_close_before_2200 <- food_outlets$name[close_before_2200]

# Print the names of outlets that close before 22:00 for all days
cat("Outlets that close before 22:00 for all days:\n")
cat(outlets_close_before_2200, sep = "\n")























my_map <- leaflet() %>%
  setView(lng = 103.8198, lat = 1.3521, zoom = 10)

my_map <- my_map %>%
  addMarkers(data = mart_restaurants, lng = ~lon, lat = ~lat, popup = ~name)
my_map



# Convert the map to an HTML widget
my_map_widget <- htmlwidgets::onRender(my_map)

# Display the map in the Viewer pane
IRdisplay::display_html(my_map_widget)
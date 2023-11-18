# Happy Sip Happy Slurp Happy Shuttle: Enhancing's Singapore's Night Life

## 1. Introduction

In the bustling cityscape of Singapore, nightlife is a significant component of the city's culture and economy. The efficiency of post-midnight services, particularly in food availability and transportation, plays a crucial role in shaping this aspect. "HappyHour" delves into the geospatial aspects of Singapore's nightlife with an aim to enhance the offerings of post-evening amenities.

This project leverages analytics and data to unravel urban patterns of nightlife, accessibility of late-night services, and their correlations with the city’s demographics. With a focus on affordable and cost-effective options, this study aims to strengthen the cultural vibrancy and economic prosperity of Singapore's night economy.

Singapore currently faces a scarcity in late-night eateries and public transport availability, leading to an overreliance on costly ride-hailing services. Our Project seeks to address these challenges by providing insights into the spatial distribution of nightlife hotspots, accessibilities, and availability of night-time transport services, ultimately guiding business decisions and catalyzing the development of a safer, more inclusive night economy.

## 2. Objectives

The primary goal of the project is to identify and delineate the landscape of nightlife activities in Singapore, focusing particularly on dining and transportation options post-midnight. The project aims to:

- **Assess Accessibility**: Measure the ease of finding late-night food and bus routes.
- **Analyze Patterns**: Understand where people go at night, the availability of food options, and bus services.
- **Define Nightlife Hotspots**: Establish criteria for what constitutes a nightlife location.

By addressing these objectives, our project hopes to offer insights for the improvement of the night-time scene in Singapore, suggesting potential areas for the addition of food places or bus stop routes post-midnight.

## [Project Structure]
Code files in the main directory are labelled No.1 to 5 with each representing the following: 
1. Data Preprocessing in R & Python
2. Exploratory Spatial Data Analysis in R
3. Analysis for finding restaurants in R
4. Analysing Bus Stops using KDE, IDW, Kriging and Buffer Spatial Analysis. 
*Note: An updated version of the HTML file requires you to knit the no.4 Rmd file*
5. Finding Optimal Bus Stops with Road Networks to map the optimal path

## Folders
1. Dataset - All of our data source for this project from Singapore Base Map, Restaurants Locations, etc
2. Plots - Contains all the image plots for our project analysis into nightlife
3. optimal_stops - Contains all the analysis in filtering the number of bus stops to serve strategic locations in Singapore's HDB areas
4. optimal_bus_stops - Contains the 2 recommended route of our public transportation analysis bringing individuals from downtown to high youth density HDB residential areas post midnight

# Example Plots of our Analysis
![linear trend at 12am.png](plots%2Fcrowd_density%2Flinear%20trend%20at%2012am.png)
![linear trends.gif](plots%2Fcrowd_density%2Flinear%20trends.gif)
![hdb_yth_plot.png](plots%2Foptimal_stops%2Fhdb_yth_plot.png)
![idw_final.png](plots%2Foptimal_stops%2Fidw_final.png)
![east_optimal_bus_route.png](plots%2Foptimal_bus_routes%2Feast_optimal_bus_route.png)
![west_optimal_busroute.png](plots%2Foptimal_bus_routes%2Fwest_optimal_busroute.png)
---
*This project is part of the BT4015 Geospatial Analytics module in the National University of Singapore (NUS) Academic Year 2023/2024 created by Ana Pelin, Minh Hai, Shayer Ahmed, Jeremy Chan Tse Ee, Zhuo Yun Hui*




# Load the libraries
library(sf)
library(dplyr)
library(purrr)
library(raster)
library(osmdata)
library(spDataLarge)
library(data.table)
library(rio)
library(ggplot2)
library(ggmap)
library(tmap)
library(rgeos)
library(geosphere)
library(tidyr)
library(osmdata)
library(OpenStreetMap)
library(maps)
library(RColorBrewer)
library(spatstat)
library(igraph)
library(janitor)
library(spatialEco)
library(RJSONIO)
library(RCurl)
library(jsonlite)
library(rangeMapper)
library(geojsonio)
library(descr)
library(BaylorEdPsych)
library(mvnmle)
library(mice)
library(Amelia)


# Load the files
spdf_eca <- readRDS('spdf_eca.rds')
spdf_build <- readRDS('spdf_build.rds')


############################# Calculate number of ECA organizations in radius of 1km for each building

calculateNumberOfEcaInRadius <- function(house_coord, eca_coords=spdf_eca, r=1000){
  # Calculate parwise distance between points
  dist_matrix_build_eca <- distm(house_coord, eca_coords)
  # Return number of organizations which are closer than 1000 meters to the building
  return(sum(dist_matrix_build_eca < r))
}


calculateNumberOfEcaInRadiusForRegion <- function(spdf_eca, spdf_build, org_category=F){
  
  ### SELECT ORGANIZATION CATEGORY IF CHOSEN
  if(org_category != F){
    spdf_eca <- spdf_eca[spdf_eca$new_category == org_category,]
  }
  
  
  ### STATE ORGANIZATIONS
  # Calculate for all buildings STATE
  eca_number_state_vec <- c()
  for (i in seq(1:length(spdf_build))){
    eca_number_state_vec <- append(eca_number_state_vec, calculateNumberOfEcaInRadius(spdf_build[i,],
                                                                                      spdf_eca[spdf_eca$own_type == 'state',]))
  }
  # Add number of closest ECA organizations to the original dataframe 
  spdf_build$eca_number_state <- eca_number_state_vec
  # Mean number of eca in radius of 1km in each district
  df_build <- as.data.frame(spdf_build)
  mean_eca_number_district <- aggregate(df_build$eca_number_state, list(df_build$district_name), mean)
  names(mean_eca_number_district) <- c("district_name", "mean_eca_number_in_radius")
  # Add eca number
  temp_df <- as.data.frame(spdf_borders$district_name)
  names(temp_df) <- "district_name"
  temp_df <- plyr::join(temp_df, mean_eca_number_district)
  spdf_borders$mean_state_eca_number <- temp_df$mean_eca_number_in_radius
  # Plot map with radius borders
  print(tm_shape(spdf_borders) + tm_borders() + tm_fill(col="mean_state_eca_number", palette="Greens") + tm_layout(
    legend.title.size = 2,
    legend.text.size = 1,
    legend.title.color = 'white'))
  print('State organizations printed')
  
  
  ### PRIVATE ORGANIZATIONS
  # Calculate for private organizations
  eca_number_private_vec <- c()
  for (i in seq(1:length(spdf_build))){
    eca_number_private_vec <- append(eca_number_private_vec, calculateNumberOfEcaInRadius(spdf_build[i,],
                                                                                          spdf_eca[spdf_eca$own_type == 'private',]))
  }
  # Add number of closest ECA organizations to the original dataframe 
  spdf_build$eca_number_private <- eca_number_private_vec
  # Add number of closest ECA organizations to the original dataframe 
  spdf_build$eca_number_private <- eca_number_private_vec
  # Mean number of eca in radius of 1km in each district
  df_build <- as.data.frame(spdf_build)
  mean_eca_number_district <- aggregate(df_build$eca_number_private, list(df_build$district_name), mean)
  names(mean_eca_number_district) <- c("district_name", "mean_eca_number_in_radius")
  # Add eca number
  temp_df <- as.data.frame(spdf_borders$district_name)
  names(temp_df) <- "district_name"
  temp_df <- plyr::join(temp_df, mean_eca_number_district)
  spdf_borders$mean_private_eca_number <- temp_df$mean_eca_number_in_radius
  # Plot map with radius borders
  print(tm_shape(spdf_borders) + tm_borders() + tm_fill(col="mean_private_eca_number", palette="Blues") + tm_layout(
    legend.title.size = 2,
    legend.text.size = 1,
    legend.title.color = 'white'))
  print('Private organizations printed')
  
  ### RATIO BETWEEN PRIVATE AND STATE ORGANIZATIONS
  spdf_borders$private_state_ratio <- spdf_borders$mean_private_eca_number / spdf_borders$mean_state_eca_number
  spdf_borders$private_state_ratio[is.na(spdf_borders$private_state_ratio)] <- 0
  # Plot map with radius borders
  print(tm_shape(spdf_borders) + tm_borders() + tm_fill(col="private_state_ratio", palette="YlOrRd") + tm_layout(
    legend.title.size = 2,
    legend.text.size = 1,
    legend.title.color = 'white'))
  print('Ratio printed')
  
}


# All organizations
calculateNumberOfEcaInRadiusForRegion(spdf_eca, spdf_build)
# Culture
calculateNumberOfEcaInRadiusForRegion(spdf_eca, spdf_build, org_category = 'culture')
# Sport
calculateNumberOfEcaInRadiusForRegion(spdf_eca, spdf_build, org_category = 'sport')
# Tech
calculateNumberOfEcaInRadiusForRegion(spdf_eca, spdf_build, org_category = 'tech')
# Other
calculateNumberOfEcaInRadiusForRegion(spdf_eca, spdf_build, org_category = 'other')


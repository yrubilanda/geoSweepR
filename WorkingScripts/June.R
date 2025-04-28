# June's Scratchpad
#libraries
install.packages("sp")
install.packages("gstat")
library(tidyverse)
library(sp)
library(gstat)
library(sf)
library(ggplot2)

d <- read_csv("Data/Clean_CrimeData_2024.csv", col_names = TRUE)
#####testing the workflow####
d_sf <- sf::st_as_sf(
  d,
  coords = c("longitude", "latitude"),  # tell sf which columns are longitude and latitude
  crs = 3857,                    # this is the standard GPS coordinate system (WGS 84)
  remove = FALSE                 # keep the original lat/lon columns in the table just in case
)

# Step 4: Create a grid over the extent of your points
# 500 meters grid size (you can adjust!)
grid <- st_make_grid(d_sf, cellsize = 500, square = TRUE)

# Step 5: Turn grid into sf object
grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)

# Step 6: Spatial join - count crimes in each grid cell
joined <- st_join(grid_sf, d_sf, left = TRUE)

# Step 7: Group and count
crime_counts <- joined %>%
  group_by(grid_id) %>%
  summarize(
    crime_count = n(),
    geometry = st_geometry(first(geometry))
  )

# Step 8: Calculate density (crimes per km²)
# Area is in m², so divide by 1,000,000 to get km²
crime_counts <- crime_counts %>%
  mutate(
    area_km2 = st_area(geometry) / 10^6,
    crime_density = crime_count / area_km2
  )

# Step 9: Convert crime_counts to spatial points (coordinates and crime density)
# Assuming that 'crime_counts' has the correct spatial reference (3857), we use st_as_sf
crime_points <- crime_counts %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 3857)

# Step 10: Create a variogram
# The variogram models the spatial structure of the data
vgram <- variogram(crime_density ~ 1, data = crime_points)

# Step 11: Fit a variogram model (e.g., Spherical model)
vgram_model <- fit.variogram(vgram, model = vgm("Sph"))

# Step 12: Perform kriging interpolation
# We will use the 'krige' function to perform the kriging based on the variogram model
kriged <- krige(crime_density ~ 1, locations = crime_points, newdata = grid_sf, model = vgram_model)

# Step 13: View kriged results
# The 'kriged' object will have the estimated values (crime density) for the grid
# You can plot the kriged result or inspect the output
plot(kriged["var1.pred"])  # Predicted crime density values

#####the function####
density_kriging  <- function(data, long_col, lat_col, crs, grid_size = 500, variogram_model = "Sph") {
  # Step 1: convert the data into an sf
  d_sf <- sf::st_as_sf(
    data,
    coords = c(long_col, lat_col),  # tell sf which columns are longitude and latitude
    crs = crs,                    # this is the standard GPS coordinate system (WGS 84)
    remove = FALSE                 # keep the original lat/lon columns in the table just in case
  )

  #Step 2: set it in a coordinate system that kriging is compatible with
  d_sf <- st_transform(d_sf, 3857)  # EPSG:3857 Web Mercator

  # Step 3: create a grid over the extent
  # currently set to a 500 meter grid (adjustable)
  grid <- st_make_grid(d_sf, cellsize = 500, square = TRUE)

  # Step 4: turn grid into sf object
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)

  # Step 6: spatial join
  joined <- st_join(grid_sf, d_sf, left = TRUE)

  # Step 7: group and count
  count_sf <- joined %>%
    group_by(grid_id) %>%
    summarize(
      count = n(),
      geometry = st_geometry(first(geometry))
    )

  # Step 8: calculate density (per km²)
  count_sf <- count_sf %>%
    mutate(
      area_km2 = st_area(geometry) / 10^6,
      density = count / area_km2
    )

  # Step 9: Convert crime_counts to spatial points (coordinates and crime density)
  # Assuming that 'crime_counts' has the correct spatial reference (3857), we use st_as_sf
  points_sf <- count_sf %>%
    st_as_sf(coords = c(long_col, lat_col), crs = 3857)

  # Step 10: Create a variogram
  # The variogram models the spatial structure of the data
  vgram <- variogram(density ~ 1, data = points_sf)

  # Step 11: Fit a variogram model (e.g., Spherical model)
  vgram_model <- fit.variogram(vgram, model = vgm(variogram_model))

  # Step 12: Perform kriging interpolation
  # We will use the 'krige' function to perform the kriging based on the variogram model
  kriged <- krige(density ~ 1, locations = points_sf, newdata = grid_sf, model = vgram_model)

  # Step 13: View kriged results
  # The 'kriged' object will have the estimated values (crime density) for the grid
  # You can plot the kriged result or inspect the output
  plot(kriged["var1.pred"], main = "Predicted Density")
}

density_kriging(data = d, long_col = "longitude", lat_col = "latitude", crs = 4326)

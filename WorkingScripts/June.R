# June's Scratchpad
#libraries
install.packages("sp")
install.packages("gstat")
library(tidyverse)
library(sp)
library(gstat)
library(sf)
#loading in the data
d <- read_csv("Data/Clean_CrimeData_2024.csv", col_names = TRUE)
d_sf <- st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Step 3: Project into a metric CRS (important for distance & area calculations!)
# 3857 is Web Mercator (meters), or you can pick local UTM zone
d_sf <- st_transform(d_sf, 3857)

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

# Step 9: Done! View
head(crime_counts)


v <- variogram(crime_density ~ 1, data = crime_counts)
v.fit <- fit.variogram(v, model=vgm(1, "Sph", 900, 1))

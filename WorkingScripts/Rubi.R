# Rubi's Scratchpad

#create project: DO NOT RUN AGAIN! PROJECT IS ALREADY CREATED
#usethis::create_package("dccrimepkg", open = FALSE)
install.packages("devtools")

#to run this you will need to install devtools
devtools::install("./dccrimepkg")

# Load libraries
library(readr)
library(dplyr)
library(sf)
library(maptiles)
library(terra)
library(ggplot2)
library(ggspatial)
library(viridis)

# 1. Read your CSV data
d <- read_csv("Data/Clean_CrimeData_2024.csv")

# 2. Convert to sf using lat/lon columns (adjust if needed)
d_sf <- st_as_sf(
  d,
  coords = c("longitude", "latitude"),  # <-- Make sure these match your actual column names
  crs = 4326,
  remove = FALSE
)

# 3. Define a manual bounding box (for D.C. area)
bbox <- sf::st_as_sfc(
  sf::st_bbox(c(
    xmin = -77.05,
    xmax = -76.95,
    ymin = 38.85,
    ymax = 38.95
  ), crs = st_crs(4326))
)

# 4. Download basemap (CartoDB Positron is a VALID provider)
raster_map <- get_tiles(bbox, provider = "CartoDB.Positron", crop = TRUE)

# 5. Reproject data to match the basemap CRS
d_proj <- st_transform(d_sf, crs = crs(raster_map))

# 6. Add x/y columns for plotting
d_proj <- cbind(d_proj, st_coordinates(d_proj))
names(d_proj)[names(d_proj) == "X"] <- "x"
names(d_proj)[names(d_proj) == "Y"] <- "y"

# 7. Create the heatmap plot
p <- ggplot() +
  layer_spatial(raster_map) +
  stat_density_2d(
    data = d_proj,
    aes(x = x, y = y, fill = after_stat(level)),
    geom = "polygon",
    contour = TRUE,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(option = "cividis") +
  labs(
    title = "Crime Heatmap – D.C.",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  coord_sf(crs = st_crs(raster_map))

# 8. Print the plot
print(p)



# Without basemap
heatmap_1 <- make_heatmap(d, lat_col = latitude, lon_col = longitude)
heatmap_1

# Filtered by type
make_crime_type_heatmap_basemap(d,
                                lat_col = latitude,
                                lon_col = longitude,
                                type_col = offense,
                                crime_type = "Robbery")

# with basemap
library(maptiles)
library(sf)
# Create bounding box for basemap
bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = -77.05, xmax = -76.95, ymin = 38.85, ymax = 38.95), crs = sf::st_crs(4326)))

# Download basemap tiles
raster_map <- get_tiles(bbox, provider = "CartoDB.Positron")

# Filtered by type
make_crime_type_heatmap_basemap(d,
                                lat_col = latitude,
                                lon_col = longitude,
                                type_col = offense,
                                crime_type = "Robbery",
                                basemap = raster_map)

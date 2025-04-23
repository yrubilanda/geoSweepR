# Rubi's Scratchpad

#create project: DO NOT RUN AGAIN! PROJECT IS ALREADY CREATED
#usethis::create_package("dccrimepkg", open = FALSE)
install.packages("devtools")

#to run this you will need to install devtools
devtools::install("./dccrimepkg")

library(dccrimepkg)
# Load sample data (replace with your actual data if needed)

d <- read_csv("Data/Clean_CrimeData_2024.csv", col_names = TRUE)
  
head(d)

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

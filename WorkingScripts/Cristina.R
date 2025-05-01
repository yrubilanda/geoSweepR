# Cristina's Scratchpad
# Data Exploration and Cleaning
f <- "inst/extdata/dc_sample.csv"
d <- read_csv(f, col_names = TRUE)

# Save my_basemap.tif to inst/extdata/
bbox <- sf::st_as_sfc(
  sf::st_bbox(c(
    xmin = -77.10,
    xmax = -76.90,
    ymin = 38.80,
    ymax = 39.00
  ), crs = st_crs(4326))
)
my_basemap <- get_tiles(bbox, provider = "CartoDB.Positron", crop = TRUE)



resample_heatmap <- function(data, lat_col, lon_col, n_samples = NULL, col_data = NULL, filter_by = NULL, basemap = NULL, input_crs = 4326){

  # Step 1: Filters and Sampling

  ## Step 1.1: Optional Filters

  ### Step 1.1.1: Check if specified and filter according to column and value
  if (!is.null(filter_by) && !is.null(col_data)) {
    data <- dplyr::filter(data, .data[[col_data]] == filter_by)

    #### Step 1.1.1.a: Check that there is still data after filtering. If there is no data, stop execution.
    if (nrow(data) == 0){
      stop("No data left after filtering. Check your col_data and filter_by values.")
    }

    ### Step 1.1.2: Resampling filtered data

    #### Step 1.1.2.a: Set default if n_samples is NULL. Default if to multiply the number of rows by 1000.
    if (is.null(n_samples)){
      n_samples <- 1000 * nrow(data)
    }

    #### Step 1.1.2.b: Resample filtered data with either a specified n_sample or default n_sample. (if specified, you might have to play around with this number)
    if(n_samples > nrow(data)){
      data <- dplyr::slice_sample(data, n = n_samples, replace = TRUE)
    }

  } else {

    ## Step 1.2: No Filter on Data

    ### Step 1.2.1: Resampling

    #### Step 1.2.1.a: Set default if n_samples is NULL. Default is to multiply the total number of rows in data set by 1000.
    if (is.null(n_samples)){
      n_samples <- 1000 * nrow(data)
    }

    #### Step 1.2.1.b: Resample the entire dataset by `n_samples`.
    if (n_samples > nrow(data)) {
      data <- data[sample(1:nrow(data), size = n_samples, replace = TRUE), ]
    }
  }

  # Step 2: Coordinate Reference System Argument

  ## Step 2.1: Make sure the coordinate reference system is used correctly. If no coordinate system is specified, the default is WGS 84.
  if (is.na(sf::st_crs(input_crs))) {
    stop("Invalid CRS: please provide a valid EPSG code or PROJ string.")
  }

  # Step 3: Convert to Spatial Object
  # Use latitude and longitude columns to convert to spatial object using {sf}.
  #`coords` tells the function which columns are latitude and longitude.
  # `crs = input_crs` calls the coordinate reference system. If no CRS is specified, it defaults to WGS 84.
  # `remove = FALSE` keeps the latitude and longitude in the table.
  data_sf <- sf::st_as_sf(
    data,
    coords = c(lon_col, lat_col),
    crs = input_crs,
    remove = FALSE
  )

  # Step 4: Handle Basemap

  ## Step 4.1: Basemap Present
  if (!is.null(basemap)) {
    crs_target <- terra::crs(basemap)
    data_proj <- sf::st_transform(data_sf, crs = crs_target)
  }

  ## Step 4.2: No Basemap. Defaults to showing the graph on its own.
  else{
    data_proj <- data_sf
  }

  # Step 5: Extract Coordinates
  data_proj <- cbind(data_proj, sf::st_coordinates(data_proj))

  ## Step 5.1: Rename for the plot.
  names(data_proj)[names(data_proj) == "X"] <- "x"
  names(data_proj)[names(data_proj) == "Y"] <- "y"

  # Step 6: Create Plot

  ## Step 6.1: Create basic plot.
  p <- ggplot2::ggplot()

  ## Step 6.2: Include basemap in the plot if it is provided.
  if(!is.null(basemap)) {
    p <- p + ggspatial::layer_spatial(basemap)
  }

  ## Step 6.3: Include layer with the heatmap.
  p <- p +
    ggplot2::stat_density_2d(
      data = data_proj,
      ggplot2::aes(x = x, y = y, fill = after_stat(level)),
      geom = "polygon",
      contour = TRUE,
      alpha = 0.5
    ) +
    ggplot2::scale_fill_viridis_c(option = "cividis") +
    ggplot2::labs(
      title = if (!is.null(filter_by)) paste("Resampled Heatmap:", filter_by) else "Resampled Heatmap",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()

  ## Step 6.4: Set coordinate reference system for basemap
  if(!is.null(basemap)) {
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(basemap))
  }

  # Step 7: Generate the final plot as the output of the function.
  return(list(
    plot = p,
    resampled_data = data_proj
  ))
}

# devtools::load_all()  # Use this to reload your package while developing if needed

#RESAMPLE TRY
p_resample <- resample_heatmap(
  data = my_data,
  lat_col = "latitude",
  lon_col = "longitude",
  col_data = "offense",
  filter_by = "Homicide")
p_resample

#KRIGGING TRY
density_kriging  <- function(data, lat_col, lon_col, input_crs = 4326, col_data = NULL, filter_by = NULL, grid_size = 500, variogram_model = "Sph", basemap = NULL) {
  # Step 1: check if there was a filter parameter set
  if (!is.null(filter_by) && !is.null(col_data)) {
    data <- dplyr::filter(data, .data[[col_data]] == filter_by)
  }
  # Step 2: convert the data into an sf
  d_sf <- sf::st_as_sf(
    data,
    coords = c(lon_col, lat_col),  # tell sf which columns are longitude and latitude
    crs = input_crs,                    # Defaults to WGS 84 if not specified.
    remove = FALSE                 # keep the original lat/lon columns in the table just in case
  )

  # Step 3: set it in a coordinate system that kriging is compatible with
  d_sf <- sf::st_transform(d_sf, 3857)  # EPSG:3857 Web Mercator

  # Step 4: create a grid over the extent
  # Grid size specification is optional. Default is set to a 500 meter grid.
  grid <- sf::st_make_grid(d_sf, cellsize = grid_size, square = TRUE)

  # Step 5: turn grid into sf object
  grid_sf <- sf::st_sf(grid_id = 1:length(grid), geometry = grid)

  # Step 6: spatial join
  joined <- sf::st_join(grid_sf, d_sf, left = TRUE)

  # Step 7: group and count
  count_sf <- joined %>%
    dplyr::group_by(grid_id) %>%
    dplyr::summarize(
      count = dplyr::n(),
      geometry = sf::st_geometry(dplyr::first(geometry))
    )

  # Step 8: calculate density (per km²)
  count_sf <- count_sf %>%
    dplyr::mutate(
      area_km2 = sf::st_area(geometry) / 10^6,
      density = count / area_km2
    )

  # Step 9: Convert `count_sf` to spatial points (coordinates and crime density)
  # Assuming that `count_sf` has the correct spatial reference (3857), we use st_as_sf
  points_sf <- count_sf %>%
    dplyr::filter(!is.na(density))


  # Step 10: Create a variogram
  # The variogram models the spatial structure of the data
  vgram <- gstat::variogram(density ~ 1, data = points_sf)

  # Step 11: Fit a variogram model (e.g., Spherical model)
  vgram_model <- gstat::fit.variogram(vgram, model = gstat::vgm(variogram_model))

  # Step 12: Perform kriging interpolation
  # We will use the 'krige' function to perform the kriging based on the variogram model
  kriged <- gstat::krige(density ~ 1, locations = points_sf, newdata = grid_sf, model = vgram_model)

  # Step 13: Transform kriged result back to `input_crs`
  kriged <- sf::st_transform(kriged, crs = input_crs)

  # Step 14: Create ggplot
  p <- ggplot2:: ggplot()

  # Step 14.1: Include basemap is specified
  if(!is.null(basemap)){
    p <- p + ggspatial::layer_spatial(basemap)
  }

  # Step 14.2: Continue building ggplot
  p <- p +
    ggplot2::geom_sf(
      data = kriged,
      aes(fill = var1.pred),
      color = NA,
      alpha = 0.5
    ) +
    ggplot2::scale_fill_viridis_c(option = "cividis") +
    ggplot2::labs(
      title = if (!is.null(filter_by)) paste("Kriged Density:", filter_by) else "Kriged Density",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()

  # Step 14.3: Make sure coordinates match the basemap if there is one
  if (!is.null(basemap)) {
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(basemap))
  }

  # Step 15: Return the kriged table and the plot
  return(list(
    kriged_data = kriged,
    plot = p
  ))
}




# TEST
p_kriging <- density_kriging(d, lon_col = "longitude", lat_col = "latitude", basemap = raster_map)
p_kriging



#MAKE HEATMAP TRY
make_heatmap <- function(data, lat_col, lon_col, col_data = NULL, filter_by = NULL, basemap = NULL, input_crs = 4326) {

  # If both a column name and a value to filter by are given, filter the data down to just that value
  if (!is.null(filter_by) && !is.null(col_data)) {
    data <- dplyr::filter(data, .data[[col_data]] == filter_by)
  }

  # Check to make sure the CRS system is used correctly
  if (is.na(st_crs(input_crs))) {
    stop("Invalid CRS: please provide a valid EPSG code or PROJ string.")
  }

  # Turn your data frame into a spatial object (sf) using latitude and longitude columns
  data_sf <- sf::st_as_sf(
    data,
    coords = c(lon_col, lat_col),  # tell sf which columns are longitude and latitude
    crs = input_crs,                    # default to WGS 84, unless specified in the function
    remove = FALSE                 # keep the original lat/lon columns in the table just in case
  )

  # If the user provided a basemap, match the coordinate system to the map
  if (!is.null(basemap)) {
    crs_target <- terra::crs(basemap)                         # get the map’s coordinate system
    data_proj <- sf::st_transform(data_sf, crs = crs_target)  # reproject your points to match the map
  } else {
    data_proj <- data_sf  # if no map, just keep the original projection (WGS 84)
  }

  # Add X and Y columns (in projected units) so we can use them in the plot
  data_proj <- cbind(data_proj, sf::st_coordinates(data_proj))
  names(data_proj)[names(data_proj) == "X"] <- "x"  # rename for plotting
  names(data_proj)[names(data_proj) == "Y"] <- "y"

  # Start building the plot with ggplot
  p <- ggplot2::ggplot()

  # If we were given a basemap, add it to the plot first (as the background layer)
  if (!is.null(basemap)) {
    p <- p + ggspatial::layer_spatial(basemap)
  }

  # Add the heatmap layer — this shows areas with more data points as more intense
  p <- p +
    ggplot2::stat_density_2d(
      data = data_proj,  # the projected spatial data with x and y columns
      ggplot2::aes(x = x, y = y, fill = ggplot2::after_stat(level)),  # use density level as the fill
      geom = "polygon",   # draw filled contour polygons
      contour = TRUE,     # draw contour lines
      alpha = 0.5         # make it semi-transparent so you can see the basemap if you added one
    ) +
    ggplot2::scale_fill_viridis_c(option = "cividis") +  # use a colorblind-friendly color scale
    ggplot2::labs(
      title = if (!is.null(filter_by)) paste("Heatmap:", filter_by) else "Heatmap",  # add a title
      x = "Longitude", y = "Latitude"  # label the axes
    ) +
    ggplot2::theme_minimal()  # use a clean minimal background theme

  # If there was a basemap, set the plot's coordinate system to match the map
  if (!is.null(basemap)) {
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(basemap))
  }

  return(p)  # Return the finished heatmap plot
}


# devtools::load_all()  # Use this to reload your package while developing if needed

#MAKE HEATMAP EXAMPLE
p_heatmap <- make_heatmap(d, lat_col = "latitude", lon_col = "longitude", basemap = raster_map)
p_heatmap



#### RUBI'S EXPLANATION FOR CREATING BASEMAPS
# Load libraries needed for getting a basemap and working with spatial data
# Package {maptiles} is needed for downloading map tile layers like CartoDB
# Package {sf} is needed for handling spatial objects like bounding boxes

# --- NOTE TO USERS ---
# The code below creates a basemap specifically for the DC crime data we're using in this example.
# The bbox values (xmin, xmax, ymin, ymax) define the area of interest in Washington, D.C.
#
# Our `make_heatmap()` function **does not generate this basemap** — you’ll need to set this up yourself
# based on the bounding box that makes sense for your own dataset (e.g., different city or region).
# This step is fully up to the user — the function just adds it if you provide one.

# Create a bounding box around the area you want to show on the map (in this case, part of D.C.)
bbox <- sf::st_as_sfc(
  sf::st_bbox(c(
    xmin = -77.05,  # western boundary of the area (longitude)
    xmax = -76.95,  # eastern boundary (longitude)
    ymin = 38.85,   # southern boundary (latitude)
    ymax = 38.95    # northern boundary (latitude)
  ), crs = sf::st_crs(4326))  # Set the coordinate reference system (WGS84)
)

# Download the basemap tiles for that bounding box using the CartoDB Positron provider
# You could change the provider to something else like OpenStreetMap or Stamen if you want a different style
my_basemap <- maptiles::get_tiles(bbox, provider = "CartoDB.Positron")

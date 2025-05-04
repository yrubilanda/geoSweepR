#' @title Resampled Kernel Density Heatmap
#' @description
#' Create a 2D Kernel Density Heatmap from Latitude and Longitude with Optional Filter and Basemap Using Resampled Data
#'
#' This function is an alternative to the make_heatmap() function.
#' This function will resample data to create a heatmap when there isn't sufficient data.
#' Can be used to resample filtered data or the whole data set.
#' When the resample size is not specified, it will multiply the number of rows by 1000 as a default.
#' It generates a smooth heatmap from latitude and longitude coordinates.You can also add a base-map.
#'
#' @param data Required. A data frame with your point data. Needs to have latitude and longitude columns.
#' @param lat_col Required. A string for the name of your latitude column
#' @param lon_col Required. A string for the name of your longitude column
#' @param col_data Optional. Name of the column you are using to filter your data. Can be categorical or factor data for filtering purposes.
#' @param filter_by Optional. Name of the value to filter the `col_data` column by values. Only works when `col_data` is also included.
#' @param basemap Optional. A spatial object (e.g., an `sf` or `raster` object) to use as background for plot.
#' @param input_crs Optional. Specify a different EPSG code or PROJ string for CRS in your input data. Default coordinate system is EPSG:4326 (WGS 84).
#' @param n_samples Optional. Integer to re-sample either entire dataset or optional filtered data. Default is set to multiply the number of rows by 100.
#'
#' @return A ggplot2 heatmap of resampled data — you can print it, save it, or add more layers if you want. And a resampled_data object with the data frame.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot stat_density_2d aes geom_polygon scale_fill_viridis_c labs theme_minimal coord_sf after_stat
#' @importFrom sf st_as_sf st_transform st_coordinates st_crs
#' @importFrom terra crs
#' @importFrom ggspatial layer_spatial
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' # Sample Data:
#' my_data <- read.csv(system.file("extdata", "dc_sample.csv", package = "geoSweepR"))
#' my_basemap <- terra::rast(system.file("extdata", "my_basemap.tif", package = "geoSweepR"))
#'
#' # Plot filtered data and specify resample size.
#' # - Example uses only "Homicide" and resamples to 300.
#' resample_heatmap(data = my_data, lat_col = "latitude", lon_col = "longitude",
#'                  col_data = "offense", filter_by = "Homicide", n_samples = 300)
#'
#' # Plot filtered data, specify resample size, and add a basemap layer underneath.
#' # - Example uses only "Homicide" and resamples to 10000.
#' resample_heatmap(data = my_data, lat_col = "latitude", lon_col = "longitude",
#'                  col_data = "offense", filter_by = "Homicide", n_samples = 300,
#'                  basemap = my_basemap)
#'

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

    #### Step 1.1.2.a: Set default if n_samples is NULL. Default is to multiply the number of rows by 1000.
    if (is.null(n_samples)){
      n_samples <- 100 * nrow(data)
    }

    #### Step 1.1.2.b: Resample filtered data with either a specified n_sample or default n_sample. (if specified, you might have to play around with this number)
    if(n_samples > nrow(data)){
      data <- dplyr::slice_sample(data, n = n_samples, replace = TRUE)
    }

  } else {

    ## Step 1.2: No Filter on Data

    ### Step 1.2.1: Resampling

    #### Step 1.2.1.a: Set default if n_samples is NULL. Default is to multiply the total number of rows in data set by 100.
    if (is.null(n_samples)){
      n_samples <- 100 * nrow(data)
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
  ## Step 3.1: Verify geometry validity
  data_sf <- sf::st_make_valid(data_sf)

  # Step 4: Handle Basemap

  ## Step 4.1: Basemap Present - Match the CRS to basemap
  if (!is.null(basemap)) {
    crs_target <- terra::crs(basemap)
    if (is.na(crs_target)) stop("Basemap CRS is invalid or missing.")
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

  # Step 7: Generate the final plot and resampled data frame as the output of the function.
  return(list(
    plot = p,
    resampled_data = data_proj
  ))
}

# devtools::load_all()  # Use this to reload your package while developing if needed

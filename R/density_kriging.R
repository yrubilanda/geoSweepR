#' @title Kriging-Based Spatial Density Estimation Map
#' @description
#' Create a map of predicted density through kriging
#'
#' This function generates a predicted density map of a categorical value from latitude and longitude coordinates and a list of observations.
#' It can be used to generate kriging results for categorical data that would otherwise be unable to be analyzed.
#' Using this function lets you infer data about missing points to better round out your dataset
#' You can adjust this data to fit the CRS, grid size for analysis, and variogram type to your specific needs.
#'
#' @param data Required. A data frame with your point data. Needs to have latitude and longitude columns.
#' @param lat_col Required. A string for the name of your latitude column
#' @param lon_col Required. A string for the name of your longitude column
#' @param col_data Optional. Name of the column you are using to filter your data. Can be categorical or factor data for filtering purposes.
#' @param filter_by Optional. Name of the value to filter the `col_data` column by values. Only works when `col_data` is also included.
#' @param basemap Optional. A spatial object (e.g., an `sf` or `raster` object) to use as background for plot..
#' @param input_crs Optional. Specify a different EPSG code or PROJ string for CRS in your input data. Default coordinate system is EPSG:4326 (WGS 84).
#' @param grid_size Optional. The size in m^2 of your grids for counting/density. Controls granularity in your model. Default is 500 m.
#' @param variogram_model Optional. A specific vgm type fit for your  data's variogram (e.g., "Gau"). If left blank, it defaults to a spherical model
#'
#' @return A ggplot2 heatmap — you can print it, save it, or add more layers if you want. And a kriged_data object with the data frame.
#' @export
#'
#' @importFrom dplyr filter group_by summarize mutate
#' @importFrom sf st_as_sf st_transform st_make_grid st_sf st_geometry st_area
#' @importFrom gstat variogram vgm fit.variogram krige
#' @importFrom ggplot2 ggplot geom_sf labs scale_fill_viridis_c theme_minimal coord_sf
#'
#' @examples
#' # Sample Data:
#' my_data <- read.csv(system.file("extdata", "dc_sample.csv", package = "geoSweepR"))
#' my_basemap <- terra::rast(system.file("extdata", "my_basemap.tif", package = "geoSweepR"))
#'
#' # Plot density with default grid and variogram model.
#' density_kriging(my_data, lon_col = "longitude", lat_col = "latitude")
#'
#' # Plot density by specifying grid size or variagram model
#' # - Example uses a 250 m^2 grid and a Gaussian model.
#' density_kriging(my_data, lon_col = "longitude", lat_col = "latitude",
#'                 grid_size = 250, variogram_model = "Gau")
#'
#' # Plot density with a filtered data
#' # - Examples uses only "Theft" and optional basemap.
#' density_kriging(my_data, lon_col = "longitude", lat_col = "latitude",
#'                 col_data = "offense", filter_by = "Theft", basemap = my_basemap)

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
  count_sf <- joined |>
    dplyr::group_by(grid_id) |>
    dplyr::summarize(
      count = dplyr::n(),
      geometry = sf::st_geometry(dplyr::first(geometry))
    )

  # Step 8: calculate density (per km²)
  count_sf <- count_sf |>
    dplyr::mutate(
      area_km2 = sf::st_area(geometry) / 10^6,
      density = count / area_km2
    )

  # Step 9: Convert `count_sf` to spatial points (coordinates and crime density)
  # Assuming that `count_sf` has the correct spatial reference (3857), we use st_as_sf
  points_sf <- count_sf |>
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

  ## Step 14.1: Include basemap is specified
  if(!is.null(basemap)){
    p <- p + ggspatial::layer_spatial(basemap)
  }

  ## Step 14.2: Continue building ggplot
  p <- p +
    ggplot2::geom_sf(
      data = kriged,
      ggplot2::aes(fill = var1.pred),
      color = NA,
      alpha = 0.5
    ) +
    ggplot2::scale_fill_viridis_c(option = "cividis") +
    ggplot2::labs(
      title = if (!is.null(filter_by)) paste("Kriged Density Map:", filter_by) else "Kriged Density Map",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()

  ## Step 14.3: Make sure coordinates match the basemap if there is one
  if (!is.null(basemap)) {
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(basemap))
  }

  # Step 15: Return the kriged table and the plot
  return(list(
    kriged_data = kriged,
    plot = p
  ))
}


# devtools::load_all()  # Use this to reload your package while developing if needed

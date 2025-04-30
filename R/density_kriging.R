#' @title Kriging-Based Density Map
#' @description
#' Create a map of predicted density through kriging
#'
#' This function generates a predicted density map of a categorical value from latitude and longitude coordinates and a list of observations.
#' It can be used to generate kriging results for categorical data that would otherwise be unable to be analyzed.
#' Using this function lets you infer data about missing points to better round out your dataset
#' You can adjust this data to fit the CRS, grid size for analysis, and variogram type to your specific needs.
#'
#' @param data A data frame with your point data. Needs to have latitude and longitude columns.
#' @param lat_col A string for the name of your latitude column
#' @param lon_col A string for the name of your longitude column
#' @param col_data Optional. Name of the column you are using to filter your data.
#' @param filter_by Optional. Name of the value to filter the `col_data` column by values.
#' @param basemap Optional. Spatial base-map used for plotting.
#' @param input_crs Optional. Specify the coordinate reference system used in your input data. Default coordinate system is EPSG:4326 (WGS 84).
#' @param grid_size Optional. The size in m^2 of your grids for counting/density. If left blank, it defaults to 500
#' @param variogram_model Optional. A specific vgm type fit for your  data's variogram (e.g., "Gau"). If left blank, it defaults to a spherical model
#'
#' @return A base R density plot with predicted densities of points for each grid — you can print it, save it, or add more layers if you want.
#' @export
#'
#' @importFrom dplyr filter group_by summarize mutate
#' @importFrom sf st_as_sf st_transform st_make_grid st_sf st_geometry st_area
#' @importFrom gstat variogram vgm fit.variogram krige
#'
#' @examples
#' # Plot density in standard grids and variogram types
#' density_kriging(my_data, lon_col = "longitude", lat_col = "latitude", input_crs = 4326)
#'
#'  # Plot density at a different grid size or variagram style - for example 250 m^2 grid and a gaussian variogram
#' density_kriging(my_data, lon_col = "longitude", lat_col = "latitude", input_crs = 4326,
#'  grid_size = 250, variogram_model = "Gau")
#'
#'  # Plot density with a just one kind of data -for example, only "Theft"
#' density_kriging(my_data, lon_col = "longitude", lat_col = "latitude", input_crs = 4326,
#'  col_data = "offense", filter_by = "Theft")

density_kriging  <- function(data, lat_col, lon_col, input_crs = 4326, col_data = NULL, filter_by = NULL, grid_size = 500, variogram_model = "Sph") {
  # Step 1: check if there was a filter parameter set
  if (!is.null(filter_by) && !is.null(col_data)) {
    data <- dplyr::filter(data, .data[[col_data]] == filter_by)
  }
   # Step 2: convert the data into an sf
  d_sf <- sf::st_as_sf(
    data,
    coords = c(lon_col, lat_col),  # tell sf which columns are longitude and latitude
    crs = input_crs,                    # this is the standard GPS coordinate system (WGS 84)
    remove = FALSE                 # keep the original lat/lon columns in the table just in case
  )

  #Step 3: set it in a coordinate system that kriging is compatible with
  d_sf <- sf::st_transform(d_sf, 3857)  # EPSG:3857 Web Mercator

  # Step 4: create a grid over the extent
  # currently set to a 500 meter grid (adjustable)
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

  # Step 9: Convert count_sf to spatial points (coordinates and crime density)
  # Assuming that 'crime_counts' has the correct spatial reference (3857), we use st_as_sf
  points_sf <- count_sf %>%
    sf::st_as_sf(coords = c(lon_col, lat_col), crs = 3857)

  # Step 10: Create a variogram
  # The variogram models the spatial structure of the data
  vgram <- gstat::variogram(density ~ 1, data = points_sf)

  # Step 11: Fit a variogram model (e.g., Spherical model)
  vgram_model <- gstat::fit.variogram(vgram, model = vgm(variogram_model))

  # Step 12: Perform kriging interpolation
  # We will use the 'krige' function to perform the kriging based on the variogram model
  kriged <- gstat::krige(density ~ 1, locations = points_sf, newdata = grid_sf, model = vgram_model)

  # Step 13: View kriged results
  # The 'kriged' object will have the estimated values (crime density) for the grid
  # You can plot the kriged result or inspect the output
  plot(kriged["var1.pred"], main = "Predicted Density")
}


# devtools::load_all()  # Use this to reload your package while developing if needed

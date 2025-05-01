#' @title Kernel Density Heatmap
#' @description
#' Create a 2D Kernel Density Heatmap from Latitude and Longitude with Optional Filter and Basemap
#'
#' This function generates a smooth heatmap from latitude and longitude coordinates.
#' You can use it to map any kind of point data (like crimes, wildlife sightings, etc.).
#' If you want, you can filter the data to show just one type (like only "Robbery" or just "Bird A").
#' You can also include a background map (basemap), but that part is optional.
#'
#' @param data Required. A data frame with your point data. Needs to have latitude and longitude columns.
#' @param lat_col Required. A string for the name of your latitude column.
#' @param lon_col Required. A string for the name of your longitude column.
#' @param col_data Optional. Name of the column you are using to filter your data. Can be categorical or factor data for filtering purposes.
#' @param filter_by Optional. Name of the value to filter the `col_data` column by values. Only works when `col_data` is also included.
#' @param basemap Optional.  A spatial object (e.g., an `sf` or `raster` object) to use as background for plot.
#' @param input_crs Optional. Specify a different EPSG code or PROJ string for CRS used in your input data. Default coordinate system is EPSG:4326 (WGS 84).
#'
#' @return A ggplot2 heatmap — you can print it, save it, or add more layers if you want.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_transform st_coordinates st_crs
#' @importFrom terra crs
#' @importFrom ggplot2 ggplot stat_density_2d aes after_stat scale_fill_viridis_c labs theme_minimal coord_sf
#' @importFrom ggspatial layer_spatial
#'
#' @examples
#' # Sample Data:
#' my_data <- read.csv(system.file("extdata", "dc_sample.csv", package = "geoSweepR"))
#' my_basemap <- terra::rast(system.file("extdata", "my_basemap.tif", package = "geoSweepR"))
#'
#' # Plot all data without filtering.
#' make_heatmap(my_data, lat_col = "latitude", lon_col = "longitude")
#'
#' # Plot filtered data
#' # — Example uses only "Theft".
#' make_heatmap(my_data, lat_col = "latitude", lon_col = "longitude",
#'              col_data = "offense", filter_by = "Theft")
#'
#' # Plot and add a basemap layer underneath.
#' make_heatmap(my_data, lat_col = "latitude", lon_col = "longitude",
#'              basemap = my_basemap)

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

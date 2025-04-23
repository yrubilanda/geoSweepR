#' Create a 2D Kernel Density Heatmap with Optional Filter and Basemap
#'
#' This function generates a smooth heatmap from latitude and longitude coordinates.
#' You can use it to map any kind of point data (like crimes, wildlife sightings, etc.).
#' If you want, you can filter the data to show just one type (like only "Robbery" or just "Bird A").
#' You can also include a background map (basemap), but that part is optional.
#'
#' @param data A data frame with your point data — it needs to include lat/lon columns.
#' @param lat_col A string for the name of your latitude column (e.g., "latitude").
#' @param lon_col A string for the name of your longitude column (e.g., "longitude").
#' @param col_data (Optional) The name of the column you want to filter by (like "offense" or "type").
#' @param filter_by (Optional) A specific value to filter by (e.g., "Robbery"). If left blank, it shows everything.
#' @param basemap (Optional) A background map, like something from CartoDB or OpenStreetMap. Created separately using maptiles.
#'
#' @return A ggplot2 heatmap — you can print it, save it, or add more layers if you want.
#' @export
#'
#' @examples
#' # Plot everything without filtering
#' make_heatmap(my_data, lat_col = "latitude", lon_col = "longitude")
#'
#' # Plot just one type of data — for example, only "Theft"
#' make_heatmap(my_data, lat_col = "latitude", lon_col = "longitude",
#'              col_data = "offense", filter_by = "Theft")
#'
#' # Plot with a basemap underneath
#' make_heatmap(my_data, lat_col = "latitude", lon_col = "longitude",
#'              basemap = my_basemap)

make_heatmap <- function(data, lat_col, lon_col, col_data = NULL, filter_by = NULL, basemap = NULL) {

  # If both a column name and a value to filter by are given, filter the data down to just that value
  if (!is.null(filter_by) && !is.null(col_data)) {
    data <- dplyr::filter(data, .data[[col_data]] == filter_by)
  }

  # Turn your data frame into a spatial object (sf) using latitude and longitude columns
  data_sf <- sf::st_as_sf(
    data,
    coords = c(lon_col, lat_col),  # tell sf which columns are longitude and latitude
    crs = 4326,                    # this is the standard GPS coordinate system (WGS 84)
    remove = FALSE                 # keep the original lat/lon columns in the table just in case
  )

  # If the user provided a basemap, match the coordinate system to the map
  if (!is.null(basemap)) {
    crs_target <- terra::crs(basemap)                    # get the map’s coordinate system
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

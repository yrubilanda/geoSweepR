# geoSweepR

<!-- badges: start -->

<!-- badges: end -->

`geoSweepR` provides tools to generate geospatial heatmaps and apply kriging to point data. It supports optional filtering, resampling for small datasets, specifying Coordinate Reference Systems, and optional basemap overlays.

## Installation

You can install the development version of `geoSweepR` from [GitHub](https://github.com/) using [`pak`](https://pak.r-lib.org/):

``` r
# Option 1: Using pak
pak::pak("yrubilanda/geoSweepR")

# Option 2: Using remotes
# install.packages("remotes")
remotes::install_github("yrubilanda/geoSweepR", build_vignettes = TRUE)
```

## Functions Overview

`geoSweepR` includes the following main functions:

-   `make_heatmap()` - Create a heatmap from geospatial point data.

-   `resample_heatmap()` - Create a heatmap by resampling small geospatial point datasets.

-   `density_kriging()` - Apply ordinary kriging to interpolate spatial point density.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geoSweepR)

# Load sample data
my_data <- read.csv(system.file("extdata", "dc_sample.csv", package = "geoSweepR"))

# Basic heatmap
make_heatmap(
  data = my_data,
  lat_col = "latitude",
  lon_col = "longitude"
)
```

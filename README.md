# geoSweepR

<!-- badges: start -->

<!-- badges: end -->

`geoSweepR` provides tools to generate geospatial heatmaps and apply kriging to point data. It supports optional filtering, resampling for small datasets, specifying Coordinate Reference Systems, and optional basemap overlays.

## Installation

You can install the development version of `geoSweepR` from [GitHub](https://github.com/) using [`pak`](https://pak.r-lib.org/):

``` r
# install.packages("pak")
pak::pak("yrubilanda/geoSweepR")
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
## basic example code
```

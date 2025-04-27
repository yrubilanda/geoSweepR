# June's Scratchpad
##libraries
install.packages("sp")
install.packages("gstat")
library(tidyverse)
library(sp)
library(gstat)
library(sf)
##Step One - loading in the data
d <- read_csv("Data/Clean_CrimeData_2024.csv", col_names = TRUE)
head(d)
class(d)

#creating the variogram
v <- variogram(log()~1, d)

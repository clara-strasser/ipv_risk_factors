######################### IPV Prevalence Mexico Map ############################


# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(sf)
library(ggplot2)
library(viridis)
library(sfheaders)

## Set path -----
path_data <- ""

# Set the SHAPE_RESTORE_SHX config option to YES
sf_options("SHAPE_RESTORE_SHX" = "YES")

## Load data -------
shape_data <- read_sf(dsn = "~/shape_files/00ent.shp")

# Plot -----

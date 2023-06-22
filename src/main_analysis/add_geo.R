############################ Add Spatial Variables #############################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(xtable)
library(psych)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_centroids <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data -------
load(paste0(path_data, "step3_endireh.RData")) # main data
load(paste0(path_centroids, "centroids.RData")) # main data


## Change data name -----
data <- step3_endireh


## Subset not needed -----
data <- data %>%
  select(-c("num_hij_par", "num_hij_par_muj"))


## Add spatial Variables --------


# Municipal Centroid Coordinates
# x  and y



# Municipal and State Factors
# cveent and cvegeo (CVE_ENT + CVE_MUN)
data$cveent <- as.factor(data$CVE_ENT)
data$cvegeo <- as.factor(paste0(data$CVE_ENT,data$CVE_MUN))






################################# Explore Data  #################################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

## Change name ------
data <- data_imp_pmm_m1

## Explore ------
n_distinct(data$ID_PER)
n_distinct(data$ID_VIV)
n_distinct(data$cveent) # 32
n_distinct(data$cvegeo) # 1262












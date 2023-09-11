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
# for non-missing: data <- data_no_imp
# for robustness: data <- data_imp_pmm_m1_rob
## Explore ------
n_distinct(data$ID_PER)
n_distinct(data$ID_VIV)
n_distinct(data$cveent) # 32
n_distinct(data$cvegeo) # 1262

## Explore missings --------
print(data.frame(Variable = names(data), 
                 Count = sapply(data, function(x) sum(is.na(x))), 
                 Percentage = sapply(data, function(x) sum(is.na(x)) / length(x) * 100)))











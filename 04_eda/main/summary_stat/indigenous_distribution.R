############################# Diversity Check ##################################

# Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)

# Set path --------------------------------------------------------------------
path_data <- "/Users/clarastrasser/ipv_data/data/final_data/"

# Load data -------------------------------------------------------------------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

# Change data name ------------------------------------------------------------
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)

# Analysis ---------------------------------------------------------------------

## Distribution
table(data$indigena)
table(data$indigena, data$cveent) # 20, 30, 04










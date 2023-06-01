############################ Societal Risk Factors #########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"

## Load data -------
load(paste0(path, "societal_government.RData"))
load(paste0(path, "societal_public_security.RData"))


## Join -----
societal_risk_factors <- left_join(societal_public_security, societal_government, by = c("CVE_ENT"))


## Save data -----
path_rf <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/risk_factors/"
save(societal_risk_factors, file = paste0(path_rf,"societal_risk_factors.RData"))


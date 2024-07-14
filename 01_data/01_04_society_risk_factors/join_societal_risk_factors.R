############################ Societal Risk Factors #########################


## Load packages ------ --------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

## Set path ----- --------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors/"

## Load data -------------------------------------------------------------------
load(paste0(path, "societal_government.RData"))
load(paste0(path, "societal_public_security.RData"))


## Join ----- ------------------------------------------------------------------
societal_risk_factors <- left_join(societal_public_security, societal_government, by = c("CVE_ENT"))


## Save data ----- -------------------------------------------------------------
path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors/"
save(societal_risk_factors, file = paste0(path_rf,"societal_risk_factors.RData"))


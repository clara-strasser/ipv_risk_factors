#########################  Risk Factors and Outcome ############################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_rf <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/risk_factors/"
path_ipv <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/ipv/"

## Load data -------
load(paste0(path_rf, "risk_factors.RData"))
load(paste0(path_ipv, "emotional_ipv.RData"))

## Join -----

# Join on individual level:
endireh <- risk_factors %>% 
  left_join(emotional_ipv, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

## Save data -----
save(endireh, file = paste0(path,"endireh.RData"))






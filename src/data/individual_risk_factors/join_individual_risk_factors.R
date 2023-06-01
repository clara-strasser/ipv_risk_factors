############################ Individual Risk Factors #########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"

## Load data -------
load(paste0(path, "individual_demographic.RData"))
load(paste0(path, "individual_economic.RData"))
load(paste0(path, "individual_gender_related.RData"))


## Join -----
individual_part1 <- left_join(individual_economic, individual_demographic, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN"))
individual_risk_factors <- left_join(individual_part1, individual_gender_related, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

## Save data -----
path_rf <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/risk_factors/"
save(individual_risk_factors, file = paste0(path_rf,"individual_risk_factors.RData"))













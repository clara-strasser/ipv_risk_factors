############################ Relationship Risk Factors #########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"

## Load data -------
load(paste0(path, "relationship_household.RData"))
load(paste0(path, "relationship_demographic.RData"))
load(paste0(path, "relationship_economic.RData"))
load(paste0(path, "relationship_gender_related.RData"))
load(paste0(path, "relationship_community.RData"))
load(paste0(path, "relationship_womens_role.RData"))


## Join -----
rel_part1 <- left_join(relationship_demographic, relationship_household, by = c("ID_VIV", "CVE_ENT", "CVE_MUN"))
rel_part2 <- left_join(rel_part1, relationship_economic, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))
rel_part3 <- left_join(rel_part2, relationship_gender_related, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))
rel_part4 <- left_join(rel_part3, relationship_community, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))
rel_part5 <- left_join(rel_part4, relationship_womens_role, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


## Save data -----
path_rf <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
save(rel_part5, file = paste0(path_rf,"relationship_risk_factors.RData"))



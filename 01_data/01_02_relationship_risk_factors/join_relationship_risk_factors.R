############################ Relationship Risk Factors #########################


## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/ipv_data/raw_data"
path_data_endireh <- file.path(base_path, "main_source/")
path_data_final <- file.path(path_data_endireh, "risk_factors/")

## Load data -------------------------------------------------------------------
load(paste0(path_data_endireh, "relationship_household.RData"))
load(paste0(path_data_endireh, "relationship_demographic.RData"))
load(paste0(path_data_endireh, "relationship_economic.RData"))
load(paste0(path_data_endireh, "relationship_gender_related.RData"))
load(paste0(path_data_endireh, "relationship_community.RData"))
load(paste0(path_data_endireh, "relationship_womens_role.RData"))


## Join ------------------------------------------------------------------------
rel_part1 <- left_join(relationship_demographic, relationship_household, by = c("ID_VIV", "CVE_ENT", "CVE_MUN"))
rel_part2 <- left_join(rel_part1, relationship_economic, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))
rel_part3 <- left_join(rel_part2, relationship_gender_related, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))
rel_part4 <- left_join(rel_part3, relationship_community, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))
relationship_risk_factors <- left_join(rel_part4, relationship_womens_role, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


## Save data -------------------------------------------------------------------
save(relationship_risk_factors, file = paste0(path_data_final,"relationship_risk_factors.RData"))



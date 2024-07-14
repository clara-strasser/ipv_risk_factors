############################ Individual Risk Factors #########################

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/ipv_data/raw_data"
path_data_endireh <- file.path(base_path, "main_source/")
path_data_final <- file.path(path_data_endireh, "risk_factors/")

# Load data --------------------------------------------------------------------
load(paste0(path_data_final, "individual_demographic.RData"))
load(paste0(path_data_final, "individual_economic.RData"))
load(paste0(path_data_final, "individual_gender_related.RData"))


# Join ------------------------------------------------------------------------
individual_part1 <- left_join(individual_economic, individual_demographic, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN"))
individual_risk_factors <- left_join(individual_part1, individual_gender_related, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

# Save data --------------------------------------------------------------------
save(individual_risk_factors, file = paste0(path_data_final,"individual_risk_factors.RData"))













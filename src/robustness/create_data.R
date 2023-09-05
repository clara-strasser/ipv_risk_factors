#########################  Create Data with Alternative Outcome ##########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/ipv/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path, "emotional_ipv_alternative.RData"))
load(paste0(path_data, "data_imp_pmm_m1.RData"))
load(paste0(path_data, "data_no_imp.RData"))

## Left_join ------

# Imputed
data_imp_pmm_m1_rob <- data_imp_pmm_m1 %>%
  left_join(emotional_ipv_alternative, by=c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

# Non-Imputed
data_no_imp_rob <- data_no_imp %>%
  left_join(emotional_ipv_alternative, by=c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

# Finalize ------

## Save data -----
save(data_imp_pmm_m1_rob, file = paste0(path_data,"data_imp_pmm_m1_rob.RData"))
save(data_no_imp_rob, file = paste0(path_data,"data_no_imp_rob.RData"))



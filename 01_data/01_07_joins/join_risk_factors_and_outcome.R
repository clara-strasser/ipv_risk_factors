#########################  Risk Factors and Outcome ############################

## Load packages --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/data/"
path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors/"
path_ipv <- "/Users/clarastrasser/ipv_data/data/ipv/"

## Load data --------------------------------------------------------------------
load(paste0(path_rf, "risk_factors.RData"))
load(paste0(path_ipv, "emotional_ipv.RData"))

## Join ------------------------------------------------------------------------

# Join on individual level:
endireh <- risk_factors %>% 
  left_join(emotional_ipv, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

## Save data -----
save(endireh, file = paste0(path,"endireh.RData"))






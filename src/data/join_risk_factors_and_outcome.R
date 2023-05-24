#########################  Risk Factors and Outcome ############################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data -------
load(paste0(path, "risk_factors.RData"))
load(paste0(path, "emotional_ipv.RData"))

## Join -----

# Join on individual level:
endireh_2021 <- risk_factors %>% 
  left_join(emotional_ipv, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

## Save data -----
save(endireh_2021, file = paste0(path,"endireh_2021.RData"))






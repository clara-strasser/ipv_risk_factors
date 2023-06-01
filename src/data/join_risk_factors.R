#################################  Risk Factors ###############################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/risk_factors/"

## Load data -------
load(paste0(path, "individual_risk_factors.RData"))
load(paste0(path, "relationship_risk_factors.RData"))
load(paste0(path, "community_risk_factors.RData"))
load(paste0(path, "societal_risk_factors.RData"))


## Join -----

# Join on individual level:
individual_level <- individual_risk_factors %>% 
  left_join(relationship_risk_factors, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

# Join on municipality level:
municipality_level <- individual_level %>%
  left_join(community_risk_factors, by = c("CVE_ENT", "CVE_MUN"))

# Join on state level:
state_level <- municipality_level %>%
  left_join(societal_risk_factors, by = c("CVE_ENT"))

## Save data -----
risk_factors <- state_level
save(risk_factors, file = paste0(path,"risk_factors.RData"))


############################ Community Risk Factors #########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"

## Load data -------
load(paste0(path, "community_demographic.RData"))
load(paste0(path, "community_economic.RData"))
load(paste0(path, "community_public_security.RData"))
load(paste0(path, "community_womens_role.RData"))



## Join -----
comm_part1 <- left_join(community_demographic_part2, community_economic, by = c("CVE_ENT", "CVE_MUN"))
comm_part2 <- left_join(comm_part1, community_women_part2, by = c("CVE_ENT", "CVE_MUN"))
community_risk_factors <- left_join(comm_part2, community_public_part2, by = c("CVE_ENT", "CVE_MUN"))


## Save data -----
path_rf <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
save(community_risk_factors, file = paste0(path_rf,"community_risk_factors.RData"))


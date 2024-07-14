#############################  Finalize Data ##################################

## Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/data/"
path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors/"
path_ipv <- "/Users/clarastrasser/ipv_data/data/ipv/"

## Load data -------------------------------------------------------------------
load(paste0(path, "endireh.RData")) # main data set
load(paste0(path_rf, "additional_risk_factors.RData")) # additional risk factors
load(paste0(path_ipv, "emotional_ipv.RData")) # additional outcome variables

## Join additional risk factors ------------------------------------------------
endireh_2021 <- endireh %>%
  left_join(additional_risk_factors, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


## Join alternative emotional ipv ----------------------------------------------
endireh_2021 <- endireh_2021 %>%
  left_join(emotional_ipv_vida, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


# Subset Main ---------
# Beginning: 110127 observations

## Gender partner:
summary(endireh_2021$par_sex)
#  male    female  NAs 
# 104656    560   4911
endireh_2021 <- subset(endireh_2021, par_sex == "male")

## Married/ Cohabiting Women
table(endireh_2021$T_INSTRUM)
#    A1    A2    B1    B2    C1 
#  65899  2398 12537  9535 14287 
# In total: 68297
endireh_2021 <- subset(endireh_2021, T_INSTRUM == "A1" | T_INSTRUM == "A2")

## Women with children
table(endireh_2021$num_hij)
#  0      >0
# 5145   63152
# In total: 63152
endireh_2021 <- subset(endireh_2021, as.numeric(num_hij) > 0)

# Save data -------------------------------------------------------------------
save(endireh_2021, file = paste0(path,"endireh_2021.RData"))



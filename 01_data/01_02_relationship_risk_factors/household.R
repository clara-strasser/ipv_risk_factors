################## Relationship - Household Risk Factors ######################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/ipv_data/raw_data"
path_data_endireh <- file.path(base_path, "main_source/")
path_data_final <- file.path(path_data_endireh, "risk_factors/")

## Load data ------------------------------------------------------------------
load(paste0(path_data_endireh,"TVIV.RData"))

# Risk Factors -----------------------------------------------------------------

## AVERAGE NUMBER OF HOUSEHOLD MEMBERS PER ROOM IN THE DWELLING -------
# Variable name: hacin
# Questions: P1_7 (how many people in the dwelling) and P1_2 (how many sleeping rooms)
# Outcome: 01 - 99 Persons living in the dwelling (included are children and elder people)
#          01 - 30 Rooms to sleep
# Level: numeric
table(TVIV$P1_7, useNA = "ifany") # No NAs
table(TVIV$P1_2, useNA = "ifany") # No NAs
TVIV$hacin <- as.numeric(as.character(TVIV$P1_7))/as.numeric(as.character(TVIV$P1_2))
head(TVIV[, c("P1_7", "P1_2", "hacin")], n = 35)

# Summary Stat:
table(TVIV$hacin, useNA = "ifany") # No NAs


# Finalize ------

## Keep relevant variables ------
relationship_household <- TVIV %>%
  select(c("ID_VIV", "CVE_ENT", "CVE_MUN", "hacin"))

## Save data -----

save(relationship_household, file = paste0(path_data_final,"relationship_household.RData"))
















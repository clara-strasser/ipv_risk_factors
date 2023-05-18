################## Relationship - Household Risk Factors ######################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

## Load data -------
load(paste0(path,"TVIV.RData"))

# Risk Factors -----

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

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(relationship_household, file = paste0(path_rf,"relationship_household.RData"))
















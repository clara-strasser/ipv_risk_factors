####################### Relationship - Economic Risk Factors ##################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

## Load data -------
load(paste0(path,"TB_SEC_IVaVD.RData"))

# Risk Factors -----

## INCOME PARTNER ------
# Variable name: P4_5_AB
# Outcome: income in pesos
# 00001 - 999996 - income
# 00000 -  no income
# 999997 - more or equal that sum
# 999998 - does not know
# 999999 - not specified
# b      - blank
# Aim: create variable "ing_par" (income partner)
# Remarks: set to NA if >999997
# Set to 0 if man does not work (the reason why NA changes from 47102 to 47351)
table(TB_SEC_IVaVD$P4_5_AB, useNA = "ifany") # 47102 NAs

TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(ing_par = as.numeric(as.character(P4_5_AB)),
         ing_par = ifelse(ing_par > 999997, NA , ing_par),
         ing_par = ifelse(P4_3 == 2, 0, ing_par))

# Summary stat:
table(TB_SEC_IVaVD$ing_par, useNA = "ifany") # 47351 NAs
head(TB_SEC_IVaVD[, c("P4_5_AB", "P4_3", "ing_par")], n = 60)

# Remarks: take into account how often the income is earned
# Variable of relevance: P4_5_1_AB
# Outcomes:       1 - once a week (*4)
#                 2 - every two weeks (*2)
#                 3 - monthly (*1)
#                 8 - does not know (NA)
#                 9 - not specified (NA)
#                 b - blank
# Create variable "ingm_par" (income per month of partner)
table(TB_SEC_IVaVD$P4_5_1_AB, useNA = "ifany") # 63092 NAs
head(TB_SEC_IVaVD[, c("P4_5_AB", "P4_3", "ing_par", "P4_5_1_AB")], n = 60)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(P4_5_1_AB_new = case_when(
    P4_5_1_AB == "1" ~ "4",
    P4_5_1_AB == "2" ~ "2",
    P4_5_1_AB == "3" ~ "1",
    P4_5_1_AB == "8" | P4_5_1_AB == "9" ~ NA_character_,
    TRUE ~ P4_5_1_AB),  # Retain the original value if no condition is matched
    P4_5_1_AB_new = as.numeric(P4_5_1_AB_new),
    ingm_par = ing_par * P4_5_1_AB_new,
    ingm_par = ifelse(ing_par == 0, 0, ingm_par))

# Summary stat:
table(TB_SEC_IVaVD$ingm_par, useNA = "ifany") # 47396 NAs
head(TB_SEC_IVaVD[, c("P4_5_AB", "P4_3", "ing_par", "P4_5_1_AB", "P4_5_1_AB_new", "ingm_par")], n = 60)





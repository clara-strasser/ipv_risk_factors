######################## Individual - Economic Risk Factors ####################

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/ipv_data/raw_data"
path_data_endireh <- file.path(base_path, "main_source/")
path_data_final <- file.path(path_data_endireh, "risk_factors/")

# Load data --------------------------------------------------------------------
load(paste0(path_data_endireh, "TB_SEC_IVaVD.RData"))

# Risk Factors -----------------------------------------------------------------

## INCOME WOMAN ------
# Variable name: P4_2
# Outcome: income in pesos
# 00001 - 999996 - income
# 00000 -  no income
# 999997 - more or equal that sum
# 999998 - does not know
# 999999 - not specified
# b      - blank
# Aim: create variable "ing_muj" (income woman)
# Remarks: set to NA if >999997
# Set to 0 if woman does not work (the reason why NA decreases from 61147 to 3995)
table(TB_SEC_IVaVD$P4_2, useNA = "ifany") # 61147 NAs
table(TB_SEC_IVaVD$P4_1, useNA = "ifany")
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(ing_muj = as.numeric(as.character(P4_2)),
         ing_muj = ifelse(ing_muj > 999997, NA , ing_muj),
         ing_muj = ifelse(P4_1 == 2, 0, ing_muj))

# Summary stat:
table(TB_SEC_IVaVD$ing_muj, useNA = "ifany") # 3995 NAs, 61573 have income of 0
head(TB_SEC_IVaVD[, c("P4_1", "P4_2", "ing_muj")], n = 60)

# Remarks: take into account how often the income is earned
# Variable of relevance: 4_2_1
# Outcomes:       1 - once a week (*4)
#                 2 - every two weeks (*2)
#                 3 - monthly (*1)
#                 8 - does not know (NA)
#                 9 - not specified (NA)
#                 b - blank
# Aim: create variable ingm_muj (income per month of woman)
table(TB_SEC_IVaVD$P4_2_1, useNA = "ifany") # 65568 NAs
head(TB_SEC_IVaVD[, c("P4_1", "P4_2", "ing_muj", "P4_2_1")], n = 60)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(P4_2_1_new = case_when(
    P4_2_1 == "1" ~ "4",
    P4_2_1 == "2" ~ "2",
    P4_2_1 == "3" ~ "1",
    P4_2_1 == "8" | P4_2_1 == "9" ~ NA_character_,
    TRUE ~ P4_2_1),  # Retain the original value if no condition is matched
    P4_2_1_new = as.numeric(P4_2_1_new),
    ingm_muj = ing_muj * P4_2_1_new,
    ingm_muj = ifelse(ing_muj == 0, 0, ingm_muj))

# Summary stat:
table(TB_SEC_IVaVD$ingm_muj, useNA = "ifany") #  4029 NAs (more NAs as P4_2_1 may be 8 or 9)
head(TB_SEC_IVaVD[, c("P4_1", "P4_2", "ing_muj", "P4_2_1",  "P4_2_1_new", "ingm_muj")], n = 500)

# Finalize ---------------------------------------------------------------------

## Keep relevant variables ------
individual_economic <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "ing_muj", "ingm_muj"))

# Save data --------------------------------------------------------------------

save(individual_economic, file = paste0(path_data_final,"individual_economic.RData"))




############################# Additional Risk Factors ##########################


## Load packages ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/raw_data/main_source/"

## Load data -------------------------------------------------------------------
load(paste0(path,"TB_SEC_IVaVD.RData"))

# Risk Factors -----------------------------------------------------------------

## CCT Recipient Women -----
# Variable name: P4_8_6
# Outcome:        1 - yes
#                 2 - no
# Level: no (1), yes (2)
# Aim: create variable "cct_rec" (CCT Receipent)
table(TB_SEC_IVaVD$P4_8_6, useNA = "ifany") # 0 NAs
TB_SEC_IVaVD$cct_rec <- factor(ifelse(TB_SEC_IVaVD$P4_8_6 == "1", "2", "1"), levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$cct_rec, useNA = "ifany") # 0 NAs
head(TB_SEC_IVaVD[, c("P4_8_6", "cct_rec")], n = 35)

# Employment Woman in Lifetime --------
# Variable name: P8_1
# Outcome:      1 - yes
#               2 - no
# Level: no (1), yes (2)
# Aim: create variable "empleo_vida"
table(TB_SEC_IVaVD$P8_1, useNA = "ifany") # 0 NAs --> 21367 never worked!
TB_SEC_IVaVD$empleo_vida <- factor(ifelse(TB_SEC_IVaVD$P8_1 == "1", "2", "1"), levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$empleo_vida, useNA = "ifany") # 0 NAs
#  no   yes 
# 21367 88760 
head(TB_SEC_IVaVD[, c("P8_1", "empleo_vida")], n = 35)


# Employment Woman Last 5 Years --------
# Variable name: P8_2
# Outcome:      1 - yes
#               2 - no
# Level: no (1), yes (2)
# Aim: create variable "empleo_5_años"
table(TB_SEC_IVaVD$P8_2, useNA = "ifany") # 21367 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(empleo_5_años = ifelse(TB_SEC_IVaVD$P8_2 == "1", "2", "1")) %>%
  mutate(empleo_5_años = ifelse(P8_1 == 2, "1", empleo_5_años))
TB_SEC_IVaVD$empleo_5_años <- factor(TB_SEC_IVaVD$empleo_5_años, levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$empleo_5_años, useNA = "ifany") # 0 NAs
#  no   yes 
# 43041 67086 
head(TB_SEC_IVaVD[, c("P8_1", "P8_2", "empleo_5_años")], n = 35)


## Unemployment in the Last 12 Month Women -----
# Variable name: P8_4
# Outcome:     1 - yes
#              2 - no
#              b - blank
# Level: no (1), yes (2)
# Aim: create variable "desempleo"
table(TB_SEC_IVaVD$P8_4, useNA = "ifany") # 43041 NAs
table(TB_SEC_IVaVD$P8_2, useNA = "ifany") # 21367 NAs
table(TB_SEC_IVaVD$empleo_vida, useNA = "ifany") #0 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(desempleo = ifelse(TB_SEC_IVaVD$P8_4 == "2", "2", "1")) %>%
  mutate(desempleo = ifelse(P8_2 == 2, "2", desempleo)) %>%
  mutate(desempleo = ifelse(P8_1 == 2, "2", desempleo))
TB_SEC_IVaVD$desempleo <- factor(TB_SEC_IVaVD$desempleo, levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$desempleo, useNA = "ifany") # 0 NAs
#   no   yes     
#  55328  11758  
head(TB_SEC_IVaVD[, c("P8_1", "P8_2", "P8_4", "desempleo")], n = 35)

## Employment Type Last Job Women -----
# Variable name: P8_5
# Outcome: 1 - empleada
#          2 - obrera
#          3 - jornalera?
#          4 - trabajadora por cuenta propia (nocontrata trabajadores/as)
#          5 - patrona (contrata trabajadores/as)
#          6 - trabajadora sin pago en un negocio familiar o no familiar?
#          b - blanco
# Remark: create new category 7 - sin_empleo in case "desempleo" = yes
# Level: category
# Aim: create variable "tipo_empl" 
table(TB_SEC_IVaVD$P8_5, useNA = "ifany") # 54799 NAs
table(TB_SEC_IVaVD$P8_6_CVE, useNA = "ifany") # 72331 NAs (too many missings!)

TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(tipo_empl = case_when(
    P8_5 == "1" ~ "empleada",
    P8_5 == "2" ~ "obrera",
    P8_5 == "3" ~ "jornalera",
    P8_5 == "4" ~ "trabajadora_pago",
    P8_5 == "5" ~ "patrona",
    P8_5 == "6" ~ "trabajadora_sin_pago",
    TRUE ~ NA_character_
  ))

# Summary Stat:
table(TB_SEC_IVaVD$tipo_empl, useNA = "ifany") # 54799 NAs

# Add new value
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(tipo_empl = ifelse(desempleo == "yes" & is.na(tipo_empl), "desempleo", tipo_empl))

# Summary Stat:
table(TB_SEC_IVaVD$tipo_empl, useNA = "ifany") # 0 NAs

## Living Abroad Childhood Women -----
# Variable name: P12_2
# Outcome: 1 - same state (no)
#          2 - other state (no)
#          3 - US (yes)
#          4 - other country (yes)
# Level: no (1), yes (2)
# Aim: create variable "extr_inf"
table(TB_SEC_IVaVD$P12_2, useNA = "ifany") # 0 NAs
TB_SEC_IVaVD$extr_inf <- factor(ifelse(TB_SEC_IVaVD$P12_2 == "3" | TB_SEC_IVaVD$P12_2 == "4", "2", "1"), levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$extr_inf, useNA = "ifany") # 0 NAs
#   no      yes 
# 109199    928 
head(TB_SEC_IVaVD[, c("P12_2", "extr_inf")], n = 35)


## Existence Previous Marriage/ Cohabiting -------
# Variable name: P13_13
# Outcome: 1,..,9 - number (>=2 yes)
#          b - blank
# Level: no (1), yes (2)
# Aim: create variable "pareja_prev"
table(TB_SEC_IVaVD$P13_13, useNA = "ifany") # 19436 NAs
TB_SEC_IVaVD$pareja_prev <- ifelse(TB_SEC_IVaVD$P13_13 %in% c("2", "3", "4", "5", "6", "7", "8", "9"), "2",
                         ifelse(TB_SEC_IVaVD$P13_13 == "1", "1", NA))
TB_SEC_IVaVD$pareja_prev <- factor(TB_SEC_IVaVD$pareja_prev, levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$pareja_prev, useNA = "ifany") # 19436 NAs
#  no   yes    NA 
# 76453 14238 19436  
head(TB_SEC_IVaVD[, c("P13_13", "pareja_prev", "T_INSTRUM")], n = 65)


## Breakup Due to Drug and Alcohol Use/ Violence Previous Relationship/Marriage -------
# Variable name: P13_16_10, P13_16_11, P13_16_13, P13_16_14
# Question: 
# ¿Dejó de vivir con su anterior esposo o pareja porque…
# él tenía problemas de alcohol o drogas?
# él era grosero o agresivo?
# vivía violencia física?
# vivían violencia sexual?
# Outcome: 0 - not an affirmative response (no)
#          1 - yes (yes)
#          9 - not specified (NA)
#          b - blank (NA)
# Level: no (1), yes (2)
# Aim: create variable "sep_ex"
table(TB_SEC_IVaVD$P13_16_10, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_16_11, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_16_13, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_16_14, useNA = "ifany") # 85137 NAs

# Set columns
sep_razon <- c("P13_16_10", "P13_16_11", "P13_16_13", "P13_16_14") 

# Create columns
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(sep_ex = ifelse(rowSums(select(., any_of(sep_razon)) == "1", na.rm = TRUE) > 0, "2", 
                             ifelse(rowSums(select(., any_of(sep_razon)) == "0", na.rm = TRUE) > 0, "1", NA_character_))) %>%
  mutate(sep_ex = factor(sep_ex, levels = c(1, 2), labels = c("no", "yes")))

# Summary Stat:
table(TB_SEC_IVaVD$sep_ex, useNA = "ifany") # 85224 NAs
head(TB_SEC_IVaVD[, c("P13_16_10", "P13_16_11", "P13_16_13", "P13_16_14", "sep_ex")], n = 65)

# Finalize ------

## Subset data ------
additional_risk_factors <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM",
           "cct_rec","empleo_vida", "empleo_5_años", "tipo_empl", "desempleo", "extr_inf", "pareja_prev",
           "sep_ex"))

## Save data -----

path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors/"
save(additional_risk_factors, file = paste0(path_rf,"additional_risk_factors.RData"))













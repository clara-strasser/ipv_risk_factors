########################## TB_SEC_IVaVD Risk Factors ###########################

# Initiate -----

## Load packages ------
library(dplyr)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------
load(paste0(path,"TB_SEC_IVaVD.RData"))

# Risk Factors -----

## EDUCATION PARTNER -----
# Variable name: P4BC_2
# Outcomes: 00 - no education
#           01 to 11 - different education levels
#           98 - does not know
#           b - blank
# Explanation:  Ninguno                   - Low
#               Pre-Escolar (from 3 to 6) - Low
#               Primaria (from 6 to 12)   - Low
#               Secundaria (12 to 15)     - Medium
#               Preparatoria o bachillerato (15 to 18) - Medium
#               Estudios técnicos (months to 2 years, provide knowledge for particular career path) - Medium
#               Normal con primaria o secundaria (teacher training program for primary and secondary) - Medium
#               Normal licenciatura (teacher training program for higher education level) - High
#               Licenciatura o profesional (undergraduate programs at the university level) - High
#               Posgrado (graduate programs like Masters or PhD) - High
# Aim: create three variables "niv_edlow", "niv_edmedium" and "niv_edhigh"
# Remark: convert outcome "98" into NA
table(TB_SEC_IVaVD$P4BC_2, useNA = "ifany")  # 73389 NAs and 397 "98"
TB_SEC_IVaVD$P4BC_2 <- ifelse(TB_SEC_IVaVD$P4BC_2 == "98", NA, TB_SEC_IVaVD$P4BC_2)
table(TB_SEC_IVaVD$P4BC_2, useNA = "ifany") # 73786 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(P4BC_2= as.numeric(P4BC_2),
         edu_par = ifelse(is.na(P4BC_2), "NA",
                         ifelse(P4BC_2  <= 2, "low",
                                ifelse(P4BC_2  >= 3 & P4BC_2  <= 8, "medium",
                                       ifelse(P4BC_2  >= 9, "high", NA)))))

# Summary Stat:
table(TB_SEC_IVaVD$edu_par)

# Create variables "niv_edlow", "niv_edmedium", "niv_edhigh" and "niv_edNA"
# Levels: 1 (no), 2 (yes)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(edu_parlow = factor(ifelse(edu_par == "low", "yes", "no"), levels = c("no", "yes")),
         edu_parmedium = factor(ifelse(edu_par == "medium", "yes", "no"), levels = c("no", "yes")),
         edu_parhigh = factor(ifelse(edu_par == "high", "yes", "no"), levels = c("no", "yes")),
         edu_parNA = factor(ifelse(edu_par == "NA", "yes", "no"), levels = c("no", "yes")))
table(TB_SEC_IVaVD$edu_parlow)
table(TB_SEC_IVaVD$edu_parmedium)
table(TB_SEC_IVaVD$edu_parhigh)

## INDIGENOUS PARTNER ------
# Variable name: P4BC_3
# Outcomes: 1 - si            - yes
#           2 - si, en parte  - yes
#           3 - no            - no
#           8 - no sabe       - NA
#           b - blank         - NA
table(TB_SEC_IVaVD$P4BC_3, useNA = "ifany")
# Create variable "ind_par"
# Levels: 1 (yes), 2 (no)
TB_SEC_IVaVD$ind_par <- ifelse(TB_SEC_IVaVD$P4BC_3 %in% c("1", "2"), "yes",
                         ifelse(TB_SEC_IVaVD$P4BC_3 == "3", "no",
                                ifelse(TB_SEC_IVaVD$P4BC_3 == "8", NA, NA)))
TB_SEC_IVaVD$ind_par <- factor(TB_SEC_IVaVD$ind_par, levels = c("no", "yes", NA))
# Summary Stat:
table(TB_SEC_IVaVD$ind_par, useNA = "ifany")

## INCOME WOMAN ------
# Variable name: P4_2
# Outcome: income in pesos
# 00001 - 999996 - income
# 00000 -  no income
# 999997 - more or equal that sum
# 999998 - does not know
# 999999 - not specified
# b      - blank
# Create variable "ing_muj" (income woman)
# Remarks: set to NA if >999997
# Set to 0 if woman does not work (the reason why NA decreases from 65142 to 3995)
table(TB_SEC_IVaVD$P4_2, useNA = "ifany")

TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(ing_muj = as.numeric(as.character(P4_2)),
         ing_muj = ifelse(ing_muj > 999997, NA , ing_muj),
         ing_muj = ifelse(P4_1 == 2, 0, ing_muj))
table(TB_SEC_IVaVD$ing_muj, useNA = "ifany")
# Remarks: take into account how often the income is earned
# Variable of relevance: 4_2_1
# Outcomes:       1 - once a week (*4)
#                 2 - every two weeks (*2)
#                 3 - monthly (*1)
#                 8 - does not know (NA)
#                 9 - not specified (NA)
#                 b - blank
# Create variable ingm_muj (income per month of woman)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(P4_2_1 = factor(P4_2_1, levels = c(4, 2, 1, NA, NA)),
         P4_2_1 = as.numeric(as.character(P4_2_1)),
         ingm_muj = ing_muj * P4_2_1,
         ingm_muj = ifelse(ing_muj == 0, 0, ingm_muj))
table(TB_SEC_IVaVD$ingm_muj, useNA = "ifany") #  15395 NAs

## INCOME PARTNER ------
# Variable name: P4_5_AB
# Outcome: income in pesos
# 00001 - 999996 - income
# 00000 -  no income
# 999997 - more or equal that sum
# 999998 - does not know
# 999999 - not specified
# b      - blank
# Create variable "ing_par" (income partner)
# Remarks: set to NA if >999997
# Set to 0 if woman does not work (the reason why NA decreases from 65142 to 3995)
table(TB_SEC_IVaVD$P4_5_AB, useNA = "ifany")

TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(ing_par = as.numeric(as.character(P4_5_AB)),
         ing_par = ifelse(ing_par > 999997, NA , ing_par),
         ing_par = ifelse(P4_3 == 2, 0, ing_par))
table(TB_SEC_IVaVD$ing_par, useNA = "ifany")

# Remarks: take into account how often the income is earned
# Variable of relevance: P4_5_1_AB
# Outcomes:       1 - once a week (*4)
#                 2 - every two weeks (*2)
#                 3 - monthly (*1)
#                 8 - does not know (NA)
#                 9 - not specified (NA)
#                 b - blank
# Create variable "ingm_par" (income per month of partner)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(P4_5_1_AB = factor(P4_5_1_AB, levels = c(4, 2, 1, NA, NA)),
         P4_5_1_AB = as.numeric(as.character(P4_5_1_AB)),
         ingm_par = ing_par * P4_5_1_AB,
         ingm_par = ifelse(ing_par == 0, 0, ingm_par))
table(TB_SEC_IVaVD$ingm_par, useNA = "ifany") # 56889 NAs


## SEXUAL VIOLENCE EXPERIENCE CHILDHOOD WOMAN
# Variable name: P12_14_1 - P12_14_6
# Remark: childhood refers to all experiences <15 years 
# Outcome:
# Levels



## VIOLENCE WITNESS CHILDHOOD WOMAN -------
# Variable name: P12_4 and P12_5
# Remark: types of violence are physical (beating) and 
# emotional (insulting and offending)
# Outcomes:     1 - sometimes
#               2 - more often
#               3 - no
# Labels: 1 (yes), 2 (no)
table(TB_SEC_IVaVD$P12_4, useNA = "ifany") 
table(TB_SEC_IVaVD$P12_5, useNA = "ifany") 
TB_SEC_IVaVD$vio_inf <- factor(ifelse(TB_SEC_IVaVD$P12_4 == "1" | TB_SEC_IVaVD$P12_5 == "1" | TB_SEC_IVaVD$P12_4 == "2" | TB_SEC_IVaVD$P12_5 == "2", "1", "2"), levels = c("1", "2"), labels = c("yes", "no"))
# Summary Stat:
table(TB_SEC_IVaVD$vio_inf, useNA = "ifany") 
#  yes    no 
# 41950 68177 


## VIOLENCE EXPERIENCE CHILDHOOD WOMAN -------
# Variable name: P12_6 and P12_7
# Remark: types of violence are physical (beating) and
# emotional (insulting and offending)
# Outcomes:     1 - sometimes
#               2 - more often
#               3 - no 
# Labels: 1 (yes), 2 (no)
table(TB_SEC_IVaVD$P12_6, useNA = "ifany") 
table(TB_SEC_IVaVD$P12_7, useNA = "ifany") 
TB_SEC_IVaVD$vio_exp_inf <- factor(ifelse(TB_SEC_IVaVD$P12_6 == "1" | TB_SEC_IVaVD$P12_7 == "1" | TB_SEC_IVaVD$P12_7 == "2" | TB_SEC_IVaVD$P12_6 == "2", "1", "2"), levels = c("1", "2"), labels = c("yes", "no"))
# Summary Stat:
table(TB_SEC_IVaVD$vio_exp_inf, useNA = "ifany") 
#  yes    no 
# 42499 67628 

## VIOLENCE EXPERIENCE CHILDHOOD PARTNER -------
# Variable name: P12_8
# Remark: types of violence are physical (beating) and
# emotional (insulting and offending)
# Outcomes:     1 - sometimes
#               2 - more often
#               3 - no 
#               8 - does not know
#               b - blank
# Labels: 1 (yes), 2 (no)
table(TB_SEC_IVaVD$P12_8, useNA = "ifany") 
TB_SEC_IVaVD$P12_8[TB_SEC_IVaVD$P12_8 == "8"] <- NA
TB_SEC_IVaVD$vio_exp_inf_par <- factor(ifelse(TB_SEC_IVaVD$P12_8 == "1" | TB_SEC_IVaVD$P12_8 == "2", "1", "2"), levels = c("1", "2"), labels = c("yes", "no"))
# Summary Stat:
table(TB_SEC_IVaVD$vio_exp_inf_par, useNA = "ifany") 
#  yes      no      NA 
#  30701  46563   32863 


## VIOLENCE WITNESS CHILDHOOD PARTNER -------
# Variable name: P12_9
# Remark: types of violence are physical IPV (father beats mother)
# Outcomes:     1 - yes
#               2 - no
#               8 - does not know
#               b - blank
# Labels: 1 (yes), 2 (no)
table(TB_SEC_IVaVD$P12_9, useNA = "ifany") 
TB_SEC_IVaVD$P12_9[TB_SEC_IVaVD$P12_9 == "8"] <- NA
TB_SEC_IVaVD$vio_inf_par <- factor(ifelse(TB_SEC_IVaVD$P12_9 == "1", "1", "2"), levels = c("1", "2"), labels = c("yes", "no"))
# Summary Stat:
table(TB_SEC_IVaVD$vio_inf_par, useNA = "ifany") 
#  yes    no 
# 41950 68177 










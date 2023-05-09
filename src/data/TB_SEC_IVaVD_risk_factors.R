########################## TB_SEC_IVaVD Risk Factors ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

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


## SEXUAL VIOLENCE EXPERIENCE CHILDHOOD WOMAN --------
# Variable name: P12_14_1 - P12_14_6
# Remark: childhood refers to all experiences <15 years 
# Outcome: 1 - yes (yes)
#          2 - no (no)
#          8 - does not remember (NA)
# Levels: 1 (yes), 2 (no)
cols_sex <- paste0("P12_14_", 1:6) 
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(vio_sex_inf = ifelse(rowSums(select(., any_of(cols_sex)) == "1", na.rm = TRUE) > 0, "1", 
                          ifelse(rowSums(select(., any_of(cols_sex)) == "2", na.rm = TRUE) > 0, "2", NA_character_))) %>%
  mutate(vio_sex_inf = factor(vio_sex_inf, levels = c("1", "2"), labels = c("yes", "no")))
# Summary Stat:
table(TB_SEC_IVaVD$vio_sex_inf, useNA = "ifany")
head(TB_SEC_IVaVD[, c("P12_14_1" ,"P12_14_2" ,"P12_14_3", "P12_14_4", "P12_14_5", "P12_14_6", "vio_sex_inf")], n = 15)
#   yes    no    NA 
#  14166 91038  4923 

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

## NUMBER OF CHILDREN --------
# Variable name: P13_1
# Outcomes: 00 - no children
#           01 - 25 - nr. of children
#           99 - not specified
#           b  - blank
# Create variable "num_hij" (number of children)
table(TB_SEC_IVaVD$P13_1, useNA = "ifany") 
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(num_hij = as.numeric(as.character(P13_1)),
         num_hij = ifelse(num_hij == 99, NA, num_hij))
# Summary stat:
table(TB_SEC_IVaVD$num_hij, useNA = "ifany") 

## AGE AT FIRST CHILD --------
# Variable name: P13_2
# Outcomes: 10 - 55 - age
#           98 - does not remember
#           99 - not specified
#           b  - blank
# Create variable "eda_hij" (age at first child)
table(TB_SEC_IVaVD$P13_2, useNA = "ifany") 
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_hij = as.numeric(as.character(P13_2)),
         eda_hij = ifelse(eda_hij >= 98, NA, eda_hij))
# Summary stat:
table(TB_SEC_IVaVD$eda_hij, useNA = "ifany") 

## GENDER OF PARTNER ----------
# Variable name: P13_5
# Outcomes:   1 - man
#             2 - woman
#             9 - not specified
#             b - blank
# Label: male (1), female (2)
table(TB_SEC_IVaVD$P13_5, useNA = "ifany") # 4849 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(par_sex = factor(ifelse(P13_5 == 1, "male", ifelse(P13_5 == 2, "female", NA)), 
                          levels = c("male", "female")))
table(TB_SEC_IVaVD$par_sex, useNA = "ifany")
# Summary stat:
#   male    female    NA 
#  104656    560     4911 


## AGE AT FIRST SEXUAL INTERCOURSE ---------
# Variable name: P13_6
# Outcomes: 03-97 - age at first sexual intercourse
#           98  - does not remember
#           99  - did not want to respond
#           999 - not specified
#           b   - blank
# Remarks: very young age (03 to 09) in the data set. Reason: experience of sexual
# violence (P12_14_1 to P12_14_6 take values of 1) + did not consent to it (P13_7)
table(TB_SEC_IVaVD$P13_6, useNA = "ifany")
TB_SEC_IVaVD[TB_SEC_IVaVD$P13_6 == "03" & !is.na(TB_SEC_IVaVD$P13_6), ]
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_sex = as.numeric(as.character(P13_6)),
         eda_sex = ifelse(eda_sex >= 98 | eda_sex == 00, NA, eda_sex))
# Summary stat:
table(TB_SEC_IVaVD$eda_sex, useNA = "ifany") 


## CONSENT TO FIRST SEXUAL INTERCOURSE ----------
# Variable name: P13_7
# Outcomes:    1 - yes
#              2 - no
#              9 - not specified
#              b - blank
# Label: yes (1), no (2)
table(TB_SEC_IVaVD$P13_7, useNA = "ifany") # 8644 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(con_sex = factor(ifelse(P13_7 == 1, "yes", ifelse(P13_7 == 2, "no", NA)), 
                          levels = c("yes", "no")))
table(TB_SEC_IVaVD$con_sex, useNA = "ifany")
# Summary stat:
#   yes    no    NA 
#  97675  3787  8665 

## AGE OF ONSET OF CURRENT MARRIAGE/ COHABITING ---------
# Variable name: P13_9
# Outcome: 09, …, 97 - age
#          98 - does not remember
#          99 - not specified
#          b  - blank
# Create variable "eda_mat" (age of marriage or start living together)
table(TB_SEC_IVaVD$P13_9, useNA = "ifany")
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_mat = as.numeric(as.character(P13_9)),
         eda_mat = ifelse(eda_mat >= 98, NA, eda_mat))
# Summary stat:
table(TB_SEC_IVaVD$eda_mat, useNA = "ifany") 

## CONSENT TO MARRIAGE -------
# Variable name: P13_11
# Outcome:  1 - got pregnant and was obligated to marry (2)
#           2 - got pregnant and both decided to marry/live together (1)
#           3 - got "stolen" and was obligated to marry/live together (2)
#           4 - in exchange of presents/properties, the parents decided (2)
#           5 - wanted to leave home (NA)
#           6 - both decided (1)
#           7 - other (NA)
#           b - blank
# Label: yes (1), no (2)
# Create variable "mot_mat" (consent to marriage)
table(TB_SEC_IVaVD$P13_11, useNA = "ifany")
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(mot_mat = factor(ifelse(P13_11 == 2 | P13_11 == 6, "yes", 
                          ifelse(P13_11 == 1 | P13_11 == 3 | P13_11 == 4, "no", NA)), 
                          levels = c("yes", "no")))
# Summary stat: 
table(TB_SEC_IVaVD$mot_mat, useNA = "ifany")
#  yes    no   NA 
# 85460  2240 22427 

## VIOLENCE PREVIOUS PARTNER --------
# Variable name:
# Physical violence: P13_17_1 and P13_17_2
# Emotional violence: P13_17_3
# Sexual violence: P13_17_5
# Economical violence: P13_17_6
# Outcomes:     1 - yes
#               2 - no
#               b - blank
# Label: yes (1), no (2)
table(TB_SEC_IVaVD$P13_17_1, useNA = "ifany")
table(TB_SEC_IVaVD$P13_17_2, useNA = "ifany")
table(TB_SEC_IVaVD$P13_17_3, useNA = "ifany")
table(TB_SEC_IVaVD$P13_17_5, useNA = "ifany")
table(TB_SEC_IVaVD$P13_17_6, useNA = "ifany")

# Set columns
vio_fis <- paste0("P13_17_", 1:2) 
vio_emo <- paste0("P13_17_", 3) 
vio_sex <- paste0("P13_17_", 5) 
vio_eco <- paste0("P13_17_", 6)

# Create columns
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(vio_fis_ex = ifelse(rowSums(select(., any_of(vio_fis)) == "1", na.rm = TRUE) > 0, "1", 
                              ifelse(rowSums(select(., any_of(vio_fis)) == "2", na.rm = TRUE) > 0, "2", NA_character_)),
         vio_emo_ex = ifelse(rowSums(select(., any_of(vio_emo)) == "1", na.rm = TRUE) > 0, "1", 
                              ifelse(rowSums(select(., any_of(vio_emo)) == "2", na.rm = TRUE) > 0, "2", NA_character_)),
         vio_sex_ex = ifelse(rowSums(select(., any_of(vio_sex)) == "1", na.rm = TRUE) > 0, "1", 
                              ifelse(rowSums(select(., any_of(vio_sex)) == "2", na.rm = TRUE) > 0, "2", NA_character_)),
         vio_eco_ex = ifelse(rowSums(select(., any_of(vio_eco)) == "1", na.rm = TRUE) > 0, "1", 
                              ifelse(rowSums(select(., any_of(vio_eco)) == "2", na.rm = TRUE) > 0, "2", NA_character_))) %>%
  mutate(vio_fis_ex = factor(vio_fis_ex, levels = c("1", "2"), labels = c("yes", "no")),
         vio_emo_ex = factor(vio_emo_ex, levels = c("1", "2"), labels = c("yes", "no")),
         vio_sex_ex = factor(vio_sex_ex, levels = c("1", "2"), labels = c("yes", "no")),
         vio_eco_ex = factor(vio_eco_ex, levels = c("1", "2"), labels = c("yes", "no")))
# Summary stat:
table(TB_SEC_IVaVD$vio_fis_ex, useNA = "ifany")
table(TB_SEC_IVaVD$vio_emo_ex, useNA = "ifany")
table(TB_SEC_IVaVD$vio_sex_ex, useNA = "ifany")
table(TB_SEC_IVaVD$vio_eco_ex, useNA = "ifany")


## WOMANS LEVEL AUTONOMY ABOUT SEX LIFE ------
# Variable name: P15_1AB_12 - P15_1AB_17
# Questions:
# cuándo tener relaciones sexuales?
# si se usan anticonceptivos?
# sobre el cuidado de su salud sexual y reproductiva? (*new question!)
# quién debe usar los métodos anticonceptivos?
# tener o no hijos(as)?
# cuándo y cuántos hijos(as) tener?
# Outcome:       1 - only her (high)
#                2 - only him (low)
#                3 - both but him a bit more (low)
#                4 - both but her a bit more (high)
#                5 - both equally (medium)
#                6 - other people (low)
#                7 - does not apply (only in the case of ex-husband) (NA)
#                b - blank (NA)
# Label: high, medium, low
table(TB_SEC_IVaVD$P15_1AB_12, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_13, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_14, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_15, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_16, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_17, useNA = "ifany")

# Create variables "lib_sex_gradlow", "lib_sex_gradmedium" and "lib_sex_gradhigh"
lib_sex <- paste0("P15_1AB_", 12:17)

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(lib_sex)), ~recode(.,
                                           "2" = "low",
                                           "3" = "low",
                                           "6" = "low",
                                           "4" = "high",
                                           "1" = "high",
                                           "5" = "medium",
                                           "7" = NA_character_)) %>%
  mutate_at(vars(any_of(lib_sex)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    high_lib_sex = rowSums(select(., all_of(lib_sex)) == "high", na.rm = TRUE),
    medium_lib_sex = rowSums(select(., all_of(lib_sex)) == "medium", na.rm = TRUE),
    low_lib_sex = rowSums(select(., all_of(lib_sex)) == "low", na.rm = TRUE)
  )
head(TB_SEC_IVaVD[, c("P15_1AB_12", "P15_1AB_13" ,"P15_1AB_14", "P15_1AB_15", "P15_1AB_16", "P15_1AB_17", "high_lib_sex", "medium_lib_sex", "low_lib_sex")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(lib_sex_grad = "medium",
         lib_sex_grad = ifelse(high_lib_sex > pmax(medium_lib_sex, low_lib_sex), "high",
                                ifelse(low_lib_sex > pmax(high_lib_sex, medium_lib_sex), "low",
                                        lib_sex_grad)),
         lib_sex_grad = ifelse(high_lib_sex == 0 & medium_lib_sex == 0 & low_lib_sex == 0, NA_character_, lib_sex_grad))
head(TB_SEC_IVaVD[, c("P15_1AB_12", "P15_1AB_13" ,"P15_1AB_14", "P15_1AB_15", "P15_1AB_16", "P15_1AB_17", "high_lib_sex", "medium_lib_sex", "low_lib_sex", "lib_sex_grad")], n = 35)
# Remarks: in case "high" and "low" equal, it is set to "medium"

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(lib_sex_gradhigh = ifelse(lib_sex_grad == "high", "yes", "no"),
         lib_sex_gradmedium = ifelse(lib_sex_grad == "medium", "yes", "no"),
         lib_sex_gradlow = ifelse(lib_sex_grad == "low", "yes", "no"),
         lib_sex_gradNA = ifelse(is.na(lib_sex_grad), "yes", "no")) %>%
  mutate(lib_sex_gradhigh = factor(lib_sex_gradhigh, levels = c("no", "yes")),
         lib_sex_gradmedium = factor(lib_sex_gradmedium, levels = c("no", "yes")),
         lib_sex_gradlow = factor(lib_sex_gradlow, levels = c("no", "yes")),
         lib_sex_gradNA = factor(lib_sex_gradNA, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("P15_1AB_12", "P15_1AB_13" ,"P15_1AB_14", "P15_1AB_15", "P15_1AB_16", "P15_1AB_17", "lib_sex_grad", "lib_sex_gradhigh", "lib_sex_gradmedium", "lib_sex_gradlow", "lib_sex_gradNA")], n = 35)

## WOMANS LEVEL AUTONOMY ABOUT LIFE AND ECONOMIC RESOURCES ------
# Variable name: P15_1AB_1, P15_1AB_3 and P15_1AB_4
# Questions:
# si usted puede trabajar o estudiar?
# qué hacer con el dinero que usted gana o del que dispone?
# si puede comprar cosas para usted?
# Outcome:       1 - only her (high)
#                2 - only him (low)
#                3 - both but him a bit more (low)
#                4 - both but her a bit more (high)
#                5 - both equally (medium)
#                6 - other people (low)
#                7 - does not apply (only in the case of ex-husband) (NA)
#                b - blank (NA)
# Label: high, medium, low
table(TB_SEC_IVaVD$P15_1AB_1, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_3, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_4, useNA = "ifany")

# Create variables "lib_eco_gradlow", "lib_eco_gradmedium" and "lib_eco_gradhigh"
lib_eco <- paste0("P15_1AB_", c(1, 3:4))  

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(lib_eco)), ~recode(.,
                                           "2" = "low",
                                           "3" = "low",
                                           "6" = "low",
                                           "4" = "high",
                                           "1" = "high",
                                           "5" = "medium",
                                           "7" = NA_character_)) %>%
  mutate_at(vars(any_of(lib_eco)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    high_lib_eco = rowSums(select(., all_of(lib_eco)) == "high", na.rm = TRUE),
    medium_lib_eco = rowSums(select(., all_of(lib_eco)) == "medium", na.rm = TRUE),
    low_lib_eco = rowSums(select(., all_of(lib_eco)) == "low", na.rm = TRUE)
  )
head(TB_SEC_IVaVD[, c("P15_1AB_1", "P15_1AB_3", "P15_1AB_4", "high_lib_eco", "medium_lib_eco", "low_lib_eco")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(lib_eco_grad = "medium",
         lib_eco_grad = ifelse(high_lib_eco > pmax(medium_lib_eco, low_lib_eco), "high",
                               ifelse(low_lib_eco > pmax(high_lib_eco, medium_lib_eco), "low",
                                      lib_eco_grad)),
         lib_eco_grad = ifelse(high_lib_eco == 0 & medium_lib_eco == 0 & low_lib_eco == 0, NA_character_, lib_eco_grad))
head(TB_SEC_IVaVD[, c("P15_1AB_1", "P15_1AB_3", "P15_1AB_4", "high_lib_eco", "medium_lib_eco", "low_lib_eco", "lib_eco_grad")], n = 35)
# Remarks: in case "high" and "low" equal, it is set to "medium"

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(lib_eco_gradhigh = ifelse(lib_eco_grad == "high", "yes", "no"),
         lib_eco_gradmedium = ifelse(lib_eco_grad == "medium", "yes", "no"),
         lib_eco_gradlow = ifelse(lib_eco_grad == "low", "yes", "no"),
         lib_eco_gradNA = ifelse(is.na(lib_eco_grad), "yes", "no")) %>%
  mutate(lib_eco_gradhigh = factor(lib_eco_gradhigh, levels = c("no", "yes")),
         lib_eco_gradmedium = factor(lib_eco_gradmedium, levels = c("no", "yes")),
         lib_eco_gradlow = factor(lib_eco_gradlow, levels = c("no", "yes")),
         lib_eco_gradNA = factor(lib_eco_gradNA, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("P15_1AB_1", "P15_1AB_3", "P15_1AB_4", "lib_eco_grad", "lib_eco_gradhigh", "lib_eco_gradmedium", "lib_eco_gradlow","lib_eco_gradNA" )], n = 35)


## WOMANS LEVEL AUTONOMY ABOUT SOCIAL AND POLITICAL ACTIVITIES ------
# Variable name: P15_1AB_2, P15_1AB_5, P15_1AB_6 and P15_1AB_9
# Questions:
# si usted puede salir de su casa?
# cuando usted quiere o tiene interés en participar en la vida social de su comunidad?
# cuando usted quiere o tiene interés en participar en la vida política de su comunidad?
# sobre el tipo de ropa y arreglo personal para usted?
# Outcome:       1 - only her (high)
#                2 - only him (low)
#                3 - both but him a bit more (low)
#                4 - both but her a bit more (high)
#                5 - both equally (medium)
#                6 - other people (low)
#                7 - does not apply (only in the case of ex-husband) (NA)
#                b - blank (NA)
# Label: high, medium, low
# Remarks*: new question added (P15_1AB_5 and P15_1AB_6 were before one question P14_1AB_5) + question P15_1AB_9 (before P14_1AB_8) was not included
table(TB_SEC_IVaVD$P15_1AB_2, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_5, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_6, useNA = "ifany")
table(TB_SEC_IVaVD$P15_1AB_9, useNA = "ifany")

# Create variables "lib_soc_gradlow", "lib_soc_gradmedium" and "lib_soc_gradhigh"
lib_soc <- paste0("P15_1AB_", c(2, 5, 6, 9))

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(lib_soc)), ~recode(.,
                                           "2" = "low",
                                           "3" = "low",
                                           "6" = "low",
                                           "4" = "high",
                                           "1" = "high",
                                           "5" = "medium",
                                           "7" = NA_character_)) %>%
  mutate_at(vars(any_of(lib_soc)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    high_lib_soc = rowSums(select(., all_of(lib_soc)) == "high", na.rm = TRUE),
    medium_lib_soc = rowSums(select(., all_of(lib_soc)) == "medium", na.rm = TRUE),
    low_lib_soc = rowSums(select(., all_of(lib_soc)) == "low", na.rm = TRUE)
  )
head(TB_SEC_IVaVD[, c("P15_1AB_2", "P15_1AB_5", "P15_1AB_6", "P15_1AB_9", "high_lib_soc", "medium_lib_soc", "low_lib_soc")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(lib_soc_grad = "medium",
         lib_soc_grad = ifelse(high_lib_soc > pmax(medium_lib_soc, low_lib_soc), "high",
                               ifelse(low_lib_soc > pmax(high_lib_soc, medium_lib_soc), "low",
                                      lib_soc_grad)),
         lib_soc_grad = ifelse(high_lib_soc == 0 & medium_lib_soc == 0 & low_lib_soc == 0, NA_character_, lib_soc_grad))
head(TB_SEC_IVaVD[, c("P15_1AB_2", "P15_1AB_5", "P15_1AB_6", "P15_1AB_9", "high_lib_soc", "medium_lib_soc", "low_lib_soc", "lib_soc_grad")], n = 35)
# Remarks: in case "high" and "low" equal, it is set to "medium"

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(lib_soc_gradhigh = ifelse(lib_soc_grad == "high", "yes", "no"),
         lib_soc_gradmedium = ifelse(lib_soc_grad == "medium", "yes", "no"),
         lib_soc_gradlow = ifelse(lib_soc_grad == "low", "yes", "no"),
         lib_soc_gradNA = ifelse(is.na(lib_soc_grad), "yes", "no")) %>%
  mutate(lib_soc_gradhigh = factor(lib_soc_gradhigh, levels = c("no", "yes")),
         lib_soc_gradmedium = factor(lib_soc_gradmedium, levels = c("no", "yes")),
         lib_soc_gradlow = factor(lib_soc_gradlow, levels = c("no", "yes")),
         lib_soc_gradNA = factor(lib_soc_gradNA, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("P15_1AB_2", "P15_1AB_5", "P15_1AB_6", "P15_1AB_9", "lib_soc_grad", "lib_soc_gradhigh", "lib_soc_gradmedium", "lib_soc_gradlow","lib_soc_gradNA" )], n = 35)

## PRO-GENDER EQUALITY ATTITUDE WOMEN -----
# Variable name: P6_1_1 - P6_1_3 and P6_1_5 and P6_2_1 - P6_2_4
# Questions I:
# Quién cree usted que debe ser responsable del cuidado de los hijos(as), de las personas enfermas y ancianas?
# Quién cree usted que debe ganar más salario en el trabajo?
# Quién cree usted que debe ser el responsable de las tareas de la casa?
# Quién cree usted que debe ser el responsable de traer dinero para la casa? (*was exlcluded!)
# Quién cree usted que tiene mayor capacidad para trabajar y/o estudiar?
# Outcome:       1 - women
#                2 - man
#                3 - both / both earn the same
# Questions II: 
# Está usted de acuerdo en que hombres y mujeres tienen el mismo derecho a salir por las noches a divertirse?
# Está usted de acuerdo en que las mujeres que tienen hijos(as) trabajen, aún si no tienen necesidad de hacerlo?
# Está usted de acuerdo en que las mujeres que se visten con escotes provocan que los hombres las molesten?
# Está usted de acuerdo en que las mujeres casadas deben tener relaciones sexuales con su esposo cuando él quiera?
# Outcome:       1 - yes
#                2 - no
table(TB_SEC_IVaVD$P6_1_1, useNA = "ifany")
TB_SEC_IVaVD$P6_1_1 <- factor(ifelse(TB_SEC_IVaVD$P6_1_1 == "1", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_1_2, useNA = "ifany")
TB_SEC_IVaVD$P6_1_2 <- factor(ifelse(TB_SEC_IVaVD$P6_1_2 == "2", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_1_3, useNA = "ifany")
TB_SEC_IVaVD$P6_1_3 <- factor(ifelse(TB_SEC_IVaVD$P6_1_3 == "1", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_1_5, useNA = "ifany")
TB_SEC_IVaVD$P6_1_5 <- factor(ifelse(TB_SEC_IVaVD$P6_1_5 == "2", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_2_1, useNA = "ifany")
TB_SEC_IVaVD$P6_2_1 <- factor(ifelse(TB_SEC_IVaVD$P6_2_1 == "2", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_2_2, useNA = "ifany")
TB_SEC_IVaVD$P6_2_2 <- factor(ifelse(TB_SEC_IVaVD$P6_2_2 == "2", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_2_3, useNA = "ifany")
TB_SEC_IVaVD$P6_2_3 <- factor(ifelse(TB_SEC_IVaVD$P6_2_3 == "1", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

table(TB_SEC_IVaVD$P6_2_4, useNA = "ifany")
TB_SEC_IVaVD$P6_2_4 <- factor(ifelse(TB_SEC_IVaVD$P6_2_4 == "1", "1", "2"), levels = c("1", "2"), labels = c("not feminist", "feminist"))

# Set columns
fem_op <- c("P6_1_1", "P6_1_2", "P6_1_3", "P6_1_5", "P6_2_1", "P6_2_2", "P6_2_3", "P6_2_4")

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    feminist = rowSums(select(., all_of(fem_op)) == "feminist", na.rm = TRUE),
    not_feminist = rowSums(select(., all_of(fem_op)) == "not feminist", na.rm = TRUE)
  )
head(TB_SEC_IVaVD[, c("P6_1_1", "P6_1_2", "P6_1_3", "P6_1_5", "P6_2_1", "P6_2_2", "P6_2_3", "P6_2_4", "feminist", "not_feminist")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(feminist_grad = ifelse(feminist >= 6, "high",
                               ifelse(feminist <= 2, "low",
                                      "medium")))
head(TB_SEC_IVaVD[, c("P6_1_1", "P6_1_2", "P6_1_3", "P6_1_5", "P6_2_1", "P6_2_2", "P6_2_3", "P6_2_4", "feminist", "not_feminist", "feminist_grad")], n = 35)
# Remarks: set same threshold as authors, only difference: here 8 questions instead of 9

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(feminist_gradhigh = ifelse(feminist_grad == "high", "yes", "no"),
         feminist_gradmedium = ifelse(feminist_grad == "medium", "yes", "no"),
         feminist_gradlow = ifelse(feminist_grad == "low", "yes", "no")) %>%
  mutate(feminist_gradhigh = factor(feminist_gradhigh, levels = c("no", "yes")),
         feminist_gradmedium = factor(feminist_gradmedium, levels = c("no", "yes")),
         feminist_gradlow = factor(feminist_gradlow, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("feminist", "not_feminist", "feminist_grad", "feminist_gradhigh", "feminist_gradmedium", "feminist_gradlow")], n = 35)

## WOMENS PERCEPTION ABOUT SUPPORT FROM SOCIAL NETWORKS --------
# Variable name: P16_3_1_1, P16_3_2_1, P16_3_3_1, P16_3_4_1, P16_3_5_1, P16_3_6_1
# Questions:
# cuidar un rato a sus hijas/hijos cuando tiene alguna emergencia o se enferman?
# hacer alguna tarea o labor?
# cuando usted se enferma?
# para platicar de sus problemas o preocupaciones?
# consejos u orientación cuando tiene dificultades con su esposo o pareja?
# cuando tiene alguna dificultad o problema económico?
# Outcome:       1 - neighbor
#                2 - friend
#                3 - college
#                4 - family
#                5 - someone else
#                6 - no one    
redsoc <- paste0("P16_3_", 1:6, "_1")

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(redsoc)), ~recode(.,
                                           "1" = "yes",
                                           "2" = "yes",
                                           "3" = "yes",
                                           "4" = "yes",
                                           "5" = "yes",
                                           "6" = "no")) %>%
  mutate_at(vars(any_of(redsoc)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    red_soc_yes = rowSums(select(., all_of(redsoc)) == "yes", na.rm = TRUE),
    red_soc_no = rowSums(select(., all_of(redsoc)) == "no", na.rm = TRUE))
head(TB_SEC_IVaVD[, c("P16_3_1_1", "P16_3_2_1", "P16_3_3_1", "P16_3_4_1", "P16_3_5_1", "P16_3_6_1", "red_soc_yes", "red_soc_no")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(redsoc_grad = ifelse(red_soc_yes >= 5, "high",
                                ifelse(red_soc_yes <= 1, "low",
                                       "medium")))
head(TB_SEC_IVaVD[, c("P16_3_1_1", "P16_3_2_1", "P16_3_3_1", "P16_3_4_1", "P16_3_5_1", "P16_3_6_1", "red_soc_yes", "red_soc_no", "redsoc_grad" )], n = 35)
# Remark: same thresholds as authors, same number of columns

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(redsoc_gradhigh = ifelse(redsoc_grad == "high", "yes", "no"),
         redsoc_gradmedium = ifelse(redsoc_grad == "medium", "yes", "no"),
         redsoc_gradlow = ifelse(redsoc_grad == "low", "yes", "no")) %>%
  mutate(redsoc_gradhigh = factor(redsoc_gradhigh, levels = c("no", "yes")),
         redsoc_gradmedium = factor(redsoc_gradmedium, levels = c("no", "yes")),
         redsoc_gradlow = factor(redsoc_gradlow, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("red_soc_yes", "red_soc_no", "redsoc_grad", "redsoc_gradhigh", "redsoc_gradmedium", "redsoc_gradlow")], n = 35)
















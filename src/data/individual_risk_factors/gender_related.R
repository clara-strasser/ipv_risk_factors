#################### Individual - Gender Related Risk Factors ##################

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

## AGE AT FIRST CHILD --------
# Variable name: P13_2
# Outcomes: 10 - 55 - age
#           98 - does not remember
#           99 - not specified
#           b  - blank
# Aim: create variable "eda_hij" (age at first child)
table(TB_SEC_IVaVD$P13_2, useNA = "ifany") # 22169 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_hij = as.numeric(as.character(P13_2)),
         eda_hij = ifelse(eda_hij >= 98, NA, eda_hij))

# Summary stat:
table(TB_SEC_IVaVD$eda_hij, useNA = "ifany") # 22965 NAs
head(TB_SEC_IVaVD[, c("P13_2", "eda_hij")], n = 60)


## AGE AT FIRST SEXUAL INTERCOURSE ---------
# Variable name: P13_6
# Outcomes: 03-97 - age at first sexual intercourse
#           98  - does not remember
#           99  - did not want to respond
#           999 - not specified
#           b   - blank
# Aim: create variable "eda_sex"
# Remarks: very young age (03 to 09) in the data set. Reason: experience of sexual
# violence (P12_14_1 to P12_14_6 take values of 1) + did not consent to it (P13_7)
table(TB_SEC_IVaVD$P13_6, useNA = "ifany") # 4849 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_sex = as.numeric(as.character(P13_6)),
         eda_sex = ifelse(eda_sex >= 98 | eda_sex == 00, NA, eda_sex))

# Summary stat:
table(TB_SEC_IVaVD$eda_sex, useNA = "ifany") # 11682 NAs
head(TB_SEC_IVaVD[, c("P13_6", "eda_sex")], n = 60)


## CONSENT TO FIRST SEXUAL INTERCOURSE ----------
# Variable name: P13_7
# Outcomes:    1 - yes
#              2 - no
#              9 - not specified
#              b - blank
# Label: no (1), yes (2)
# Aim: create variable "con_sex"
table(TB_SEC_IVaVD$P13_7, useNA = "ifany") # 8644 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(con_sex = factor(ifelse(P13_7 == 2, "1", ifelse(P13_7 == 1, "2", NA)), 
                          levels =c(1, 2), labels = c("no", "yes")))

# Summary stat:
table(TB_SEC_IVaVD$con_sex, useNA = "ifany") # 8665 NAs
head(TB_SEC_IVaVD[, c("P13_7", "con_sex")], n = 80)
#  no   yes    NA 
# 3787 97675  8665


## AGE OF ONSET OF CURRENT MARRIAGE/ COHABITING ---------
# Variable name: P13_9
# Outcome: 09, …, 97 - age
#          98 - does not remember
#          99 - not specified
#          b  - blank
# Aim: create variable "eda_mat" (age of marriage or start living together)
table(TB_SEC_IVaVD$P13_9, useNA = "ifany") # 19436 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_mat = as.numeric(as.character(P13_9)),
         eda_mat = ifelse(eda_mat >= 98, NA, eda_mat))

# Summary stat:
table(TB_SEC_IVaVD$eda_mat, useNA = "ifany") # 20812 NAs
head(TB_SEC_IVaVD[, c("P13_9", "eda_mat")], n = 80)

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
# Label: no (1), yes (2)
# Aim: create variable "mot_mat" (consent to marriage)
table(TB_SEC_IVaVD$P13_11, useNA = "ifany") # 19436 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(mot_mat = factor(ifelse(P13_11 == 1 | P13_11 == 3 | P13_11 == 4, "1", 
                                 ifelse(P13_11 == 2 | P13_11 == 6, "2", NA_character_)), 
                          levels = c(1, 2), labels = c("no", "yes")))
# Summary stat: 
table(TB_SEC_IVaVD$mot_mat, useNA = "ifany") # 22427 NAs
#   no   yes   NA 
#  2240 85460 22427
head(TB_SEC_IVaVD[, c("P13_11", "mot_mat")], n = 100)


## NUMBER OF CHILDREN --------
# Variable name: P13_1
# Outcomes: 00 - no children
#           01 - 25 - nr. of children
#           99 - not specified
#           b  - blank
# Aim: create variable "num_hij" (number of children)
table(TB_SEC_IVaVD$P13_1, useNA = "ifany") # 4849 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(num_hij = as.numeric(as.character(P13_1)),
         num_hij = ifelse(num_hij == 99, NA, num_hij))

# Summary stat:
table(TB_SEC_IVaVD$num_hij, useNA = "ifany") # 4911 NAs
head(TB_SEC_IVaVD[, c("P13_1", "num_hij")], n = 100)


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


## VIOLENCE WITNESS CHILDHOOD WOMAN -------
# Variable name: P12_4 and P12_5
# Remark: types of violence are physical (beating) and 
# emotional (insulting and offending)
# Outcomes:     1 - sometimes
#               2 - more often
#               3 - no
# Labels: 1 (no), 2 (yes)
# Aim: create variable "vio_inf"
table(TB_SEC_IVaVD$P12_4, useNA = "ifany") # 0 NAs
table(TB_SEC_IVaVD$P12_5, useNA = "ifany")# 0 NAs
TB_SEC_IVaVD$vio_inf <- factor(ifelse(TB_SEC_IVaVD$P12_4 == "1" | TB_SEC_IVaVD$P12_5 == "1" | TB_SEC_IVaVD$P12_4 == "2" | TB_SEC_IVaVD$P12_5 == "2", "2", "1"), levels = c("1", "2"), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$vio_inf, useNA = "ifany") # 0 NAs
head(TB_SEC_IVaVD[, c("P12_4", "P12_5", "vio_inf")], n = 35)
#   no   yes 
# 68177 41950  


## VIOLENCE EXPERIENCE CHILDHOOD WOMAN -------
# Variable name: P12_6 and P12_7
# Remark: types of violence are physical (beating) and
# emotional (insulting and offending)
# Outcomes:     1 - sometimes
#               2 - more often
#               3 - no 
# Labels: 1 (no), 2 (yes)
# Aim: create variable "vio_exp_inf"
table(TB_SEC_IVaVD$P12_6, useNA = "ifany") # 0 NAs
table(TB_SEC_IVaVD$P12_7, useNA = "ifany") # 0 NAs
TB_SEC_IVaVD$vio_exp_inf <- factor(ifelse(TB_SEC_IVaVD$P12_6 == "1" | TB_SEC_IVaVD$P12_7 == "1" | TB_SEC_IVaVD$P12_7 == "2" | TB_SEC_IVaVD$P12_6 == "2", "2", "1"), levels = c("1", "2"), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$vio_exp_inf, useNA = "ifany") # 0 NAs
head(TB_SEC_IVaVD[, c("P12_6", "P12_7", "vio_exp_inf")], n = 35)
#  no     yes 
# 67628  42499  


## SEXUAL VIOLENCE EXPERIENCE CHILDHOOD WOMAN --------
# Variable name: P12_14_1 - P12_14_6
# Remark: childhood refers to all experiences <15 years 
# Outcome: 1 - yes (yes)
#          2 - no (no)
#          8 - does not remember (NA)
# Levels: 1 (no), 2 (yes)
cols_sex <- paste0("P12_14_", 1:6) 
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(vio_sex_inf = ifelse(rowSums(select(., any_of(cols_sex)) == "1", na.rm = TRUE) > 0, "1", 
                              ifelse(rowSums(select(., any_of(cols_sex)) == "2", na.rm = TRUE) > 0, "2", NA_character_))) %>%
  mutate(vio_sex_inf = factor(vio_sex_inf, levels = c("2", "1"), labels = c("no", "yes")))

# Summary Stat:
table(TB_SEC_IVaVD$vio_sex_inf, useNA = "ifany") # 4923 NAs
head(TB_SEC_IVaVD[, c("P12_14_1" ,"P12_14_2" ,"P12_14_3", "P12_14_4", "P12_14_5", "P12_14_6", "vio_sex_inf")], n = 60)
#  no    yes    NA 
# 91038 14166  4923



# Finalize ------

## Keep relevant variables ------
individual_gender_related <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "vio_inf", "vio_exp_inf", "vio_sex_inf", "num_hij", "eda_hij", "eda_sex", "con_sex", "eda_mat", "mot_mat", "feminist_gradhigh", "feminist_gradmedium", "feminist_gradlow"))

## Save data -----
path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(individual_gender_related, file = paste0(path_rf,"individual_gender_related.RData"))








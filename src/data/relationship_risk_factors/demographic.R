####################### Relationship - Demographic Risk Factors ##################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

## Load data -------
load(paste0(path,"TSDem.RData"))
load(paste0(path,"TB_SEC_IVaVD.RData"))

# Risk Factors -----

## AGE WOMAN ----
# Variable name: EDAD
# Outcomes: 00 - less than 1 year
#           01 to 96 - age in years
#           97 - more than 97 years old
#           98 and 99 - age not specified
table(TSDem$EDAD, useNA = "ifany") # no NAs
TSDem <- TSDem %>%
  mutate(EDAD = ifelse(EDAD>=97, NA, as.numeric(EDAD))) # set to NA if age not specified and age>=97

# Summary Stat:
table(TSDem$EDAD, useNA = "ifany") # 956 NAs
head(TSDem[, c("EDAD")], n = 60)


## AGE WOMAN OF ONSET OF CURRENT MARRIAGE/ COHABITING ---------
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
head(TB_SEC_IVaVD[, c("P13_9", "eda_mat")], n = 60)


## AGE PARTNER OF ONSET OF CURRENT MARRIAGE ---------
# Variable name: P13_10
# Outcome: 10, …, 97 - age
#          98 - does not remember
#          99 - not specified
#          b  - blank
# Aim: create variable "eda_par" (age of marriage partner)
table(TB_SEC_IVaVD$P13_10, useNA = "ifany") # 19436 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(eda_par = as.numeric(as.character(P13_10)),
         eda_par = ifelse(eda_par >= 98, NA, eda_par))

# Summary stat:
table(TB_SEC_IVaVD$eda_par, useNA = "ifany") # 22484 NAs
head(TB_SEC_IVaVD[, c("P13_10", "eda_par")], n = 60)


## AGE OF PARTNER --------
# Remark: Age of partner is:
# Age partner cohabiting/ married + (age woman - age of marriage/cohabiting)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  left_join(TSDem %>% select(c("ID_VIV", "ID_PER","EDAD")), by = c("ID_VIV", "ID_PER"))
TB_SEC_IVaVD$eda_par2 <- TB_SEC_IVaVD$eda_par + (TB_SEC_IVaVD$EDAD - TB_SEC_IVaVD$eda_mat)
head(TB_SEC_IVaVD[, c("EDAD", "eda_mat", "eda_par", "eda_par2")], n = 60)


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
# Levels: 1 (no), 2 (yes)
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
table(TB_SEC_IVaVD$edu_par) # 73786 NAs

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(edu_parlow = factor(ifelse(edu_par == "low", "2", ifelse(edu_par == "NA", NA_character_, "1")), levels = c(1, 2), labels = c("no", "yes")),
         edu_parmedium = factor(ifelse(edu_par == "medium", "2", ifelse(edu_par == "NA", NA_character_, "1")), levels = c(1, 2), labels = c("no", "yes")),
         edu_parhigh = factor(ifelse(edu_par == "high", "2", ifelse(edu_par == "NA", NA_character_, "1")), levels = c(1, 2), labels = c("no", "yes")),
         edu_parNA = factor(ifelse(edu_par == "NA", "2", "1"), levels = c(1, 2), labels = c("no", "yes")))

# Summary stat:
head(TB_SEC_IVaVD[, c("P4BC_2", "edu_par", "edu_parlow", "edu_parmedium", "edu_parhigh")], n = 60)
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
# Create variable "ind_par"
# Levels: no (1), yes (2)
table(TB_SEC_IVaVD$P4BC_3, useNA = "ifany") # 73389 NAs
TB_SEC_IVaVD$ind_par <- ifelse(TB_SEC_IVaVD$P4BC_3 %in% c("1", "2"), "2",
                               ifelse(TB_SEC_IVaVD$P4BC_3 == "3", "1",
                                      ifelse(TB_SEC_IVaVD$P4BC_3 == "8", NA, NA)))
TB_SEC_IVaVD$ind_par <- factor(TB_SEC_IVaVD$ind_par, levels = c(1, 2), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$ind_par, useNA = "ifany") # 76549 NAs
head(TB_SEC_IVaVD[, c("P4BC_3", "ind_par")], n = 60)



# Finalize ------

## Keep relevant variables ------
relationship_demographic <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "eda_par2", "edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(relationship_demographic, file = paste0(path_rf,"relationship_demographic.RData"))









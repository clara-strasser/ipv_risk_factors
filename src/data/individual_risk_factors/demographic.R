####################### Individual - Demographic Risk Factors ##################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

## Load data -------
load(paste0(path,"TSDem.RData"))

# Risk Factors -----

## GENDER ------
# Variable name: SEXO
# Levels: 1 (male), 2 (female)
table(TSDem$SEXO, useNA = "ifany") # no NAs
TSDem$SEXO <- factor(TSDem$SEXO, levels = c(1, 2), labels = c("male", "female"))

# Summary Stat:
summary(TSDem$SEXO)

## AGE ----
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

## INDIGENOUS ------
# Variable name: P2_10
# Outcomes: 1 - si            - yes
#           2 - si, en parte  - yes
#           3 - no            - no
#           8 - no sabe       - NA
#           b - blank         - NA
# Aim: create variable "indigena"
# Levels: 1 (no), 2 (yes)
table(TSDem$P2_10, useNA = "ifany") # 16648 NAs
TSDem$indigena <- ifelse(TSDem$P2_10 %in% c("1", "2"), "yes",
                         ifelse(TSDem$P2_10 == "3", "no",
                                ifelse(TSDem$P2_10 == "8", NA, NA)))
TSDem$indigena <- factor(TSDem$indigena, levels = c("no", "yes", NA))

# Summary Stat:
table(TSDem$indigena, useNA = "ifany") # 23760 NAs
head(TSDem[, c("P2_10", "indigena")], n = 35)

## EDUCATION -----
# Variable name: NIV (P2_7)
# Outcomes: 00 - no education
#           01 to 11 - different education levels
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
# Levels:  1 (no), 2 (yes)
table(TSDem$NIV, useNA = "ifany") # 16648 NAs

# Create "niv_ed" variable
TSDem <- TSDem %>%
  mutate(NIV = as.numeric(NIV),
         niv_ed = ifelse(is.na(NIV), "NA",
                         ifelse(NIV <= 2, "low",
                                ifelse(NIV >= 3 & NIV <= 8, "medium",
                                       ifelse(NIV >= 9, "high", NA_character_)))))
# Summary Stat:
table(TSDem$niv_ed)
head(TSDem[, c("NIV", "niv_ed")], n = 60)

# Create variables "niv_edlow", "niv_edmedium", "niv_edhigh" and "niv_edNA"
# Levels: 1 (no), 2 (yes)
TSDem <- TSDem %>%
  mutate(niv_edlow = factor(ifelse(niv_ed == "low", "yes", ifelse(niv_ed == "NA", NA_character_, "no")), levels = c("no", "yes")),
         niv_edmedium = factor(ifelse(niv_ed == "medium", "yes", ifelse(niv_ed == "NA", NA_character_, "no")), levels = c("no", "yes")),
         niv_edhigh = factor(ifelse(niv_ed == "high", "yes", ifelse(niv_ed == "NA", NA_character_, "no")), levels = c("no", "yes")),
         niv_edNA = factor(ifelse(niv_ed == "NA", "yes", "no"), levels = c("no", "yes")))
head(TSDem[, c("NIV", "niv_ed", "niv_edlow", "niv_edmedium", "niv_edhigh", "niv_edNA")], n = 60)

# Finalize ------

## Keep relevant
individual_demographic <- TSDem %>%
  select(c("ID_VIV", "ID_PER", "SEXO", "EDAD", "indigena", "niv_edlow", "niv_edmedium", "niv_edhigh"))






########################## Individual Level Risk Factors #######################

# Prepare -----

## Load packages ------
library(dplyr)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"
setwd(path)

## Load data -------
load("TSDem.RData")

# Risk Factors ------

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
summary(TSDem$EDAD)

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
# Aim: create three levels "niv_edlow", "niv_edmedium" and "niv_edhigh"
table(TSDem$NIV, useNA = "ifany") # 16648 NAs








  


########################## TSDem Risk Factors #################################

# Initiate -----

## Load packages ------
library(dplyr)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------
load(paste0(path,"TSDem.RData"))

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
# Aim: create three variables "niv_edlow", "niv_edmedium" and "niv_edhigh"
table(TSDem$NIV, useNA = "ifany") # 16648 NAs

# Create "niv_ed" variable
TSDem <- TSDem %>%
  mutate(NIV = as.numeric(NIV),
         niv_ed = ifelse(is.na(NIV), "NA",
                         ifelse(NIV <= 2, "low",
                                ifelse(NIV >= 3 & NIV <= 8, "medium",
                                       ifelse(NIV >= 9, "high", NA)))))
# Summary Stat:
table(TSDem$niv_ed)

# Create variables "niv_edlow", "niv_edmedium", "niv_edhigh" and "niv_edNA"
# Levels: 1 (no), 2 (yes)
TSDem <- TSDem %>%
  mutate(niv_edlow = factor(ifelse(niv_ed == "low", "yes", "no"), levels = c("no", "yes")),
         niv_edmedium = factor(ifelse(niv_ed == "medium", "yes", "no"), levels = c("no", "yes")),
         niv_edhigh = factor(ifelse(niv_ed == "high", "yes", "no"), levels = c("no", "yes")),
         niv_edNA = factor(ifelse(niv_ed == "NA", "yes", "no"), levels = c("no", "yes")))

## INDIGENOUS ------
# Variable name: P2_10
# Outcomes: 1 - si            - yes
#           2 - si, en parte  - yes
#           3 - no            - no
#           8 - no sabe       - NA
#           b - blank         - NA
table(TSDem$P2_10, useNA = "ifany")

# Create variable "indigena"
# Levels: 1 (yes), 2 (no)
TSDem$indigena <- ifelse(TSDem$P2_10 %in% c("1", "2"), "yes",
                         ifelse(TSDem$P2_10 == "3", "no",
                                ifelse(TSDem$P2_10 == "8", NA, NA)))
TSDem$indigena <- factor(TSDem$indigena, levels = c("no", "yes", NA))
# Summary Stat:
table(TSDem$indigena, useNA = "ifany")

## PARTNER STATUS ------
# Variable name: P2_16
# Outcomes: 1 - cohabiting
#           2 - separated
#           3 - divorced
#           4 - widowed
#           5 - married
#           6 - single
#           b - blank
# Levels: cohabiting/married (1), other (2)
TSDem$partner_status <- ifelse(TSDem$P2_16 == "1" | TSDem$P2_16 == "5", 1, 2)
TSDem$partner_status <- factor(TSDem$partner_status, levels = c(1, 2), labels = c("cohabiting/married", "other"))
# Summary Stat:
table(TSDem$partner_status, useNA = "ifany")

# Finalize -----

## Keep relevant data ----

# Keep relevant rows:
# partner_status = "cohabiting/married"
TSDem_final <- filter(TSDem, partner_status == "cohabiting/married")

# chosen women in survey older than 15
# Variable name: CODIGO
# Levels: 0 (women older than 15 not chosen), 1 (women older than 1 chosen)
table(TSDem$CODIGO, useNA = "ifany")
table(TSDem_final$CODIGO, useNA = "ifany")
TSDem_final <- filter(TSDem_final, CODIGO == "1")

# Keep relevant columns
TSDem_final <- TSDem_final %>%
  select(c("ID_VIV", "ID_PER", "UPM", "VIV_SEL", "CVE_ENT",
                          "NOM_ENT", "CVE_MUN", "NOM_MUN", "HOGAR", "N_REN", 
                          "SEXO", "EDAD", "NIV", "COD_M15", "CODIGO", 
                          "REN_MUJ_EL", "REN_INF_AD", 
                          "FAC_VIV", "FAC_MUJ", "DOMINIO", "ESTRATO", 
                          "EST_DIS", "UPM_DIS", "niv_ed", "niv_edlow", 
                          "niv_edmedium", "niv_edhigh", "niv_edNA", 
                          "indigena", "partner_status"))


## Save data -----
save_path <- "/Users/clara/Desktop/Masterarbeit/r_projects/ipv_risk_factors/data/"
save(TSDem_final, file = paste0(save_path,"TSDem_final.RData"))



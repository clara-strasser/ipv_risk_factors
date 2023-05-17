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


## AGE OF PARTNER --------
# Remark: Age of partner is:
# Age partner cohabiting/ married + (age woman - age of marriage/cohabiting)
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  left_join(TSDem %>% select(c("ID_VIV", "ID_PER","EDAD")), by = c("ID_VIV", "ID_PER"))
TB_SEC_IVaVD$eda_par2 <- TB_SEC_IVaVD$eda_par + (TB_SEC_IVaVD$EDAD - TB_SEC_IVaVD$eda_mat)

# Finalize ------

## Keep relevant variables ------
relationship_demographic <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "eda_par2"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(relationship_demographic, file = paste0(path_rf,"relationship_demographic.RData"))









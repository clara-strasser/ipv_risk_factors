############################# Add Risk Factors ##########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"

## Load data -------
load(paste0(path,"TB_SEC_IVaVD.RData"))
load(paste0(path_data, "step2_endireh.RData"))

# Risk Factors -----

## NUMBER OF CHILDREN OF PARTNER --------
# Variable name: P13_3
# Outcomes: 00 - no children
#           01 - 25 - nr. of children
#           99 - not specified
#           b  - blank
# Aim: create variable "num_hij" (number of children)
table(TB_SEC_IVaVD$P13_3, useNA = "ifany") # 22169 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(num_hij_par = as.numeric(as.character(P13_3)),
         num_hij_par = ifelse(num_hij_par == 99, NA, num_hij_par))

# Summary stat:
table(TB_SEC_IVaVD$num_hij_par, useNA = "ifany") # 22632 NAs
head(TB_SEC_IVaVD[, c("P13_3", "num_hij_par")], n = 100)


## NUMBER OF CHILDREN OF PARTNER WITH OTHER WOMAN --------
# Variable name: P13_4
# Outcomes: 00 - no children
#           01 - 25 - nr. of children
#           99 - not specified
#           b  - blank
# Aim: create variable "num_hij" (number of children)
table(TB_SEC_IVaVD$P13_4, useNA = "ifany") # 4851 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(num_hij_par_muj = as.numeric(as.character(P13_4)),
         num_hij_par_muj = ifelse(num_hij_par_muj == 99, NA, num_hij_par_muj),
         num_hij_par_muj = ifelse(num_hij_par_muj == 98, NA, num_hij_par_muj))

# Summary stat:
table(TB_SEC_IVaVD$num_hij_par_muj, useNA = "ifany") # 11460 NAs
head(TB_SEC_IVaVD[, c("P13_4", "num_hij_par_muj")], n = 100)


# Finalize ------

## Subset data ------
extended_risk_factors <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM",
           "num_hij_par","num_hij_par_muj"))

## Join data ---------
step2_endireh <- step2_endireh %>% 
  left_join(extended_risk_factors, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


## Save data -----

save(step2_endireh, file = paste0(path_data ,"step2_endireh.RData"))













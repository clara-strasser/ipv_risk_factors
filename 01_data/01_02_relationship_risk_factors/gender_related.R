################# Relationship - Gender Related Risk Factors ##################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/ipv_data/raw_data"
path_data_endireh <- file.path(base_path, "main_source/")
path_data_final <- file.path(path_data_endireh, "risk_factors/")

## Load data -------------------------------------------------------------------
load(paste0(path_data_endireh,"TB_SEC_IVaVD.RData"))

# Risk Factors -----------------------------------------------------------------

## GENDER OF PARTNER ----------
# Variable name: P13_5
# Outcomes:   1 - man
#             2 - woman
#             9 - not specified
#             b - blank
# Label: male (1), female (2)
# Aim: create variable "par_sex"
table(TB_SEC_IVaVD$P13_5, useNA = "ifany") # 4849 NAs
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(par_sex = factor(ifelse(P13_5 == 1, "1", ifelse(P13_5 == 2, "2", NA)), 
                          levels = c(1, 2), labels = c("male", "female")))
table(TB_SEC_IVaVD$par_sex, useNA = "ifany") # 4911 NAs
# Summary stat:
#   male    female    NA 
#  104656    560     4911 
head(TB_SEC_IVaVD[, c("P13_5", "par_sex")], n = 60)


## VIOLENCE EXPERIENCE CHILDHOOD PARTNER -------
# Variable name: P12_8
# Remark: types of violence are physical (beating) and
# emotional (insulting and offending)
# Outcomes:     1 - sometimes
#               2 - more often
#               3 - no 
#               8 - does not know
#               b - blank
# Labels: 1 (no), 2 (yes)
# Aim: create variable "vio_exp_inf_par"
table(TB_SEC_IVaVD$P12_8, useNA = "ifany") # 4849 NAs
TB_SEC_IVaVD$P12_8[TB_SEC_IVaVD$P12_8 == "8"] <- NA
TB_SEC_IVaVD$vio_exp_inf_par <- factor(ifelse(TB_SEC_IVaVD$P12_8 == "1" | TB_SEC_IVaVD$P12_8 == "2", "1", "2"), levels = c("2", "1"), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$vio_exp_inf_par, useNA = "ifany") # 32863 NAs
#    no   yes    NA 
#  46563 30701 32863  


## VIOLENCE WITNESS CHILDHOOD PARTNER -------
# Variable name: P12_9
# Remark: types of violence are physical IPV (father beats mother)
# Outcomes:     1 - yes
#               2 - no
#               8 - does not know
#               b - blank
# Labels: 1 (no), 2 (yes)
# Aim: create variable "vio_inf_par"
table(TB_SEC_IVaVD$P12_9, useNA = "ifany") # 4849 NAs
TB_SEC_IVaVD$P12_9[TB_SEC_IVaVD$P12_9 == "8"] <- NA
TB_SEC_IVaVD$vio_inf_par <- factor(ifelse(TB_SEC_IVaVD$P12_9 == "1", "1", "2"), levels = c("2", "1"), labels = c("no", "yes"))

# Summary Stat:
table(TB_SEC_IVaVD$vio_inf_par, useNA = "ifany") # 38657 NAs
#   no   yes    NA 
#  50471 20999 38657  


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
# Aim: create variables vio_fis_ex, vio_emo_ex, vio_eco_ex, vio_sex_ex
table(TB_SEC_IVaVD$P13_17_1, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_17_2, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_17_3, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_17_5, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$P13_17_6, useNA = "ifany") # 85137 NAs


# Set columns
vio_fis <- paste0("P13_17_", 1:2) 
vio_emo <- paste0("P13_17_", 3) 
vio_sex <- paste0("P13_17_", 5) 
vio_eco <- paste0("P13_17_", 6)

# Create columns
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(vio_fis_ex = ifelse(rowSums(select(., any_of(vio_fis)) == "1", na.rm = TRUE) > 0, "2", 
                             ifelse(rowSums(select(., any_of(vio_fis)) == "2", na.rm = TRUE) > 0, "1", NA_character_)),
         vio_emo_ex = ifelse(rowSums(select(., any_of(vio_emo)) == "1", na.rm = TRUE) > 0, "2", 
                             ifelse(rowSums(select(., any_of(vio_emo)) == "2", na.rm = TRUE) > 0, "1", NA_character_)),
         vio_sex_ex = ifelse(rowSums(select(., any_of(vio_sex)) == "1", na.rm = TRUE) > 0, "2", 
                             ifelse(rowSums(select(., any_of(vio_sex)) == "2", na.rm = TRUE) > 0, "1", NA_character_)),
         vio_eco_ex = ifelse(rowSums(select(., any_of(vio_eco)) == "1", na.rm = TRUE) > 0, "2", 
                             ifelse(rowSums(select(., any_of(vio_eco)) == "2", na.rm = TRUE) > 0, "1", NA_character_))) %>%
  mutate(vio_fis_ex = factor(vio_fis_ex, levels = c(1, 2), labels = c("no", "yes")),
         vio_emo_ex = factor(vio_emo_ex, levels = c(1, 2), labels = c("no", "yes")),
         vio_sex_ex = factor(vio_sex_ex, levels = c(1, 2), labels = c("no", "yes")),
         vio_eco_ex = factor(vio_eco_ex, levels = c(1, 2), labels = c("no", "yes")))

# Summary stat:
table(TB_SEC_IVaVD$vio_fis_ex, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$vio_emo_ex, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$vio_sex_ex, useNA = "ifany") # 85137 NAs
table(TB_SEC_IVaVD$vio_eco_ex, useNA = "ifany") # 85137 NAs
head(TB_SEC_IVaVD[, c("P13_17_1", "P13_17_2", "vio_fis_ex", "P13_17_3", "vio_emo_ex")], n = 65)


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
#                7 - does not apply (NA)
#                b - blank (NA)
# Label: high, medium, low
# Level: 1 (no), 2 (yes)
# Aim: create variables "lib_sex_gradlow", "lib_sex_gradmedium" and "lib_sex_gradhigh"
table(TB_SEC_IVaVD$P15_1AB_12, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_13, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_14, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_15, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_16, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_17, useNA = "ifany") # 19436 NAs

# Set columns
lib_sex <- paste0("P15_1AB_", 12:17)

# Re-code the values and convert to factors
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
  mutate(lib_sex_gradhigh = ifelse(lib_sex_grad == "high", "2", "1"),
         lib_sex_gradmedium = ifelse(lib_sex_grad == "medium", "2", "1"),
         lib_sex_gradlow = ifelse(lib_sex_grad == "low", "2", "1"),
         lib_sex_gradNA = ifelse(is.na(lib_sex_grad), "2", "1")) %>%
  mutate(lib_sex_gradhigh = factor(lib_sex_gradhigh, levels = c(1, 2), labels = c("no", "yes")),
         lib_sex_gradmedium = factor(lib_sex_gradmedium, levels = c(1, 2), labels = c("no", "yes")),
         lib_sex_gradlow = factor(lib_sex_gradlow, levels = c(1, 2), labels = c("no", "yes")),
         lib_sex_gradNA = factor(lib_sex_gradNA, levels = c(1, 2), labels = c("no", "yes")))  

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
# Level: 1 (no), 2 (yes)
# Aim: create variables "lib_eco_gradlow", "lib_eco_gradmedium" and "lib_eco_gradhigh"
table(TB_SEC_IVaVD$P15_1AB_1, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_3, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_4, useNA = "ifany") # 19436 NAs

# Set columns
lib_eco <- paste0("P15_1AB_", c(1, 3:4))  

# Re-code the values and convert to factors
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
  mutate(lib_eco_gradhigh = ifelse(lib_eco_grad == "high", "2", "1"),
         lib_eco_gradmedium = ifelse(lib_eco_grad == "medium", "2", "1"),
         lib_eco_gradlow = ifelse(lib_eco_grad == "low", "2", "1"),
         lib_eco_gradNA = ifelse(is.na(lib_eco_grad), "2", "1")) %>%
  mutate(lib_eco_gradhigh = factor(lib_eco_gradhigh, levels = c(1, 2), labels = c("no", "yes")),
         lib_eco_gradmedium = factor(lib_eco_gradmedium, levels = c(1, 2), labels = c("no", "yes")),
         lib_eco_gradlow = factor(lib_eco_gradlow, levels = c(1, 2), labels = c("no", "yes")),
         lib_eco_gradNA = factor(lib_eco_gradNA, levels = c(1, 2), labels = c("no", "yes")))  

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
# Level: 1 (no), 2 (yes)
# Aim: create variables "lib_soc_gradlow", "lib_soc_gradmedium" and "lib_soc_gradhigh"
# Remarks*: new question added (P15_1AB_5 and P15_1AB_6 were before one question P14_1AB_5) + question P15_1AB_9 (before P14_1AB_8) was not included
table(TB_SEC_IVaVD$P15_1AB_2, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_5, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_6, useNA = "ifany") # 19436 NAs
table(TB_SEC_IVaVD$P15_1AB_9, useNA = "ifany") # 19436 NAs

# Set columns
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
  mutate(lib_soc_gradhigh = ifelse(lib_soc_grad == "high", "2", "1"),
         lib_soc_gradmedium = ifelse(lib_soc_grad == "medium", "2", "1"),
         lib_soc_gradlow = ifelse(lib_soc_grad == "low", "2", "1"),
         lib_soc_gradNA = ifelse(is.na(lib_soc_grad), "2", "1")) %>%
  mutate(lib_soc_gradhigh = factor(lib_soc_gradhigh, levels = c(1, 2), labels = c("no", "yes")),
         lib_soc_gradmedium = factor(lib_soc_gradmedium, levels = c(1, 2), labels = c("no", "yes")),
         lib_soc_gradlow = factor(lib_soc_gradlow, levels = c(1, 2), labels = c("no", "yes")),
         lib_soc_gradNA = factor(lib_soc_gradNA, levels = c(1, 2), labels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("P15_1AB_2", "P15_1AB_5", "P15_1AB_6", "P15_1AB_9", "lib_soc_grad", "lib_soc_gradhigh", "lib_soc_gradmedium", "lib_soc_gradlow","lib_soc_gradNA" )], n = 35)


# Finalize ------

## Keep relevant variables ------
relationship_gender_related <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM",
           "vio_exp_inf_par", "vio_inf_par", "par_sex", 
           "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex",
           "lib_sex_gradhigh", "lib_sex_gradmedium", "lib_sex_gradlow",
           "lib_eco_gradhigh", "lib_eco_gradmedium", "lib_eco_gradlow",
           "lib_soc_gradhigh", "lib_soc_gradmedium", "lib_soc_gradlow"))

## Save data -----

save(relationship_gender_related, file = paste0(path_data_final,"relationship_gender_related.RData"))






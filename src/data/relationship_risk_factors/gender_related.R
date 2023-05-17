################# Relationship - Gender Related Risk Factors ##################

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


# Finalize ------

## Keep relevant variables ------
relationship_gender_related <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM",
           "lib_sex_gradhigh", "lib_sex_gradmedium", "lib_sex_gradlow",
           "lib_eco_gradhigh", "lib_eco_gradmedium", "lib_eco_gradlow",
           "lib_soc_gradhigh", "lib_soc_gradmedium", "lib_soc_gradlow"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(relationship_gender_related, file = paste0(path_rf,"relationship_demographic.RData"))






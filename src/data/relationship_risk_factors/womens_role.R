################# Relationship - Women's Role Risk Factors ##################

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


## DIVISION OF HOUSEWORK --------
# Variable name: P17_1_1_1 - P17_1_2_1 (the same for: _2 and _3 at the end)
# Questions:
# cuidar o apoyar a las niñas y niños que viven aquí?
# cuidar o apoyar a las ancianas y ancianos que viven aquí?
# hacer los quehaceres domésticos (cocinar, lavar, planchar, asear la casa)?
# hacer los trámites y compras para el hogar (pagos de luz, teléfono, ir al banco, al mercado, etcétera)?
# atender o apoyar a las personas con alguna discapacidad?
# hacer reparaciones a su vivienda, muebles, vehículos o aparatos electrodomésticos?
# atender a personas enfermas? 
# Outcome:  1 - women (F)
#           2 - husband or partner (M)
#           3 - both (both)
#           4 - daughters (F)
#           5 - sons (M)
#           6 - mother(F)
#           7 - father (M)
#           8 - both parents (both)
#           9 - sisters (F)
#          10 - brothers (M)
#          11 - someone external (NA)
#          12 - someone from the household (NA)
#          13 - someone outside the household (NA)
#          14 - no one (NA)
#          15 - other (NA)
#          99 - not specified (NA)
# Remarks: for each question there are 3 response possibilities
# Level: 1 (no), 2 (yes)
# Aim: create variables "act_distfemales", "act_distmales", "act_distboth"
table(TB_SEC_IVaVD$P17_1_1_1, useNA = "ifany") # No NAs
table(TB_SEC_IVaVD$P17_1_1_2, useNA = "ifany") # NO NAs

# Set column names
tareas <- paste0(rep(paste0("P17_1_", 1:7, "_"), each = 3), 1:3)

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(tareas)), ~recode(.,
                                          "01" = "females",
                                          "02" = "males",
                                          "03" = "both",
                                          "04" = "females",
                                          "05" = "males",
                                          "06" = "females",
                                          "07" = "males",
                                          "08" = "both",
                                          "09" = "females",
                                          "10" = "males",
                                          "11" = NA_character_,
                                          "12" = NA_character_,
                                          "13" = NA_character_,
                                          "14" = NA_character_,
                                          "15" = NA_character_,
                                          "99" = NA_character_)) %>%
  mutate_at(vars(any_of(tareas)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    females = rowSums(select(., all_of(tareas)) == "females", na.rm = TRUE),
    males = rowSums(select(., all_of(tareas)) == "males", na.rm = TRUE),
    both = rowSums(select(., all_of(tareas)) == "both", na.rm = TRUE)
  )
head(TB_SEC_IVaVD[, c("P17_1_1_1", "P17_1_1_2", "P17_1_1_3", "P17_1_2_1", "P17_1_2_2", "P17_1_2_3", "P17_1_3_1", "P17_1_3_2", "P17_1_3_3", "P17_1_4_1",
                      "P17_1_4_2", "P17_1_4_3", "P17_1_5_1", "P17_1_5_2", "P17_1_5_3", "P17_1_6_1", "P17_1_6_2", "P17_1_6_3", "P17_1_7_1", "P17_1_7_2",
                      "P17_1_7_3", "females", "males", "both")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(act_dist = ifelse(females > males, "females",
                           ifelse(males > females, "males",
                                  "both")),
         act_dist = ifelse(females == 0 & males == 0 & both == 0, NA_character_, act_dist))
head(TB_SEC_IVaVD[, c("P17_1_1_1", "P17_1_1_2", "P17_1_1_3", "P17_1_2_1", "P17_1_2_2", "P17_1_2_3", "P17_1_3_1", "P17_1_3_2", "P17_1_3_3", "P17_1_4_1",
                      "P17_1_4_2", "P17_1_4_3", "P17_1_5_1", "P17_1_5_2", "P17_1_5_3", "P17_1_6_1", "P17_1_6_2", "P17_1_6_3", "P17_1_7_1", "P17_1_7_2",
                      "P17_1_7_3", "females", "males", "both", "act_dist")], n = 35)
# Remarks: same threshold as authors

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(act_distfemales = ifelse(act_dist == "females", "2", "1"),
         act_distmales = ifelse(act_dist == "males", "2", "1"),
         act_distboth = ifelse(act_dist == "both", "2", "1"),
         act_distNA = ifelse(is.na(act_dist), "2", "1")) %>%
  mutate(act_distfemales = factor(act_distfemales, levels = c(1, 2), labels = c("no", "yes")),
         act_distmales = factor(act_distmales, levels = c(1, 2), labels = c("no", "yes")),
         act_distboth = factor(act_distboth, levels = c(1, 2), labels = c("no", "yes")),
         act_distNA = factor(act_distNA, levels = c(1, 2), labels = c("no", "yes")))  

head(TB_SEC_IVaVD[, c("females", "males", "both", "act_dist", "act_distfemales", "act_distmales", "act_distboth", "act_distNA")], n = 35)


# Finalize ------

## Keep relevant variables ------
relationship_womens_role <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM",
           "act_distfemales", "act_distmales", "act_distboth"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(relationship_womens_role, file = paste0(path_rf,"relationship_womens_role.RData"))










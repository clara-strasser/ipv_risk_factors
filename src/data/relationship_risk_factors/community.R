################# Relationship - Community Risk Factors ##################

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


## WOMENS PERCEPTION ABOUT SUPPORT FROM SOCIAL NETWORKS --------
# Variable name: P16_3_1_1, P16_3_2_1, P16_3_3_1, P16_3_4_1, P16_3_5_1, P16_3_6_1
# Questions:
# cuidar un rato a sus hijas/hijos cuando tiene alguna emergencia o se enferman?
# hacer alguna tarea o labor?
# cuando usted se enferma?
# para platicar de sus problemas o preocupaciones?
# consejos u orientación cuando tiene dificultades con su esposo o pareja?
# cuando tiene alguna dificultad o problema económico?
# Outcome:       1 - neighbor
#                2 - friend
#                3 - college
#                4 - family
#                5 - someone else
#                6 - no one    
# Remarks: for each question there are three response possibilities, the respondant can answer with more than one person
# However: not relevant the list of people
# Level: 1 (no), 2 (yes)
# Aim: create variables "redsoc_gradhigh", "redsoc_gradmedium", "redsoc_gradlow"
table(TB_SEC_IVaVD$P16_3_1_1, useNA = "ifany") #  13543 no one

# Set columns
redsoc <- paste0("P16_3_", 1:6, "_1")

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(redsoc)), ~recode(.,
                                          "1" = "yes",
                                          "2" = "yes",
                                          "3" = "yes",
                                          "4" = "yes",
                                          "5" = "yes",
                                          "6" = "no")) %>%
  mutate_at(vars(any_of(redsoc)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    red_soc_yes = rowSums(select(., all_of(redsoc)) == "yes", na.rm = TRUE),
    red_soc_no = rowSums(select(., all_of(redsoc)) == "no", na.rm = TRUE))
head(TB_SEC_IVaVD[, c("P16_3_1_1", "P16_3_2_1", "P16_3_3_1", "P16_3_4_1", "P16_3_5_1", "P16_3_6_1", "red_soc_yes", "red_soc_no")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(redsoc_grad = ifelse(red_soc_yes >= 5, "high",
                              ifelse(red_soc_yes <= 1, "low",
                                     "medium")))
head(TB_SEC_IVaVD[, c("P16_3_1_1", "P16_3_2_1", "P16_3_3_1", "P16_3_4_1", "P16_3_5_1", "P16_3_6_1", "red_soc_yes", "red_soc_no", "redsoc_grad" )], n = 35)
# Remark: same thresholds as authors, same number of columns

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(redsoc_gradhigh = ifelse(redsoc_grad == "high", "yes", "no"),
         redsoc_gradmedium = ifelse(redsoc_grad == "medium", "yes", "no"),
         redsoc_gradlow = ifelse(redsoc_grad == "low", "yes", "no")) %>%
  mutate(redsoc_gradhigh = factor(redsoc_gradhigh, levels = c("no", "yes")),
         redsoc_gradmedium = factor(redsoc_gradmedium, levels = c("no", "yes")),
         redsoc_gradlow = factor(redsoc_gradlow, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("red_soc_yes", "red_soc_no", "redsoc_grad", "redsoc_gradhigh", "redsoc_gradmedium", "redsoc_gradlow")], n = 30)


## LEVEL OF SOCIAL INTERACTION WOMEN --------
# Variable name: P16_2_1 - P16_2_6
# Questions:
# salir con amigas a divertirse?
# platicar con vecinas?
# reunirse con familiares?
# asistir a reuniones religiosas?
# asistir a reuniones de colonos o de organizaciones?
# practicar deportes en equipo?
# Outcome:     1 - yes
#              2 - no
# Level: 1 (no), 2 (yes)
# Aim: create varibles "rout_gradhigh", "rout_gradmedium", "rout_gradlow"
table(TB_SEC_IVaVD$P16_2_1, useNA = "ifany")

# Set columns
rout <- paste0("P16_2_", 1:6)

# Recode the values and convert to factors
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate_at(vars(any_of(rout)), ~recode(.,
                                        "1" = "yes",
                                        "2" = "no")) %>%
  mutate_at(vars(any_of(rout)), factor)

# Sum
TB_SEC_IVaVD <- TB_SEC_IVaVD %>%
  mutate(
    rout_yes = rowSums(select(., all_of(rout)) == "yes", na.rm = TRUE),
    rout_no = rowSums(select(., all_of(rout)) == "no", na.rm = TRUE))
head(TB_SEC_IVaVD[, c("P16_2_1", "P16_2_2", "P16_2_3", "P16_2_4", "P16_2_5", "P16_2_6", "rout_yes", "rout_no")], n = 35)

# Create variables
TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(rout_grad = ifelse(rout_yes >= 5, "high",
                            ifelse(rout_yes <= 1, "low",
                                   "medium")))
head(TB_SEC_IVaVD[, c("P16_2_1", "P16_2_2", "P16_2_3", "P16_2_4", "P16_2_5", "P16_2_6", "rout_yes", "rout_no", "rout_grad")], n = 35)
# Remark: same thresholds as authors, same number of columns

TB_SEC_IVaVD <- TB_SEC_IVaVD %>% 
  mutate(rout_gradhigh = ifelse(rout_grad == "high", "yes", "no"),
         rout_gradmedium = ifelse(rout_grad == "medium", "yes", "no"),
         rout_gradlow = ifelse(rout_grad == "low", "yes", "no")) %>%
  mutate(rout_gradhigh = factor(rout_gradhigh, levels = c("no", "yes")),
         rout_gradmedium = factor(rout_gradmedium, levels = c("no", "yes")),
         rout_gradlow = factor(rout_gradlow, levels = c("no", "yes")))  

# Summary stat:
head(TB_SEC_IVaVD[, c("rout_yes", "rout_no", "rout_grad", "rout_gradhigh", "rout_gradmedium", "rout_gradlow")], n = 35)


# Finalize ------

## Keep relevant variables ------
relationship_community <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM",
           "redsoc_gradhigh", "redsoc_gradmedium", "redsoc_gradlow",
           "rout_gradhigh", "rout_gradmedium", "rout_gradlow"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(relationship_community, file = paste0(path_rf,"relationship_community.RData"))









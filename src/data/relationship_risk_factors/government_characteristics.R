######################## Societal - Government Risk Factors ####################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

# Risk Factors -----

## SHARE OF POPULATION THAT CONSIDERED CORRUPTION A PROBLEM --------
# Variable name: cor19
# Outcome: share, 0-1
# Level: state
encig <- read_excel(paste0(path,"encig_2019.xlsx"), sheet = 3, range = cell_rows(10:44), col_names = TRUE)
encig <- encig %>%
  slice(-1) %>%
  select(1,5,8) %>%
  slice(-1) %>%
  rename(entidad = 1, rel1 = 2, rel2= 3) %>%
  mutate(cor19 = (as.numeric(rel1) + as.numeric(rel2))/100) %>%
  mutate(CVE_ENT = sprintf("%02d", 1:32)) %>%
  select(-c("entidad", "rel1", "rel2"))


## SHARE OF POPULATION SATISFIED WITH BASIC PUBLIC SERVICES ----------
# Variable name: satis19
# Outcome: share, 0-1
# Level: state
encig_ev <- read_excel(paste0(path,"encig_evaluacion_2019.xlsx"), sheet = 15, range = cell_rows(11:43), col_names = FALSE)
encig_ev <- encig_ev %>%
  select(1,5) %>%
  slice(-1) %>%
  rename(entidad = 1, rel1 = 2) %>%
  mutate(satis19 = as.numeric(rel1)/100) %>%
  mutate(CVE_ENT = sprintf("%02d", 1:32)) %>%
  select(-c("entidad", "rel1"))


# Finalize ------

## Join data ------
societal_government <- encig %>%
  left_join(encig_ev, by = c("CVE_ENT"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(societal_government, file = paste0(path_rf,"societal_government.RData"))

























####################### Community - Demographic Risk Factors ##################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

# Risk Factors -----


## TYPE OF COMMUNITY -----
# Variable name: Type_comrural, Type_comlow_urban, Type_commedium_urban, Type_comhigh_urban
# Outcome: no (1), yes (2)
# Level: municipality
# Remarks: variable "POP_TOT" (Total population in the municipality is needed) for 2020
conapo_1 <- read_excel(paste0(path,"conapo_2020.xls"), sheet = 2,col_names = TRUE)
conapo_1 <- conapo_1 %>%
  select(c("CVE_ENT", "CVE_MUN", "POB_TOT")) %>%
  mutate(CVE_MUN = substr(CVE_MUN, 3, nchar(CVE_MUN))) %>%
  mutate(Type_com = case_when(
    POB_TOT < 2500 ~ "rural",
    POB_TOT >= 2500 & POB_TOT < 15000 ~ "low_urban",
    POB_TOT >= 15000 & POB_TOT < 100000 ~ "medium_urban",
    POB_TOT >= 100000 ~ "high_urban"
  )) %>%
  mutate(Type_com = factor(Type_com)) %>%
  mutate(Type_comrural = ifelse(Type_com == "rural", "2", "1"),
         Type_comlow_urban = ifelse(Type_com == "low_urban", "2", "1"),
         Type_commedium_urban = ifelse(Type_com == "medium_urban", "2", "1"),
         Type_comhigh_urban = ifelse(Type_com == "high_urban", "2", "1")) %>%
  mutate(Type_comrural = factor(Type_comrural, levels = c(1, 2), labels = c("no", "yes")),
         Type_comlow_urban = factor(Type_comlow_urban, levels = c(1, 2), labels = c("no", "yes")),
         Type_commedium_urban = factor(Type_commedium_urban, levels = c(1, 2), labels = c("no", "yes")),
         Type_comhigh_urban = factor(Type_comhigh_urban, levels = c(1, 2), labels = c("no", "yes")))  %>%
  select(-c("Type_com"))

## SOCIAL MARGINALIZATION --------
# Variable name: Marg15high, Marg15low, Marg15medium, Marg15very_high, Marg15very_low
# Outcome: no (1), yes (2)
# Level: municipality
conapo_2 <- read_excel(paste0(path,"conapo_2020.xls"), sheet = 2,col_names = TRUE)
conapo_2 <- conapo_2 %>%
  select(c("CVE_ENT", "CVE_MUN", "GM_2020")) %>%
  mutate(CVE_MUN = substr(CVE_MUN, 3, nchar(CVE_MUN))) %>%
  mutate(Marg15high = ifelse(GM_2020 == "Alto", "2", "1"),
         Marg15low = ifelse(GM_2020 == "Bajo", "2", "1"),
         Marg15medium = ifelse(GM_2020 == "Medio", "2", "1"),
         Marg15very_high = ifelse(GM_2020 == "Muy alto", "2", "1"),
         Marg15very_low = ifelse(GM_2020 == "Muy bajo", "2", "1")) %>%
  mutate(Marg15high = factor(Marg15high, levels = c(1, 2), labels = c("no", "yes")),
         Marg15low = factor(Marg15low, levels = c(1, 2), labels = c("no", "yes")),
         Marg15medium = factor(Marg15medium, levels = c(1, 2), labels = c("no", "yes")),
         Marg15very_high = factor(Marg15very_high, levels = c(1, 2), labels = c("no", "yes")),
         Marg15very_low = factor(Marg15very_low, levels = c(1, 2), labels = c("no", "yes")))  %>%
  select(-c("GM_2020"))


## FEMALE AND MALE SHARE OF MIGRANT POPULATION --------
# Variable name: pres_2020_f and pres_2020_m
# Outcome: %
# Level: municipality
migracion_inegi <- read_excel(paste0(path,"migracion_inegi_2020.xlsx"), sheet = 1, col_names = TRUE)
migracion_inegi <- migracion_inegi[migracion_inegi$indicador %in% c("Porcentaje de población masculina nacida en otro país residente en México",
                                                                    "Porcentaje de población femenina nacida en otro país residente en México"),]
migracion_inegi <- migracion_inegi[migracion_inegi$cve_municipio != "0", ]
migracion_inegi <- migracion_inegi %>%
  mutate(pres_2020_f = ifelse(indicador == "Porcentaje de población femenina nacida en otro país residente en México", `2020`, NA),
         pres_2020_m = ifelse(indicador == "Porcentaje de población masculina nacida en otro país residente en México", `2020`, NA)) %>%
  select(-c("indicador", "2020")) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(pres_2020_f = max(pres_2020_f, na.rm = TRUE), pres_2020_m = max(pres_2020_m, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(CVE_ENT = cve_entidad,
         CVE_MUN = cve_municipio)


# Finalize ------

## Join data ------
community_demographic_part1 <- conapo_1 %>%
  left_join(conapo_2, by = c("CVE_ENT", "CVE_MUN"))

community_demographic_part2 <- community_demographic_part1 %>%
  left_join(migracion_inegi, by = c("CVE_ENT", "CVE_MUN"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(community_demographic_part2, file = paste0(path_rf,"community_demographic.RData"))





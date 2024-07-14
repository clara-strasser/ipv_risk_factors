##################### Community - Womens Role Risk Factors ##################

## Load packages ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/raw_data/other_sources/"

# Risk Factors -----------------------------------------------------------------


## WOMENS/MENS ECONOMICALLY ACTIVE POPULATION -------
# Variable name: pea_f and pea_m
# Outcome: %
# Level: municipality
# Aim: create variable "pea_f" and "pea_m" (percentage of economically active female/male population aged 12 and over)
intercensal <- read_excel(paste0(path,"intercensal_survey_2020.xlsx"), sheet = 1, skip = 4, col_names = TRUE)
intercensal <- intercensal[intercensal$indicador %in% c("Porcentaje de la población femenina de 12 años y más económicamente activa",
                                                        "Porcentaje de la población masculina de 12 años y más económicamente activa"),]
intercensal <- intercensal[intercensal$cve_municipio != "0", ]
intercensal <- intercensal %>%
  mutate(pea_f = ifelse(indicador == "Porcentaje de la población femenina de 12 años y más económicamente activa", `2020`, NA),
         pea_m = ifelse(indicador == "Porcentaje de la población masculina de 12 años y más económicamente activa", `2020`, NA)) %>%
  select(-c("indicador", "2020", "id_indicador", "desc_entidad", "desc_municipio", "unidad_medida")) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(pea_f = max(pea_f, na.rm = TRUE), pea_m = max(pea_m, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(CVE_ENT = "cve_entidad",
         CVE_MUN = "cve_municipio")


## WOMEN HOUSEHOLD HEADSHIP ------
# Variable name: phogjef_f
# Outcome: %
# Level: municipality
# Aim: create variable "phogjef_f" (female-headed households)
intercensal_2 <- read_excel(paste0(path,"intercensal_survey_2020.xlsx"), sheet = 1, skip = 4, col_names = TRUE)
intercensal_2 <- intercensal_2[intercensal_2$indicador %in% c("Hogares censales",
                                                        "Hogares con jefatura femenina"),]
intercensal_2 <- intercensal_2[intercensal_2$cve_municipio != "0", ]
intercensal_2 <- intercensal_2 %>%
  select(-c("id_indicador", "desc_entidad", "desc_municipio","unidad_medida")) %>%
  mutate(hogar_total = ifelse(indicador == "Hogares censales", `2020`, NA_character_),
         hogar_fem = ifelse(indicador == "Hogares con jefatura femenina", `2020`, NA_character_)) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(hogar_total = max(hogar_total, na.rm = TRUE), hogar_fem = max(hogar_fem, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phogjef_f = as.numeric(hogar_fem)/as.numeric(hogar_total)) %>%
  rename(CVE_ENT = "cve_entidad",
         CVE_MUN = "cve_municipio") %>%
  filter(CVE_MUN != "998") %>%
  select(-c("hogar_total", "hogar_fem"))


## WOMENS POLITICAL PARTICIPATION ------
# Variable name: ParPolF
# Outcome: share
# Level: municipality
# Aim: create variable "ParPolF" (womens political participation)
cngmd <- read_excel(paste0(path,"cngmd_2020.xlsx"), sheet = 3, skip = 3, col_names = TRUE)
cngmd <- cngmd[1:2501, ]
cngmd <- cngmd %>%
  select(c("Clave\r\nEntidad", "Clave\r\nMunicipio o demarcación", "Total", "Mujeres")) %>%
  rename(CVE_MUN = "Clave\r\nMunicipio o demarcación",
         CVE_ENT= "Clave\r\nEntidad") %>%
  mutate(ParPolF = as.numeric(Mujeres)/as.numeric(Total)) %>%
  select(-c("Mujeres", "Total")) %>% 
  filter(CVE_MUN != "000")


# Finalize ------

## Join data ------
community_women_part1 <- intercensal %>%
  left_join(intercensal_2, by = c("CVE_ENT", "CVE_MUN"))

community_women_part2 <- community_women_part1 %>%
  left_join(cngmd, by = c("CVE_ENT", "CVE_MUN"))


## Save data -----

path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors"
save(community_women_part2, file = paste0(path_rf,"community_womens_role.RData"))





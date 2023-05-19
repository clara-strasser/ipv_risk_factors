##################### Community - Public Security Risk Factors ##################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

# Risk Factors -----

## HOMICIDE RATE TOTAL --------
# Variable name: ghr15
# Outcome: count
# Level: municipality
# Aim: create variable "ghr20" (homicide rate total from 2017 to 2021 divided by 2020 population * 100000)

# Read code of "state" and "municipality"
# Remark: needed for matching
codigo <- read_excel(paste0(path,"INEGI_codigo.xlsx"), sheet = 1, skip = 3, col_names = TRUE)
codigo <- codigo %>%
  mutate(cvegeo = paste0(CVE_ENT, CVE_MUN))

# Read homicide record total
# Remark: homicide records were partly manually coded
homicidios <- read_excel(paste0(path,"INEGI_homicidios.xlsx"), sheet = 1, range = cell_rows(15:2151), col_names = TRUE, col_types = c("text", "text", "text", "text", "text", "text", "text", "text"))
homicidios <- homicidios %>%
  dplyr::filter(mun != "No especificado")%>%
  dplyr::filter(is.na(cvegeo)) %>%
  select(-c("cvegeo")) 
homicidios <- homicidios %>%
  left_join(codigo %>% select(NOM_MUN, CVE_ENT, CVE_MUN, cvegeo), by = c("mun" = "NOM_MUN", "cveent" = "CVE_ENT"), keep = FALSE)  %>%
  dplyr::filter(!(mun == "San Juan Mixtepec" & `2019` == "4" & CVE_MUN == "209")) %>%
  dplyr::filter(!(mun == "San Juan Mixtepec" & `2019` == "1" & CVE_MUN == "208")) %>%
  dplyr::filter(!(mun == "San Pedro Mixtepec"))

# Left join with population of 2020
conapo_pop <- read_excel(paste0(path,"conapo_2020.xls"), sheet = 2,col_names = TRUE) %>%
  select(3,4,5)
homicidios <- homicidios %>%
  left_join(conapo_pop, by = c("cvegeo" = "CVE_MUN"))

# Create variable "ghr20"
homicidios <- homicidios %>% 
  mutate_at(vars(`2017`:`2021`), ~ ifelse(grepl("\\.", .), as.numeric(round(as.numeric(.)*1000)), as.numeric(.))) %>%
  mutate(sum_col = rowSums(select(., `2017`:`2021`), na.rm = TRUE),
         ghr20 = sum_col / as.numeric(POB_TOT) * 100000) %>%
  rename(CVE_ENT = cveent) %>%
  select(c("CVE_ENT", "CVE_MUN", "ghr20"))


## HOMICIDE RATE MEN --------
# Variable name: mhr20
# Outcome: count
# Level: municipality
# Aim: create variable "mhr20" (homicide rate men from 2017 to 2021 divided by 2020 population * 100000)

# Read homicide record men
# Remark: homicide records were partly manually coded
homicidios_hombres <- read_excel(paste0(path,"INEGI_homicidios.xlsx"), sheet = 2, range = cell_rows(6:2112), col_names = TRUE,  col_types = rep("text", 18)) %>%
  select(1,2,18) %>%
  filter(Column2 != "No especificado") %>%
  filter(Column2 != "Total") %>%
  filter(!Column1 %in% as.character(1:32)) %>%
  separate(Column1, into = c("CVE_ENT", "CVE_MUN"), sep = " ", remove = FALSE) %>%
  select(-c("Column1", "Column2")) %>%
  mutate(Sum = as.numeric(Sum)) %>%
  mutate_at(vars(Sum), ~ ifelse(grepl("\\.", .), as.numeric(round(as.numeric(.))), as.numeric(.))) 

# Load male population of 2020
poblacion <- read_excel(paste0(path,"poblacion_INEGI.xlsx"), sheet = 1, col_names = TRUE,  col_types = rep("text", 51)) %>%
  select(1,3,6,46)  %>%
  filter(indicador == "Población total hombres" | indicador == "Población total mujeres")
poblacion  <- poblacion[poblacion$cve_municipio != "0", ]
poblacion <- poblacion %>%
  mutate(pob_h = ifelse(indicador == "Población total hombres", `2020`, NA),
         pob_m = ifelse(indicador == "Población total mujeres", `2020`, NA)) %>%
  select(-c("indicador", "2020")) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(pob_h = max(pob_h, na.rm = TRUE), pob_m = max(pob_m, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(CVE_ENT = "cve_entidad",
         CVE_MUN = "cve_municipio") 

# Left join with male population 2020
homicidios_hombres <- homicidios_hombres %>%
  left_join(poblacion, by = c("CVE_ENT", "CVE_MUN")) %>%
  mutate(mhr20 = as.numeric(Sum)/as.numeric(pob_h)*100000) %>%
  select(c("CVE_ENT", "CVE_MUN", "mhr20"))

## HOMOCIDE RATE WOMEN --------
# Variable name: fhr20
# Outcome:
# Level: municipality
# Aim: create variable "fhr20" (homicide rate women from 2017 to 2021 divided by 2020 population * 100000)

# Read homicide record women
# Remark: homicide records were partly manually coded
homicidios_mujeres <- read_excel(paste0(path,"INEGI_homicidios.xlsx"), sheet = 3, range = cell_rows(6:1484), col_names = TRUE,  col_types = rep("text", 18)) %>%
  select(1,2,18) %>%
  filter(Column2 != "No especificado") %>%
  filter(Column2 != "Total") %>%
  filter(!(Column1 %in% c(paste0("0", 1:9), 10:32))) %>%
  separate(Column1, into = c("CVE_ENT", "CVE_MUN"), sep = " ", remove = FALSE) %>%
  select(-c("Column1", "Column2")) %>%
  mutate(Sum = as.numeric(Sum)) %>%
  mutate_at(vars(Sum), ~ ifelse(grepl("\\.", .), as.numeric(round(as.numeric(.))), as.numeric(.))) 


# Left join with female population 2020
homicidios_mujeres <- homicidios_mujeres %>%
  left_join(poblacion, by = c("CVE_ENT", "CVE_MUN")) %>%
  mutate(fhr20 = as.numeric(Sum)/as.numeric(pob_m)*100000) %>%
  select(c("CVE_ENT", "CVE_MUN", "fhr20"))


# Finalize ------

## Join data ------
community_public_part1 <- homicidios %>%
  left_join(homicidios_hombres, by = c("CVE_ENT", "CVE_MUN"))

community_public_part2 <- community_public_part1 %>%
  left_join(homicidios_mujeres, by = c("CVE_ENT", "CVE_MUN"))

## Save data -----

path_rf <- "/Users/clara/Desktop/master_thesis/data/risk_factors/"
save(community_public_part2, file = paste0(path_rf,"community_public_security.RData"))












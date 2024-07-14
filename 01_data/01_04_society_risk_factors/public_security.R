#################### Societal - Public Security Risk Factors ####################


## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/raw_data/other_sources/"

# Risk Factors -----------------------------------------------------------------

## SHARE OF COMMON CRIMES AGAINST MEN/WOMEN NOT REPORTED -------
# Variable name: MasNoDen, FemNoDen
# Outcome: 0-100
# Level: state
envipe_1 <- read_excel(paste0(path,"envipe_2021.xlsx"), sheet = 5, range = cell_rows(10:41), col_names = FALSE)
envipe_2 <- read_excel(paste0(path,"envipe_2021.xlsx"), sheet = 6,range = cell_rows(10:41), col_names = FALSE)

envipe_1 <- envipe_1 %>%
  select(1, 5) %>%   
  rename(entidad = 1, MasNoDen = 2) %>%
  mutate(CVE_ENT = sprintf("%02d", 1:32)) %>%
  select(-c("entidad"))

envipe_2 <- envipe_2 %>%
  select(1, 5) %>%   
  rename(entidad = 1, FemNoDen = 2) %>%
  mutate(CVE_ENT = sprintf("%02d", 1:32)) %>%
  select(-c("entidad"))

envipe <- left_join(envipe_1, envipe_2, by = c("CVE_ENT"))

## PREVALENCE RATE OF COMMON CRIMES AGAINST MEN/WOMEN -----------
# Variable name: MasPrev, FemPrev
# Outcome: continous
# Level: state
envipe_3 <- read_excel(paste0(path,"envipe_2022.xlsx"), sheet = 1, skip= 4, col_names = TRUE)
envipe_3 <- envipe_3[envipe_3$cve_entidad != "00", ]
envipe_3 <- envipe_3 %>%
  filter(indicador %in% c("Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, mujeres",
                          "Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, hombres")) %>%
  select(indicador, everything()) %>%
  mutate(MasPrev = ifelse(indicador == "Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, hombres", `2020`, NA),
         FemPrev = ifelse(indicador == "Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, mujeres", `2020`, NA)) %>%
  select(-c("indicador", "id_indicador", "desc_entidad", "desc_municipio", "2010":"2022", "unidad_medida")) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(MasPrev = max(MasPrev, na.rm = TRUE), FemPrev = max(FemPrev, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(CVE_ENT = "cve_entidad") %>%
  select(-c("cve_municipio"))


# Finalize ------

## Join data ------
societal_public_security <- envipe %>%
  left_join(envipe_3, by = c("CVE_ENT")) %>%
  select(CVE_ENT, everything())

## Save data -----

path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors/"
save(societal_public_security, file = paste0(path_rf,"societal_public_security.RData"))





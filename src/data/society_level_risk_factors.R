######################## Society Level Risk Factors #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(readxl)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------


# Risk Factors -----

## SHARE OF COMMON CRIMES AGAINST MEN/WOMEN NOT REPORTED -------
# Variable name: MasNoDen, FemNoDen
# Outcome: 0-100
# Level: state
envipe_1 <- read_excel(paste0(path,"envipe_2021.xlsx"), sheet = 5, range = cell_rows(10:41), col_names = FALSE)
envipe_2 <- read_excel(paste0(path,"envipe_2021.xlsx"), sheet = 6,range = cell_rows(10:41), col_names = FALSE)

envipe_1 <- envipe_1 %>%
  select(1, 5) %>%   
  rename(entidad = 1, MasNoDen = 2) %>%
  mutate(cve_ent = sprintf("%02d", 1:32)) %>%
  select(-c("entidad"))

envipe_2 <- envipe_2 %>%
  select(1, 5) %>%   
  rename(entidad = 1, FemNoDen = 2) %>%
  mutate(cve_ent = sprintf("%02d", 1:32)) %>%
  select(-c("entidad"))

envipe <- left_join(envipe_1, envipe_2, by = c("cve_ent"))

## PREVALENCE RATE OF COMMON CRIMES AGAINST MEN/WOMEN -----------
# Variable name: MasPrev, FemPrev
# Outcome: continous
# Level: state
envipe <- read_excel(paste0(path,"envipe_2022.xlsx"), sheet = 1, skip= 4, col_names = TRUE)
envipe <- envipe[envipe$cve_entidad != "00", ]
envipe <- envipe %>%
  filter(indicador %in% c("Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, mujeres",
                          "Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, hombres")) %>%
  select(indicador, everything()) %>%
  mutate(MasPrev = ifelse(indicador == "Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, hombres", `2020`, NA),
         FemPrev = ifelse(indicador == "Tasa de prevalencia delictiva por cada cien mil habitantes de 18 años y más, mujeres", `2020`, NA)) %>%
  select(-c("indicador", "id_indicador", "desc_entidad", "desc_municipio", "2010":"2022", "unidad_medida")) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(MasPrev = max(MasPrev, na.rm = TRUE), FemPrev = max(FemPrev, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cvegeo=paste0(cve_entidad, cve_municipio)) %>%
  rename(cveent = "cve_entidad") %>%
  select(-c("cve_municipio"))

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
  mutate(cve_ent = sprintf("%02d", 1:32)) %>%
  select(-c("entidad", "rel1", "rel2"))


## SHARE OF POPULATION SATISFIED WITH BASIC PUBLIC SERVICES ----------
# Variable name: satis25
# Outcome:
# Level: state











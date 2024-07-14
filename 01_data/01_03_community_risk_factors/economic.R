####################### Community - Economic Risk Factors ##################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/raw_data/other_sources/"

# Risk Factors -----------------------------------------------------------------

## GINI INDEX ----
# Variable name: gini20
# Outcome: 0-1
# Level: municipality
coneval <- read_excel(paste0(path,"coneval_2023.xlsx"), sheet = 3, skip = 2, range = "A8:F2479", col_names = TRUE)
coneval <- coneval[-c(1:2), -1]
coneval <- coneval %>%
  rename(CVE_ENT = "Clave de entidad",
         CVE_MUN = "Clave de municipio",
         gini20 = "Coeficiente de Gini") %>%
  mutate(CVE_MUN = substr(CVE_MUN, 3, nchar(CVE_MUN))) %>%
  select(-c("Entidad  federativa", "Municipio"))

## HUMAN DEVELOPEMENT INDEX -----
# Variable name: idh2020
# Outcome: 0-1
# Level: municipality
undp <- read_excel(paste0(path,"undp_2020.xlsx"), sheet = 1, col_names = TRUE)
undp <- undp %>%
  select(c("AGEE", "AGEM", "IDH")) %>%
  rename(CVE_ENT = "AGEE",
         CVE_MUN = "AGEM",
         idh2020 = "IDH") 


# Finalize ------

## Join data ------
community_economic <- coneval %>%
  left_join(undp, by = c("CVE_ENT", "CVE_MUN"))

## Small corrections ------
community_economic <- community_economic %>%
  mutate(gini20 = ifelse(gini20 == "n.d.", NA, gini20),
         idh2020 = ifelse(idh2020 == "-/-", NA, idh2020))


## Save data -----

path_rf <- "/Users/clarastrasser/ipv_data/raw_data/main_source/risk_factors"
save(community_economic, file = paste0(path_rf,"community_economic.RData"))












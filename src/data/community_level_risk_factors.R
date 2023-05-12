######################## Community Level Risk Factors #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(readxl)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------


# Risk Factors -----

## GINI INDEX ----
# Variable name: gini20
# Outcome: 0-1
# Level: municipality
coneval <- read_excel(paste0(path,"coneval_2023.xlsx"), sheet = 3, skip = 2, range = "A8:F2479", col_names = TRUE)
coneval <- coneval[-c(1:2), -1]
coneval <- coneval %>%
  rename(cveent = "Clave de entidad",
         cvegeo = "Clave de municipio",
         gini20 = "Coeficiente de Gini") %>%
  select(-c("Entidad  federativa", "Municipio"))

## HUMAN DEVELOPEMENT INDEX -----
# Variable name: idh2020
# Outcome: 0-1
# Level: municipality
undp <- read_excel(paste0(path,"undp_2020.xlsx"), sheet = 1, col_names = TRUE)
undp <- undp %>%
  select(c("AGEE", "AGEM", "IDH")) %>%
  mutate(cvegeo=paste0(AGEE,AGEM)) %>%
  rename(cveent = "AGEE",
         idh2020 = "IDH") %>%
  select(-c("AGEM"))


## MUNICIPAL FUNCTIONAL CAPACITIES ------
# Variable name: icfm
# Outcome: 0-1
# Level: municipality
# Remark: was excluded, last version: 2016

## WOMENS/MENS ECONOMICALLY ACTIVE POPULATION -------
# Variable name: pea_f and pea_m
# Outcome: %
# Level: municipality
intercensal <- read_excel(paste0(path,"intercensal_survey_2020.xlsx"), sheet = 1, skip = 4, col_names = TRUE)
intercensal <- intercensal[intercensal$indicador %in% c("Porcentaje de la población femenina de 12 años y más económicamente activa",
                                                            "Porcentaje de la población masculina de 12 años y más económicamente activa"),]
intercensal <- intercensal[intercensal$cve_municipio != "0", ]
intercensal <- intercensal %>%
  mutate(pea_f = ifelse(indicador == "Porcentaje de la población femenina de 12 años y más económicamente activa", `2020`, NA),
         pea_m = ifelse(indicador == "Porcentaje de la población masculina de 12 años y más económicamente activa", `2020`, NA)) %>%
  select(-c("indicador", "2020", "id_indicador", "desc_entidad", "desc_municipio"))
intercensal <- intercensal %>%
  group_by(cve_entidad, cve_municipio, unidad_medida) %>%
  summarise(pea_f = max(pea_f, na.rm = TRUE), pea_m = max(pea_m, na.rm = TRUE)) %>%
  ungroup()
intercensal <- intercensal %>%
  mutate(cvegeo=paste0(cve_entidad, cve_municipio)) %>%
  rename(cveent = "cve_entidad") %>%
  select(-c("cve_municipio", "unidad_medida"))

## WOMENS POLITICAL PARTICIPATION ------
# Variable name: ParPolF
# Outcome: share
# Level: municipality
cngmd <- read_excel(paste0(path,"cngmd_2020.xlsx"), sheet = 3, skip = 3, col_names = TRUE)
cngmd <- cngmd[1:2501, ]
cngmd <- cngmd %>%
  select(c("Clave\r\nEntidad", "Clave\r\nMunicipio o demarcación", "Total", "Mujeres")) %>%
  rename(cvemun = "Clave\r\nMunicipio o demarcación",
         cveent= "Clave\r\nEntidad") %>%
  mutate(cvegeo= paste0(cveent,cvemun)) %>%
  mutate(ParPolF = as.numeric(Mujeres)/as.numeric(Total)) %>%
  select(-c("Mujeres", "Total"))
cngmd <- cngmd %>% filter(cvemun != "000")

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
  select(-c("indicador", "2020"))

migracion_inegi <- migracion_inegi %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(pres_2020_f = max(pres_2020_f, na.rm = TRUE), pres_2020_m = max(pres_2020_m, na.rm = TRUE)) %>%
  ungroup()

migracion_inegi <- migracion_inegi %>%
  mutate(cvegeo=paste0(cve_entidad, cve_municipio)) %>%
  rename(cveent = "cve_entidad") %>%
  select(-c("cve_municipio"))

## WOMEN HOUSEHOLD HEADSHIP ------
# Variable name: phogjef_f
# Outcome: %
# Level: municipality
intercensal <- read_excel(paste0(path,"intercensal_survey_2020.xlsx"), sheet = 1, skip = 4, col_names = TRUE)
intercensal <- intercensal[intercensal$indicador %in% c("Hogares censales",
                                                        "Hogares con jefatura femenina"),]

intercensal <- intercensal[intercensal$cve_municipio != "0", ]

intercensal <- intercensal %>%
  select(-c("id_indicador", "desc_entidad", "desc_municipio","unidad_medida")) %>%
  mutate(hogar_total = ifelse(indicador == "Hogares censales", `2020`, NA_character_),
         hogar_fem = ifelse(indicador == "Hogares con jefatura femenina", `2020`, NA_character_)) %>%
  group_by(cve_entidad, cve_municipio) %>%
  summarise(hogar_total = max(hogar_total, na.rm = TRUE), hogar_fem = max(hogar_fem, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(phogjef_f = as.numeric(hogar_fem)/as.numeric(hogar_total),
         cvegeo=paste0(cve_entidad, cve_municipio)) %>%
  rename(cveent = "cve_entidad") %>%
  select(-c("cve_municipio", "hogar_total", "hogar_fem"))
















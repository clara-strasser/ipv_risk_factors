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

## Human Developement Index -----
# Variable name: idh2020
# Outcome: 0-1
# Level: municipality
undp <- read_excel(paste0(path,"undp_2020.xlsx"), sheet = 1, col_names = TRUE)
undp <- undp %>%
  select(c("AGEE", "AGEM", "IDH"))














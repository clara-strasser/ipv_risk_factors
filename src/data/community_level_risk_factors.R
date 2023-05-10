######################## Community Level Risk Factors #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(readxl)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------
coneval <- read_excel(paste0(path,"coneval_2023.xlsx"), sheet = 3, skip = 2, range = "A8:F2479", col_names = TRUE)
coneval <- coneval[-c(1:2), -1]
coneval <- coneval %>%
  rename(cveent = "Clave de entidad",
         cvegeo = "Clave de municipio",
         gini20 = "Coeficiente de Gini") %>%
  select(-c("Entidad  federativa", "Municipio"))

# Risk Factors -----

## GINI INDEX ----
# Variable name: gini20
# Outcome: 0-1
# Level: municipality


















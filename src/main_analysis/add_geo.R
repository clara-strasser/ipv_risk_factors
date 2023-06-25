###################### Add Spatial Variables and FAC_MUJ #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_additionals <- "/Users/clara/Desktop/master_thesis/data/"
path_save <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "step3_endireh.RData")) # main data
load(paste0(path_additionals,"TB_SEC_IVaVD.RData")) # FAC_MUJ
centroids <- read_excel(paste0(path_additionals,"inegi_municipios_geo.xlsx"), sheet = 1, col_names = TRUE) # Centroids


## Prep data -----

# centroids
centroids <- centroids %>%
  rename(cvegeo = CVEGEO) %>%
  mutate(cvegeo = as.factor(cvegeo)) %>%
  select(c("cvegeo", "lon", "lat"))

# fac_muj
fac_muj <- TB_SEC_IVaVD %>%
  select(c("ID_PER", "FAC_MUJ"))

## Change data name -----
data <- step3_endireh

## Subset not needed -----
data <- data %>%
  select(-c("num_hij_par", "num_hij_par_muj")) %>%
  rename(Marg20high = Marg15high,
         Marg20low = Marg15low,
         Marg20medium = Marg15medium,
         Marg20very_high = Marg15very_high,
         Marg20very_low = Marg15very_low)

## Add spatial Variables --------

# Municipal and State Factors
# cveent and cvegeo (CVE_ENT + CVE_MUN)
data$cveent <- as.factor(data$CVE_ENT)
data$cvegeo <- as.factor(paste0(data$CVE_ENT,data$CVE_MUN))

# Municipal Centroid Coordinates
# lon  and lat
data <- data %>%
  left_join(centroids, by = c("cvegeo"))

## Add FAC_MUJ --------
data <- data %>%
  left_join(fac_muj, by= c("ID_PER"))


# Save data ----------
# Data set: 61.205 with geo information + FAC_MUJ
data_imp_pmm_m1 <- data
save(data_imp_pmm_m1, file = paste0(path_save,"data_imp_pmm_m1.RData"))









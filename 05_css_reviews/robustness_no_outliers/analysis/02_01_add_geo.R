###################### Add Spatial Variables and FAC_MUJ #########################


## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

## Set path --------------------------------------------------------------------
path_data <- "/dss/dsshome1/0B/ru23kek2/data/prep_data/"
path_additionals_main <- "/dss/dsshome1/0B/ru23kek2/data/raw_data/main_source/"
path_additionals_other <- "//dss/dsshome1/0B/ru23kek2/data/raw_data/other_sources/"
path_save <- "/dss/dsshome1/0B/ru23kek2/data/final_data/rob_no_outliers/"

## Load data ------------------------------------------------------------------
load(paste0(path_data, "step2_endireh.RData")) # main data
load(paste0(path_additionals_main,"TB_SEC_IVaVD.RData")) # FAC_MUJ
centroids <- read_excel(paste0(path_additionals_other,"inegi_municipios_geo.xlsx"), sheet = 1, col_names = TRUE) # Centroids


## Prep data -------------------------------------------------------------------

# centroids
centroids <- centroids %>%
  rename(cvegeo = CVEGEO) %>%
  mutate(cvegeo = as.factor(cvegeo)) %>%
  select(c("cvegeo", "lon", "lat"))

# fac_muj
fac_muj <- TB_SEC_IVaVD %>%
  select(c("ID_PER", "FAC_MUJ"))

## Change data name -----
data <- step2_endireh

## Subset not needed -----
data <- data %>%
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
data_imp_pmm_no_outliers <- data
save(data_imp_pmm_no_outliers, file = paste0(path_save,"data_imp_pmm_no_outliers.RData"))









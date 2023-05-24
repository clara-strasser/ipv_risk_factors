#############################  Finalize Data ##################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data -------
load(paste0(path, "endireh_2021.RData"))

# Subset Main ---------
# Beginning: 110127 observations

## Gender partner:
summary(endireh_2021$par_sex)
#  male    female  NAs 
# 104656    560   4911
endireh_2021 <- subset(endireh_2021, par_sex == "male")

## Married/ Cohabiting Women
table(endireh_2021$T_INSTRUM)
#    A1    A2    B1    B2    C1 
#  65899  2398 12537  9535 14287 
# In total: 68297
endireh_2021 <- subset(endireh_2021, T_INSTRUM == "A1" | T_INSTRUM == "A2")

## Women with children
table(endireh_2021$num_hij)
#  0      >0
# 5145   63152
# In total: 63152
endireh_2021 <- subset(endireh_2021, as.numeric(num_hij) > 0)

# Subset NA Observations -----

# Calculate NAs of all variables
endireh_2021 %>%
  summarise_all(list(missing = ~sum(is.na(.))))

# Income Women:
# ing_muj: 1893
# ingm_muj: 1908

# Age Women:
# EDAD: 44

# Indigena Women:
# indigena: 733

# Sexual Violence Experience Women:
# vio_sex_inf: 2683

# Age at first child:
# eda_hij: 424

# Age at first sexual intercourse:
# eda_sex: 1344

# Age at first marriage:
# eda_mat: 599

# Age partner first marriage:
# mot_mat: 1699

# Age partner:
# eda_par2: 1555

# Education partner:
# 63152! All observations are missing, observations not given for A1 and A2

# Indiginous partner:
# 63152! All observations are missing, observations not given for A1 and A2

# Income partner:
# ing_par: 10556
# ingm_par: 10592

# Violence experience partner:
# vio_exp_inf: 12989

# Violence witness partner:
# vio_inf_par: 16230 

# Violence experience women:
# vio_fis_ex/ vio_emo_ex/ vio_sex_ex / vio_eco_ex: 52950 (only 10202 have observations, the 19 %)

# Sexual Liberty:
# lib_sex: 1286 

# Economical Liberty:
# lib_eco: 161

# Social Liberty:
# lib_soc: 91

# Household distribution:
# act_dist: 217

# IDH:
# idh2020: 81

# ParPolF:
# ParPolF: 1459

# Total homocide rate:
# ghr20:934

# Men homocide rate:
# mhr20: 996

# Women homocide rate:
# fhr20:  5573

# Emotional IPV:
# vio_emo_año: 42728

# Subset and Test Part I
endireh_2021_test <- endireh_2021 %>%
  filter(!is.na(vio_emo_año))
endireh_2021_test <- endireh_2021_test %>%
  select(-c("edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par", 
            "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex", 
            "vio_exp_inf_par", "vio_inf_par", "ing_muj", "ing_par", "vio_sex_inf"))
endireh_2021_test %>%
  summarise_all(list(missing = ~sum(is.na(.))))

# Remove following columns:
endireh_2021_test <- endireh_2021_test[complete.cases(endireh_2021_test), ]

# Subset and Test Part II
endireh_2021_test_2 <- endireh_2021
endireh_2021_test_2 <- endireh_2021_test_2 %>%
  select(-c("edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par", 
            "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex", "vio_emo_año",
            "vio_exp_inf_par", "vio_inf_par", "ing_muj", "ing_par", "vio_sex_inf"))
endireh_2021_test_2 %>%
  summarise_all(list(missing = ~sum(is.na(.))))

# Remove following columns:
endireh_2021_test_2 <- endireh_2021_test_2[complete.cases(endireh_2021_test_2), ]
































#############################  Finalize Data ##################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set
load(paste0(path, "additional_risk_factors.RData")) # additional risk factors
load(paste0(path, "emotional_ipv_vida.RData")) # additional outcome variables

## Join additional risk factors ------
endireh_2021 <- endireh_2021 %>%
  left_join(additional_risk_factors, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


## Join alternative emotional ipv ------
endireh_2021 <- endireh_2021 %>%
  left_join(emotional_ipv_vida, by = c("ID_PER", "ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))


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
missing_counts <- sapply(endireh_2021, function(x) sum(is.na(x)))
missing_counts <- missing_counts[order(missing_counts, decreasing = TRUE)]
print(missing_counts)

# Remove risk factors with missings > 30 %
# edu_parlow
# edu_parmedium
# edu_parhigh        
# ind_par 
# sep_ex 
# vio_fis_ex 
# vio_emo_ex 
# vio_sex_ex  
# vio_eco_ex  
# tipo_empl
# desempleo
endireh_2021 <- endireh_2021 %>%
  select(-c("edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par", 
            "sep_ex", "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex",
            "desempleo", "tipo_empl"))

# Distribution of emotional ipv
# vio_emo_año:
table(endireh_2021$vio_emo_año)
# no   yes 
# 5993 14431
# ratio no/yes: 0.41

# vio_emo_año_alt
table(endireh_2021$vio_emo_año_alt)
#  no   yes 
# 48721 14431 
# ratio no/yes: 3.3

# vio_emo_vida
table(endireh_2021$vio_emo_vida)
#   no   yes 
# 42728 20424
# ratio no/yes: 2.1


# Save data -----
save(endireh_2021, file = paste0(path,"endireh_2021.RData"))


# Plausibility Analysis ---------

## vio_emo_año_alt and vio_emo_vida
# vio_emo_año and vio_emo_vida
# Explanation: vio_emo_año_alt may not be "yes" if vio_emo_vida is "no"
# vio_emo_año may not be "yes" if vio_emo_vida is "no"
plaus_1a <- endireh_2021[endireh_2021$vio_emo_año_alt == "yes" & endireh_2021$vio_emo_vida == "no", ]
plaus_1b <- endireh_2021[endireh_2021$vio_emo_año == "yes" & !is.na(endireh_2021$vio_emo_año) & endireh_2021$vio_emo_vida == "no", ]

## eda_sex
# Explanation: Women’s age at first sexual intercourse cannot be greater than women’s age at the time of being surveyed
plaus_2 <- endireh_2021[!is.na(endireh_2021$eda_sex) & !is.na(endireh_2021$EDAD) & endireh_2021$eda_sex > endireh_2021$EDAD, ]

## eda_mat
# Explanation: Women’s age at first marriage (or at cohabitation) cannot be greater than women’s age at the time of being surveyed
plaus_3 <- endireh_2021[!is.na(endireh_2021$eda_mat) & !is.na(endireh_2021$EDAD) & endireh_2021$eda_mat > endireh_2021$EDAD, ]

## eda_hij
# Explanation: Women’s age at first childbirth cannot be greater than women’s age at the time of being surveyed
plaus_4 <- endireh_2021[!is.na(endireh_2021$eda_hij) & !is.na(endireh_2021$EDAD) & endireh_2021$eda_hij > endireh_2021$EDAD, ]


# Results: no implausible results found!



# Create two data frames
# First: final_alt1 --> vio_emo_año 
# Second: final_alt2 --> vio_emo_año_alt

final_alt1 <- endireh_2021 %>%
  select(-c( "vio_emo_año_alt"))
final_alt2 <- endireh_2021 %>%
  select(-c( "vio_emo_año"))


# Alternative 1:
# Step 1: Remove missings of "vio_emo_año"
# Step 2: Find combination of columns with least missings
# Step 3: create df with no misisngs

# STEP 1:
final_alt1 <- final_alt1 %>%
  filter(!is.na(vio_emo_año))

# STEP 2:
# Remove:
# vio_inf_par
# vio_exp_inf_par
final_alt1 <- final_alt1 %>%
  select(-c( "vio_inf_par", "vio_exp_inf_par"))

# STEP 3:
# Keep non-missing
final_alt1 <- final_alt1[complete.cases(final_alt1), ]


# STEP 4:
# n = 13.097


# Alternative 2:

# STEP 1:

# STEP 2:
# Remove:
# vio_inf_par
# vio_exp_inf_par
final_alt2 <- final_alt2 %>%
  select(-c( "vio_inf_par"))

# STEP 3:
# Keep non-missing
final_alt2 <- final_alt2[complete.cases(final_alt2), ]


# STEP 4:
# n = 32.597



missing_counts <- sapply(final_alt2, function(x) sum(is.na(x)))
missing_counts <- missing_counts[order(missing_counts, decreasing = TRUE)]
missing_counts




































########################### Step 1: Complete Cases ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(mice) # relevant for step 1
library(VIM) # relevant for step 1
library(naniar)

## Load functions -----
source(file = "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/functions/propplot.R")

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"


## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set

# Data Preparation Process ------

## Prepare data -----

# Convert characters to numeric
endireh_2021 <- endireh_2021 %>%
  mutate(pres_2020_f = as.numeric(pres_2020_f),
         pres_2020_m = as.numeric(pres_2020_m),
         gini20 = as.numeric(gini20),
         idh2020 = as.numeric(idh2020),
         pea_f = as.numeric(pea_f),
         pea_m = as.numeric(pea_m),
         MasPrev = as.numeric(MasPrev),
         FemPrev = as.numeric(FemPrev))

# Remove unnecessary risk factors 
# edu_parlow
# edu_parmedium
# edu_parhigh        
# ind_par 
# sep_ex 
# vio_fis_ex 
# vio_emo_ex 
# vio_sex_ex  
# vio_eco_ex  
# ing_par
# ing_muj
endireh_2021 <- endireh_2021 %>%
  select(-c("edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par", 
            "sep_ex", "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex", "ing_par", "ing_muj"))

## Initial Descriptives -----
# Initial data set: 63.152

# Calculate NAs of all variables
print(data.frame(Variable = names(endireh_2021), 
                 Count = sapply(endireh_2021, function(x) sum(is.na(x))), 
                 Percentage = sapply(endireh_2021, function(x) sum(is.na(x)) / length(x) * 100)))

# Keep only columns with missing data
endireh_missing <- endireh_2021 %>%
  select(where(~ any(is.na(.))))

# See percent of missings
gg_miss_var(endireh_missing)

# Pattern of missing data
missing_pattern <- md.pattern(endireh_missing)

# Plot missing data combinations
gg_miss_upset(endireh_missing, nsets =30, nintersects = 60, 
              sets.x.label = "Number of Missings by Variable", mainbar.y.label = "Number of Missings by Combination",
              mb.ratio = c(0.4, 0.6), shade.alpha = 0.5, matrix.color = "#002b58", sets.bar.color = "#002b58",
              main.bar.color = "#002b58")


## Missing Data Imputation ------

### Variant 1: Predictive Mean Matching -----

# Remove multi-collinear columns
step1_df <- endireh_2021 %>%
  select(-c("ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM")) %>% # remove as constant
  select(-ends_with("high")) %>%  # remove
  select(-ends_with("both")) %>% # remove
  select(-ends_with("comhigh_urban")) # remove

# Use "predictive mean matching" (pmm) to impute missing values
step1_df <- mice(step1_df, m=1, maxit = 5, meth = "pmm", seed=800)

# Create complete data set
step1_completed <- complete(step1_df, 1)

# Test Tabular
summary(endireh_2021)
summary(step1_completed)

# Test Graph and save
densityplot(step1_df) # for continuous
propplot(step1_df) # for factors


# Create complete data set
step1_endireh <- endireh_2021 %>%
  mutate(across(everything(), ~ coalesce(., step1_completed[[cur_column()]])))

# Replace NAs in factor variables
step1_endireh <- step1_endireh %>%
  mutate(lib_sex_gradhigh = factor(ifelse(lib_sex_gradmedium == "no" & lib_sex_gradlow == "no", 2, 1),
                                   levels = c(1, 2),
                                   labels = c("no", "yes"))) %>%
  mutate(lib_eco_gradhigh = factor(ifelse(lib_eco_gradmedium == "no" & lib_eco_gradlow == "no", 2, 1),
                                   levels = c(1, 2),
                                   labels = c("no", "yes"))) %>%
  mutate(lib_soc_gradhigh = factor(ifelse(lib_soc_gradmedium == "no" & lib_soc_gradlow == "no", 2, 1),
                                   levels = c(1, 2),
                                   labels = c("no", "yes"))) %>%
  mutate(act_distboth = factor(ifelse(act_distfemales == "no" & act_distmales == "no", 2, 1),
                                 levels = c(1, 2),
                                 labels = c("no", "yes")))

# Examples:
# 0206256.01.1.02
# 2360033.02.1.02
# 3202611.06.1.02

# Save data
save(step1_endireh, file = paste0(path_save,"step1_endireh.RData"))


### Variant 2: Classification and Regression Trees -----

# Use "classification and regression trees" (cart) to impute missing values
step1_df_alt <- mice(endireh_2021, m=1, maxit = 5, meth = "cart", seed=800)

# Create complete data set
step1_endireh_alt <- complete(step1_df_alt, 1)

# Test
summary(endireh_2021)
summary(step1_endireh_alt)
densityplot(step1_df_alt) # for continuous
propplot(step1_df_alt) # for factors

# Save data
save(step1_endireh_alt, file = paste0(path_save,"step1_endireh_alt.RData"))


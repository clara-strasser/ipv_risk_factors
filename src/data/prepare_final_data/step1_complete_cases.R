################### Step 1:  Prepare Data - Complete Cases ####################

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


## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/"


## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set

# Data Preparation Process ------

## Descriptives -----
# Initial data set: 63.152

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

# Calculate NAs of all variables
print(data.frame(Variable = names(endireh_2021), 
                 Count = sapply(endireh_2021, function(x) sum(is.na(x))), 
                 Percentage = sapply(endireh_2021, function(x) sum(is.na(x)) / length(x) * 100)))

# Remove non-relevant risk factors 
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

# Keep only columns with missing data
endireh_missing <- endireh_2021 %>%
  select(where(~ any(is.na(.))))

# See percent of missings
gg_miss_var(endireh_missing)

# Pattern of missing data
md.pattern(endireh_missing)

# Plot missing data combinations
gg_miss_upset(endireh_missing, nsets =30, nintersects = 60, 
              sets.x.label = "Number of Missings by Variable", mainbar.y.label = "Number of Missings by Combination",
              mb.ratio = c(0.4, 0.6), shade.alpha = 0.5, matrix.color = "#002b58", sets.bar.color = "#002b58",
              main.bar.color = "#002b58")


## Missing Data Imputation ------

# Variant 1: Predictive Mean Matching

# remove non-necessary columns
step1_df <- endireh_2021 %>%
  select(-c("ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "par_sex", "pea_m", "pres_2020_m", "tipo_empl"))
step1_df <- step1_df %>% select(-ends_with("high"))
step1_df <- step1_df %>% select(-ends_with("both"))
step1_df <- mice(step1_df, m=1, maxit = 1, meth='pmm', seed=800)

completedData <- complete(step1_df, 1)
densityplot(step1_df)
propplot(step1_df)





# Impute missing data using "predictive mean matching"
endireh_missing <- endireh_missing %>%
  select(-c("eda_par2", "act_distfemales",  "act_distmales",  "act_distboth", "lib_sex_gradmedium",  "lib_sex_gradlow"))
endireh_2021_no_missings <- mice(endireh_2021, m =1, method = "cart")
endireh_2021_no_missings$imp$vio_exp_inf_par
completedData <- complete(endireh_2021_no_missings,1)

gg_miss_var(completedData)

table(completedData$vio_exp_inf_par)



# Keep complete cases
# Meaning: remove all observations with min. one missing value in the covariates 
emo_ipv_final <- endireh_2021[complete.cases(endireh_2021), ]

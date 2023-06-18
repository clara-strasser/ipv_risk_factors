######################### Step 1: Complete Cases - TEST ########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(arsenal) # useful for comparison
library(cowplot)


## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"


## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set
load(paste0(path_save, "step1_endireh.RData"))
load(paste0(path_save, "step1_endireh_alt.RData"))

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


# Test Single Imputation ------
# Remark: single imputation (instead of multiple imputation) was done using "pmm"
# Trade-off: bias and variance

## Graphical comparison ------

# EDAD
# Create histogram for endireh_2021
hist_endireh <- ggplot(endireh_2021, aes(x = EDAD)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Distribution of EDAD - endireh_2021", x = "EDAD", y = "Frequency")

# Create histogram for step1_endireh
hist_step1 <- ggplot(step1_endireh, aes(x = EDAD)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Distribution of EDAD - step1_endireh", x = "EDAD", y = "Frequency")

# Combine the histograms into a single plot
combined_plot <- plot_grid(hist_endireh, hist_step1, nrow = 1)

# Display the combined plot
print(combined_plot)

# ingm_par
# Create histogram for endireh_2021
hist_endireh <- ggplot(endireh_2021, aes(x = ingm_par)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 20) +
  labs(title = "Distribution of ingm_par - endireh_2021", x = "ingm_par", y = "Frequency")

# Create histogram for step1_endireh
hist_step1 <- ggplot(step1_endireh, aes(x = ingm_par)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  labs(title = "Distribution of ingm_par - step1_endireh", x = "ingm_par", y = "Frequency")

# Combine the histograms into a single plot
combined_plot <- plot_grid(hist_endireh, hist_step1, nrow = 1)

# Display the combined plot
print(combined_plot)

## Tabular comparison -------

# EDAD:
summary(endireh_2021$EDAD)
summary(step1_endireh$EDAD)


# ingm_par
summary(endireh_2021$ingm_par)
summary(step1_endireh$ingm_par)


# vio_inf_par
summary(endireh_2021$vio_inf_par)
summary(step1_endireh$vio_inf_par)

# vio_exp_inf_par
summary(endireh_2021$vio_exp_inf_par)
summary(step1_endireh$vio_exp_inf_par)

# fhr20
summary(endireh_2021$fhr20)
summary(step1_endireh$fhr20)

# idh2020
summary(endireh_2021$idh2020)
summary(step1_endireh$idh2020)


# Comparison
summary <- summary(comparedf(endireh_2021, step1_endireh))
summary[["frame.summary.table"]] # shows how the NAs were substituted



## Statistical test -------







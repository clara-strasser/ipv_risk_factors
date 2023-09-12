######################## Analysis - Linear Regression #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mgcv)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

## Change data name -----
data <- data_imp_pmm_m1
# data <- data_no_imp
rm(data_imp_pmm_m1)

## Prepare data ------

### Income difference -----
data <- data %>%
  mutate(ingm_dif = ingm_par - ingm_muj)

### Log transform -----
data <- data %>%
  mutate(log_ingm_muj = log1p(ingm_muj),
         log_ingm_par = log1p(ingm_par))

### Age difference -------
data <- data %>%
  mutate(edad_dif = eda_par2 - EDAD)

### Convert outcome ------
data$vio_emo_año <- factor(data$vio_emo_año, levels = c("no", "yes"), labels = c("0", "1"))


## Run model ------

### GAM with linear effects ------
offset_var <- rep(pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                      w = data$FAC_MUJ))-0.5, nrow(data))
model <- gam(vio_emo_año ~ desempleo + vio_inf + vio_exp_inf + 
               vio_sex_inf + con_sex + mot_mat + vio_inf_par + 
               vio_exp_inf_par + act_distboth + act_distmales + lib_sex_gradmedium +
             lib_eco_gradmedium + redsoc_gradhigh + rout_gradmedium + MasNoDen, 
             family = binomial(link = "probit"), 
             weights = data$FAC_MUJ,
             offset = offset_var,
             data = data)
summary(model)















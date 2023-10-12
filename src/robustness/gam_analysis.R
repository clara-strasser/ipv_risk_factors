######################## Analysis - Linear Regression #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mgcv)
library(margins)
library(stats)
library(survey)
library(xtable)
library(stargazer)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

## Change data name -----
data <- data_imp_pmm_m1
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

### Demean --------
numerical_var_names <- c("num_hij", "ingm_muj", "EDAD", "eda_hij", "eda_sex", "eda_mat", "eda_par2", "hacin",
                         "ingm_par", "POB_TOT", "pres_2020_f", "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m",
                         "phogjef_f", "ParPolF", "ghr20", "mhr20", "fhr20", "MasNoDen", "FemNoDen", "MasPrev",
                         "FemPrev", "cor19", "satis19", "lon", "lat", "log_ingm_muj", "log_ingm_par",
                         "edad_dif", "ingm_dif")
data <- data %>%
  mutate_at(vars(all_of(numerical_var_names)), as.numeric)
for(i in c(numerical_var_names)){
  data[, i] <- scale(data[, i], center = TRUE, scale = FALSE)
} 

## Run model ------

### GLM with linear effects ------
offset_var <- rep(pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                      w = data$FAC_MUJ))-0.5, nrow(data))

dstrat<-svydesign(id=~1,weights=~data$FAC_MUJ, data=data)
model1 <- svyglm(vio_emo_año ~ desempleo + vio_inf + vio_exp_inf + 
               vio_sex_inf + con_sex*eda_sex + mot_mat + vio_inf_par + 
               vio_exp_inf_par + act_distboth + act_distmales + lib_sex_gradmedium +
               lib_eco_gradmedium + redsoc_gradhigh + rout_gradmedium + MasNoDen, 
              design = dstrat,
             family = binomial(link = "probit"), 
             offset = offset_var,
             data = data)
summary(model1)

### GAM with linear effects ------
model_gam <- gam(vio_emo_año ~ indigena + edad_dif*niv_edmedium + niv_edhigh +
                cct_rec + desempleo + vio_inf + vio_exp_inf + vio_sex_inf + 
                s(num_hij, bs="ps") + con_sex*eda_sex + ti(eda_sex,edad_dif) +
                s(eda_mat, bs="ps") + mot_mat + vio_inf_par + 
                vio_exp_inf_par +  s(hacin, bs="ps") + act_distboth + 
                act_distmales + feminist_gradmedium + lib_sex_gradmedium +
                lib_sex_gradhigh + lib_eco_gradmedium + lib_eco_gradhigh + lib_soc_gradmedium +
                lib_soc_gradhigh + redsoc_gradhigh + rout_gradmedium + rout_gradhigh +
                s(log_ingm_muj, bs="ps") + log_ingm_par + ti(log_ingm_muj, log_ingm_par)+
                + mhr20 + s(phogjef_f, bs="ps") + pres_2020_f + gini20 +
                pea_f + Marg20low + Marg20high + Marg20very_high + Type_comlow_urban +
                te(lon, lat) + s(FemPrev, bs="ps") + s(MasNoDen, bs="ps")  + 
                s(cor19, bs="ps") + s(cveent, bs='re') + s(cvegeo, bs='re'),
              family = binomial(link = "probit"), 
              weights = FAC_MUJ,
              offset = offset_var,
              data = data)
summary(model_gam)
save(model_gam,  file = "model_gam.RData")

### Save Effects

# Model 1
xtable(model1)
model1




############################ Main Analysis #############################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mboost)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

## Change data name -----
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)

## Prepare data ------

### Log transform -----
summary(data$ingm_muj)
summary(data$ingm_par)
data <- data %>%
  mutate(log_ingm_muj = log1p(ingm_muj),
         log_ingm_par = log1p(ingm_par))

summary(data$log_ingm_muj)
summary(data$log_ingm_par)
hist(data$log_ingm_muj, breaks = nrow(data))
hist(data$log_ingm_par, breaks = nrow(data))

### Age difference -------
summary(data$EDAD)
summary(data$eda_par2)
data <- data %>%
  mutate(edad_dif = eda_par2 - EDAD)
summary(data$edad_dif)
head(data[, c("EDAD", "eda_par2", "edad_dif")], n = 35)

### Demean --------
numerical_var_names <- c("num_hij", "ingm_muj", "EDAD", "eda_hij", "eda_sex", "eda_mat", "eda_par2", "hacin",
                         "ingm_par", "POB_TOT", "pres_2020_f", "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m",
                         "phogjef_f", "ParPolF", "ghr20", "mhr20", "fhr20", "MasNoDen", "FemNoDen", "MasPrev",
                         "FemPrev", "cor19", "satis19", "lon", "lat", "log_ingm_muj", "log_ingm_par",
                         "edad_dif")
for(i in c(numerical_var_names)){
  data[, i] <- scale(data[, i], center = TRUE, scale = FALSE)
} 


## Run model ------

### Functional Gradient Descent Boosting ------
modelemoipv <- gamboost(model_1,
                        data = data,
                        control = boost_control(mstop = 2000, nu = 0.5, 
                                                trace = TRUE, 
                                                stopintern = TRUE),
                        weights = data$FAC_MUJ,
                        offset = pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                                     w = data$FAC_MUJ))-0.5,
                        family = Binomial(link = "probit"))

### Cross-Validation ---------
set.seed(1806)
cvemoipv <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), 
                                           type = "subsampling"), 
                   grid = 1:10000, 
                   papply = mclapply,
                   mc.cores = parallel::detectCores())

stopemoipv <- mstop(cvemoipv)
modelemoipv[stopemoipv]

### Stability selection -------
p <- length(names(coef(modelemoipv, which = "")))
stabsel_conf <- stabsel_parameters(p = p, 
                                   q = 20, 
                                   cutoff = 0.8)


### Stability selection with unimodality assumption ------
# Cutoff: 0.8; q: 20; PFER (*):  3.74 
# (*) or expected number of low selection probability variables
# PFER (specified upper bound):  3.743316 
# PFER corresponds to signif. level 0.0425 (without multiplicity adjustment)
stabselemoipv <- stabsel(modelemoipv,
                         q = 20, 
                         cutoff = 0.8,
                         sampling.type = "SS",
                         mc.cores = parallel::detectCores())

### Pointwise bootstrap confidence intervals -------
confintemoipv <- confint(modelemoipv, B = 1000, 
                         level = 0.95, B.mstop = 0,
                         papply = mclapply, 
                         cvrisk_options = list(mc.cores = 25))

#save(confintemoipv, stabselemoipv, modelemoipv, stopemoipv, cvemoipv, file = "estimation_ipv.RData")

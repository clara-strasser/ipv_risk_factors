############################ Main Analysis #############################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

## Change data name -----
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)

## Prepare data ------

### Log transform -----
data <- data %>%
  mutate(log_ingm_muj = log1p(ingm_muj),
         log_ingm_par = log1p(ingm_par))


### Age difference -------
data <- data %>%
  mutate(edad_dif = eda_par2 - EDAD)

### Demean --------
numerical_var_names <- c("num_hij", "ingm_muj", "EDAD", "eda_hij", "eda_sex", "eda_mat", "eda_par2", "hacin",
                         "ingm_par", "POB_TOT", "pres_2020_f", "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m",
                         "phogjef_f", "ParPolF", "ghr20", "mhr20", "fhr20", "MasNoDen", "FemNoDen", "MasPrev",
                         "FemPrev", "cor19", "satis19", "lon", "lat", "log_ingm_muj", "log_ingm_par",
                         "edad_dif")
for(i in c(numerical_var_names)){
  data[, i] <- scale(data[, i], center = TRUE, scale = FALSE)
} 

### Add intercept -------
data <- data %>%
  mutate(intercept = 1)

### Convert outcome ------
data <- data %>% 
  mutate(vio_emo_año = recode(vio_emo_año, "no" = "0", "yes" = "1"))

### Correct levels -----
data$cvegeo <- droplevels(data$cvegeo)


## Run model ------

### Functional Gradient Descent Boosting ------
modelemoipv <- gamboost(model, 
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
start_time <- Sys.time()
cvemoipv <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), 
                                           type = "subsampling"), 
                   grid = 1:10000, 
                   papply = mclapply,
                   mc.cores = parallel::detectCores())

# End measuring execution time
end_time <- Sys.time()
# Calculate the execution time
execution_time <- end_time - start_time
# Print the execution time
print(execution_time)

# Test with grid = 1:3, 20 cores (10 physical)
# Till variable: bbs(edad_dif, by = niv_edmedium, knots = 20, df = 1, center = TRUE)
# Options:
#   1. mc.preschedule = TRUE: 3.625592 mins
#   2. mc.preschedule = FALSE: 3.34062 mins
#   3. lapply: Forever



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

# Save
save(modelemoipv,  file = "../modelemoipv.RData")
#save(confintemoipv, stabselemoipv, modelemoipv, stopemoipv, cvemoipv, file = "estimation_ipv.RData")

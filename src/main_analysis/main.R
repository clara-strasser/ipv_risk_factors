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
set.seed(1806)
modelemoipv <- gamboost(model, # model specification
                        data = data,
                        control = boost_control(mstop = 2000, nu = 0.5, # mstop = number of boosting iterations, nu = shrinkage parameter
                                                trace = TRUE, # trace info during process
                                                stopintern = TRUE),
                        weights = data$FAC_MUJ, # weights for observations in the model
                        offset = pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                                     w = data$FAC_MUJ))-0.5, # weighted mean = 0.2, probability observing a value less or equal to weighted mean = 0.59. Endresult = 0.09.
                                                                             # probability value obtained from the weighted mean. This adjustment centers the distribution of the predictions around 0.
                                                                             # offset used to account for the baseline prevalence of IPV in the population.
                                                                             # it assists in accounting for the base rate of occurrence and can be particularly useful when the data is imbalanced or when specific prior knowledge about the prevalence is available.
                        family = Binomial(link = "probit")) # family and link function for the GAM


# Inspect
# Number of boosting iterations: mstop = 2000 
# Step size:  0.5 
# Offset:  0.09093536 
# Number of baselearners:  95 
# Selection frequencies: 
coef(modelemoipv) 
names(coef(modelemoipv))
summary(modelemoipv)
par(mfrow = c(1,4))
plot(modelemoipv)

### Cross-Validation ---------

# Option 1
set.seed(1806)
start_time <- Sys.time()
cvm <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), type = "kfold"),
              papply = parallel::mclapply,
              mc.cores = parallel::detectCores())
end_time <- Sys.time()
save(cvm,  file = "../cvm.RData")
# Remarks:
#Warning messages:
#1: In papply(1:ncol(folds), function(i) try(dummyfct(weights = folds[,  :
#1 function calls resulted in an error
#2: In cvrisk.mboost(modelemoipv, folds = cv(model.weights(modelemoipv),  :
#1 fold(s) encountered an error. Results are based on 9 folds only.
#Original error message(s):
#Error in chol.default(A) : 
#the leading minor of order 19 is not positive definite

# Option 2
set.seed(1806)
start_time_strat <- Sys.time()
cvm_strat <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), 
                                            type = "kfold",
                                            strata = data$vio_emo_año),
              papply = parallel::mclapply,
              mc.cores = parallel::detectCores())
end_time_strat <- Sys.time()
save(cvm_strat,  file = "../cvm_strat.RData")
# Remarks:
mstop(cvm_strat) # 2000
cvm_strat
plot(cvm_strat)

# Option 3
set.seed(1806)
start_time_strat_2 <- Sys.time()
cvm_strat_grid <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), 
                                            type = "kfold",
                                            strata = data$vio_emo_año),
                    grid = 1:10000,
                    papply = parallel::mclapply,
                    mc.cores = parallel::detectCores())
end_time_strat_2 <- Sys.time()
save(cvm_strat_grid,  file = "../cvm_strat_grid.RData")
mstop(cvm_strat_grid) # 4644
cvm_strat_grid
plot.cvrisk(cvm_strat_grid)

# Option 4
set.seed(1806)
start_time_authors <- Sys.time()
cvemoipv <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), # modelemoipv = model of gamboost(), cv() generated folds for cross-validation
                                                                       # model.weights = influence of each observation in the model
                                           type = "subsampling"), # subsampling = type of cross-validation where data randomly divided into folds
                   grid = 1:10000,  # grid of hyperparameters to be explored
                   papply = mclapply,
                   mc.cores = parallel::detectCores())
end_time_authors <- Sys.time()
save(cvemoipv,  file = "../cvemoipv.RData")
mstop(cvemoipv) # 2282 
plot.cvrisk(cvemoipv)

### Set optimal mstop ------
stopemoipv <- mstop(cvemoipv)
modelemoipv[stopemoipv]


### Stability selection -------
set.seed(1806)
p <- length(names(coef(modelemoipv, which = "")))
stabsel_conf <- stabsel_parameters(p = p, 
                                   q = 20, 
                                   cutoff = 0.8)
stabsel_conf
# Findings:
# PFER (*):  3.47


### Stability selection with unimodality assumption ------
# Cutoff: 0.8; q: 20; PFER (*):  3.74 
# (*) or expected number of low selection probability variables
# PFER (specified upper bound):  3.743316 
# PFER corresponds to signif. level 0.0425 (without multiplicity adjustment)
# Method to extract selected variables
start_time <- Sys.time()
set.seed(1806)
stabselemoipv_3 <- stabsel(modelemoipv,
                         q = 20, 
                         cutoff = 0.8,
                         sampling.type = "SS")
end_time<- Sys.time()
save(stabselemoipv_3,  file = "../stabselemoipv_3.RData")

### Pointwise bootstrap confidence intervals -------
confintemoipv <- confint(modelemoipv, B = 1000, 
                         level = 0.95, B.mstop = 0,
                         papply = mclapply, 
                         cvrisk_options = list(mc.cores = 25))

# Save
save(modelemoipv,  file = "../modelemoipv.RData")
save(cvm,  file = "../cvm.RData")
#save(confintemoipv, stabselemoipv, modelemoipv, stopemoipv, cvemoipv, file = "estimation_ipv.RData")

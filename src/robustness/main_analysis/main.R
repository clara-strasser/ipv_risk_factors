############################ Main Analysis #############################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)
library(stabs)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1_rob.RData")) # main data

## Change data name -----
data <- data_imp_pmm_m1_rob
rm(data_imp_pmm_m1_rob)

## Set working directory ----
setwd("~/model3_imputed_rob")

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
data <- data %>%
  mutate_at(vars(all_of(numerical_var_names)), as.numeric)
for(i in c(numerical_var_names)){
  data[, i] <- scale(data[, i], center = TRUE, scale = FALSE)
} 

### Add intercept -------
data <- data %>%
  mutate(intercept = 1)

### Convert outcome ------
data <- data %>% 
  mutate(vio_emo_año = recode(vio_emo_año, "no" = "0", "yes" = "1")) %>%
  mutate(control_año = recode(control_año, "no" = "0", "yes" = "1")) %>%
  mutate(surv_año = recode(surv_año, "no" = "0", "yes" = "1")) %>%
  mutate(emo_abuse_año = recode(emo_abuse_año, "no" = "0", "yes" = "1")) 

### Correct levels -----
data$cvegeo <- droplevels(data$cvegeo)

## Run model ------

### Functional Gradient Descent Boosting ------
set.seed(1806)

#### Control ----
model_control <- gamboost(model_out1, # model specification with outcome variable "control_año"
                        data = data,
                        control = boost_control(mstop = 2000, nu = 0.5, # mstop = number of boosting iterations, nu = shrinkage parameter
                                                trace = TRUE, # trace info during process
                                                stopintern = TRUE),
                        weights = data$FAC_MUJ, # weights for observations in the model
                        offset = pnorm(weighted.mean(x = as.numeric(as.character(data[, "control_año"])),
                                                     w = data$FAC_MUJ))-0.5, # weighted mean = 0.2, probability observing a value less or equal to weighted mean = 0.59. Endresult = 0.09.
                        # probability value obtained from the weighted mean. This adjustment centers the distribution of the predictions around 0.
                        # offset used to account for the baseline prevalence of IPV in the population.
                        # it assists in accounting for the base rate of occurrence and can be particularly useful when the data is imbalanced or when specific prior knowledge about the prevalence is available.
                        family = Binomial(link = "probit")) # family and link function for the GAM
save(model_control,  file = "model_control.RData")

#### Surveillance ----
model_surv <- gamboost(model_out2, # model specification with outcome variable "surv_año"
                          data = data,
                          control = boost_control(mstop = 2000, nu = 0.5, # mstop = number of boosting iterations, nu = shrinkage parameter
                                                  trace = TRUE, # trace info during process
                                                  stopintern = TRUE),
                          weights = data$FAC_MUJ, # weights for observations in the model
                          offset = pnorm(weighted.mean(x = as.numeric(as.character(data[, "surv_año"])),
                                                       w = data$FAC_MUJ))-0.5, # weighted mean = 0.2, probability observing a value less or equal to weighted mean = 0.59. Endresult = 0.09.
                          # probability value obtained from the weighted mean. This adjustment centers the distribution of the predictions around 0.
                          # offset used to account for the baseline prevalence of IPV in the population.
                          # it assists in accounting for the base rate of occurrence and can be particularly useful when the data is imbalanced or when specific prior knowledge about the prevalence is available.
                          family = Binomial(link = "probit")) # family and link function for the GAM
save(model_surv,  file = "model_surv.RData")

#### Emotional Abuse ----
model_emo_abuse <- gamboost(model_out3, # model specification with outcome variable "emo_abuse_año"
                       data = data,
                       control = boost_control(mstop = 2000, nu = 0.5, # mstop = number of boosting iterations, nu = shrinkage parameter
                                               trace = TRUE, # trace info during process
                                               stopintern = TRUE),
                       weights = data$FAC_MUJ, # weights for observations in the model
                       offset = pnorm(weighted.mean(x = as.numeric(as.character(data[, "emo_abuse_año"])),
                                                    w = data$FAC_MUJ))-0.5, # weighted mean = 0.2, probability observing a value less or equal to weighted mean = 0.59. Endresult = 0.09.
                       # probability value obtained from the weighted mean. This adjustment centers the distribution of the predictions around 0.
                       # offset used to account for the baseline prevalence of IPV in the population.
                       # it assists in accounting for the base rate of occurrence and can be particularly useful when the data is imbalanced or when specific prior knowledge about the prevalence is available.
                       family = Binomial(link = "probit")) # family and link function for the GAM
save(model_emo_abuse,  file = "model_emo_abuse.RData")


### Test Boosting Results ----

#### Control -----
coef(model_control) 
summary(model_control)

#### Surveillance -----
coef(model_surv) 
summary(model_surv)

#### Emotional Abuse -----
coef(model_emo_abuse) 
summary(model_emo_abuse)

### Subsampling ---------
set.seed(1806)

#### Control -----
cv_control <- cvrisk(model_control, folds = cv(model.weights(model_control), 
                                           type = "subsampling"), 
                   grid = 1:10000, 
                   papply = mclapply,
                   mc.cores = parallel::detectCores())
save(cv_control,  file = "cv_control.RData")

#### Surveillance -----
cv_surv <- cvrisk(model_surv, folds = cv(model.weights(model_surv), 
                                               type = "subsampling"), 
                     grid = 1:10000, 
                     papply = mclapply,
                     mc.cores = parallel::detectCores())
save(cv_surv,  file = "cv_surv.RData")

#### Emotional Abuse -----
cv_emo_abuse <- cvrisk(model_emo_abuse, folds = cv(model.weights(model_emo_abuse), 
                                               type = "subsampling"), 
                     grid = 1:10000, 
                     papply = mclapply,
                     mc.cores = parallel::detectCores())
save(cv_emo_abuse,  file = "cv_emo_abuse.RData")

### Set mstop -----

#### Control ----
mstop(cv_control) # 3473
plot.cvrisk(cv_control)

# Set
stop1 <- mstop(cv_control)
model_control[stop1]

#### Surveillance ----
mstop(cv_surv) # 3098
plot.cvrisk(cv_surv)

# Set
stop2 <- mstop(cv_surv)
model_surv[stop2]

#### Emotional Abuse ----
mstop(cv_emo_abuse) # 1572
plot.cvrisk(cv_emo_abuse)

# Set
stop3 <- mstop(cv_emo_abuse)
model_emo_abuse[stop3]


### Stability selection -------
set.seed(1806)

#### Control ----
stabsel_control <- stabsel(model_control,
                         q = 20, 
                         cutoff = 0.8,
                         sampling.type = "SS",
                         mc.cores = parallel::detectCores())
save(stabsel_control,  file = "stabsel_control.RData")

#### Surveillance ----
stabsel_surv <- stabsel(model_surv,
                           q = 20, 
                           cutoff = 0.8,
                           sampling.type = "SS",
                           mc.cores = parallel::detectCores())
save(stabsel_surv,  file = "stabsel_surv.RData")

#### Emotional Abuse ----
stabsel_emo_abuse <- stabsel(model_emo_abuse,
                        q = 20, 
                        cutoff = 0.8,
                        sampling.type = "SS",
                        mc.cores = parallel::detectCores())
save(stabsel_emo_abuse,  file = "stabsel_emo_abuse.RData")




### Pointwise bootstrap confidence intervals -------
confintemoipv <- confint(modelemoipv, B = 1000, 
                         level = 0.95, B.mstop = 0,
                         papply = mclapply, 
                         cvrisk_options = list(mc.cores = 25))

# Save
save(modelemoipv,  file = "../modelemoipv.RData")
save(cvm,  file = "../cvm.RData")
#save(confintemoipv, stabselemoipv, modelemoipv, stopemoipv, cvemoipv, file = "estimation_ipv.RData")

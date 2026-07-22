############################ Main Analysis #############################
# See also: Torres Munguía & Martínez-Zarzoso (2022)

# Renv install


## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)
library(stabs)
library(RhpcBLASctl)

## Pin BLAS to a single thread -------------------------------------------------
# This container ships multi-threaded OpenBLAS, which defaults to one thread per
# core (20 here). mboost's base-learners are many tiny matrix ops, so the thread
# sync overhead costs far more than it saves: measured 0.636 s/iter with 20 BLAS
# threads vs 0.170 s/iter pinned to 1 -- a 3.7x slowdown. It gets far worse once
# mclapply forks 25 folds, since each fork opens its own 20 BLAS threads
# (25 x 20 = 500 threads over 20 real cores), which is what made cvrisk look hung.
# NOTE: setting Sys.setenv(OPENBLAS_NUM_THREADS) here does NOT work -- OpenBLAS
# reads that only at load time. This call changes it at runtime, so it takes
# effect in an already-running session.
blas_set_num_threads(1)
omp_set_num_threads(1)

## Set path --------------------------------------------------------------------
path_data <- "/dss/dsshome1/0B/ru23kek2/data/final_data/rob_no_outliers"
path_save <- "/dss/dsshome1/0B/ru23kek2/data/final_data/rob_no_outliers"   # <- results are written here; adjust if needed
# (must end in "/", it is pasted directly onto filenames)

## Load data -------------------------------------------------------------------
load(paste0(path_data, "/data_imp_pmm_no_outliers.RData")) # main data
load("~/model_pmm_alt.RData")

## Change data name ------------------------------------------------------------
data <- data_imp_pmm_no_outliers

## Prepare data ----------------------------------------------------------------

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

### Add intercept -------
data <- data %>%
  mutate(intercept = 1)

### Convert outcome ------
data <- data %>% 
  mutate(vio_emo_año = recode(vio_emo_año, "no" = "0", "yes" = "1"))

### Correct levels -----
data$cvegeo <- droplevels(data$cvegeo)


## Run model -------------------------------------------------------------------

# REQUIREMENT:
# RUN formula first!

### Functional Gradient Descent Boosting ------
set.seed(1806)
modelemoipv <- gamboost(model, # model specification
                        data = data,
                        control = boost_control(mstop = 2000, nu = 0.5,
                                                trace = TRUE,
                                                stopintern = TRUE),
                        weights = data$FAC_MUJ,
                        offset = pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                                     w = data$FAC_MUJ))-0.5,
                        family = Binomial(link = "probit"))
save(modelemoipv,  file = paste0(path_save, "/model4.RData"))

# Inspect
# Number of boosting iterations: mstop = 2000 
# Step size:  0.5 
# Offset:  0.09093536 
# Number of baselearners:  95 
# Selection frequencies: 
coef(modelemoipv) 
names(coef(modelemoipv))
summary(modelemoipv)

### Resampling Method ----------------------------------------------------------

# Subsampling
set.seed(1806)
folds <- cv(model.weights(modelemoipv), type = "subsampling")   # B = 25 folds (mboost default)

# Silence the progress trace: 25 forks all writing dots to one console interleave
# into garbage and add pointless I/O contention.
modelemoipv$control$trace <- FALSE

# mc.cores: do NOT use parallel::detectCores(). It reports 96 here (all cores on
# the physical node), but this session is only allocated 20 (see `nproc` /
# taskset affinity). Forking 96 workers onto 20 cores just thrashes.
# We fork exactly one worker per fold: 25 workers over 20 cores finishes in
# ~1.25 "waves". That beats capping at 20, which would need 2 full waves
# (20 folds, then a straggler wave of 5) and take ~1.6x longer.
n_folds <- ncol(folds)

# Runtime at 0.17 s/iter (measured, BLAS-pinned, n = 61,402, 94 baselearners):
#   cvrisk refits every fold from scratch up to max(grid) -- `grid` only selects
#   which mstop values get reported, it does NOT reduce computation.
#   grid = 1:10000 -> ~28 min per fold  -> ~35 min wall
#   grid = 1:3000  -> ~8.5 min per fold -> ~11 min wall
start_time <- Sys.time()
cvemoipv <- cvrisk(modelemoipv, folds = folds,
                   grid = 1:3000,
                   papply = mclapply,
                   mc.cores = n_folds)
end_time <- Sys.time()
print(end_time - start_time)

save(cvemoipv,  file = paste0(path_save,"/cv4.RData"))
mstop(cvemoipv)
plot(cvemoipv)

### Optimal mstop --------------------------------------------------------------
stopemoipv <- mstop(cvemoipv)
modelemoipv[stopemoipv]


### Stability selection --------------------------------------------------------
set.seed(1806)
p <- length(names(coef(modelemoipv, which = "")))
stabsel_conf <- stabsel_parameters(p = p, 
                                   q = 20, 
                                   cutoff = 0.8)
stabsel_conf

# Cutoff: 0.8; q: 20; PFER (*):  3.74 
# (*) or expected number of low selection probability variables
# PFER (specified upper bound):  3.743316 
# PFER corresponds to signif. level 0.0425 (without multiplicity adjustment)
# Method to extract selected variables
start_time <- Sys.time()
set.seed(1806)
stabselemoipv <- stabsel(modelemoipv,
                         q = 20,
                         cutoff = 0.8,
                         sampling.type = "SS",
                         mc.cores = 20)   # real allocated cores, not detectCores() = 96
end_time<- Sys.time()
save(stabselemoipv,  file = paste0(path_save,"/stabsel4.RData"))

### Pointwise bootstrap confidence intervals -----------------------------------
# WARNING on cost: B = 1000 means 1000 full refits. At ~0.17 s/iter and mstop
# 2000 that is ~5.7 min each -> ~1000 * 5.7 / 20 cores = ~4.5 h wall.
# Drop B (e.g. 200 -> ~1 h) if you need this to finish sooner.
start_time <- Sys.time()
confintemoipv <- confint(modelemoipv,  B = 1000,
                         level = 0.95, B.mstop = 0,
                         papply = mclapply,
                         mc.cores = 20,   # real allocated cores, not detectCores() = 96
                         cvrisk_options = list(mc.cores = 20))
end_time<- Sys.time()
save(confintemoipv,  file = paste0(path_save,"confintemoipv1.RData"))


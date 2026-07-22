################################## GLM Analysis ################################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mgcv)
library(margins)
library(stats)
if (!requireNamespace("survey", quietly = TRUE)) install.packages("survey", type = "binary")
library(survey)
library(xtable)
library(stargazer)

## Set path --------------------------------------------------------------------
path_data <- "/dss/dsshome1/0B/ru23kek2/data/final_data/"
path_save <- "/dss/dsshome1/0B/ru23kek2/data/final_data/"

## Load data -------------------------------------------------------------------
load(paste0(path_data, "data_imp_pmm_m1.RData")) # main data

## Change data name ------------------------------------------------------------
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)

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

## Run model -------------------------------------------------------------------

### GLM with linear effects ----------------------------------------------------
offset_var <- rep(pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                      w = data$FAC_MUJ))-0.5, nrow(data))

dstrat<-svydesign(id=~1,weights=~data$FAC_MUJ, data=data)
model_glm <- svyglm(vio_emo_año ~ desempleo + vio_inf + vio_exp_inf + 
               vio_sex_inf + con_sex*eda_sex + mot_mat + vio_inf_par + 
               vio_exp_inf_par + act_distboth + act_distmales + lib_sex_gradmedium +
               lib_eco_gradmedium + redsoc_gradhigh + rout_gradmedium + MasNoDen, 
              design = dstrat,
             family = binomial(link = "probit"), 
             offset = offset_var,
             data = data)
summary(model_glm)

margins_glm <- summary(margins(model_glm, design = dstrat))
print(margins_glm)

### GAM with linear effects ----------------------------------------------------
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

### AME and 95% CI for the GAM --------------------------------------------------

# margins / marginaleffects do not support mgcv::gam objects, so the average
# marginal effects of the parametric terms are computed directly:
#   AME_j = mean_w(phi(eta)) * beta_j
# with phi() the standard normal density and eta the linear predictor
# (offset included). The standard error follows from the delta method, where
# the gradient of AME_j w.r.t. the full coefficient vector is
#   grad = mean_w(phi'(eta) * X) * beta_j + mean_w(phi(eta)) * e_j
# and phi'(z) = -z * phi(z). Weights are the survey weights (FAC_MUJ), so the
# AME refers to the population rather than the sample.
# Smooth / spatial / random terms are excluded: their effect is nonlinear and
# not summarised by a single number.

ame_gam <- function(model, level = 0.95) {
  X    <- predict(model, type = "lpmatrix")   # n x p design matrix
  beta <- coef(model)
  V    <- vcov(model)
  eta  <- model$linear.predictors             # includes the offset
  w    <- model$prior.weights
  w    <- w / sum(w)

  phi       <- dnorm(eta)
  phi_prime <- -eta * phi
  scale_factor <- sum(w * phi)

  # parametric coefficients come first in mgcv; drop the intercept
  para_idx <- seq_len(model$nsdf)
  para_idx <- para_idx[names(beta)[para_idx] != "(Intercept)"]

  base_grad <- as.vector(crossprod(X, w * phi_prime))  # mean_w(phi'(eta) * X)

  est <- se <- numeric(length(para_idx))
  for (k in seq_along(para_idx)) {
    j <- para_idx[k]
    est[k] <- scale_factor * beta[j]

    grad     <- base_grad * beta[j]
    grad[j]  <- grad[j] + scale_factor
    se[k]    <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  }

  z <- qnorm(1 - (1 - level) / 2)
  data.frame(
    factor    = names(beta)[para_idx],
    AME       = est,
    SE        = se,
    z         = est / se,
    p         = 2 * pnorm(-abs(est / se)),
    lower     = est - z * se,
    upper     = est + z * se,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

margins_gam <- ame_gam(model_gam)
print(margins_gam, digits = 4)

# NOTE: for variables entering an interaction (edad_dif x niv_edmedium,
# con_sex x eda_sex, log_ingm_muj x log_ingm_par) the value above is the effect
# holding the interaction partner at its (demeaned) sample value of 0, not the
# total marginal effect.

### Save Effects ----------------------------------------------------------------

# Model 1
xtable(model_glm)
xtable(margins_glm)


# Model 2
xtable(model_gam)
xtable(margins_gam)






######################## Analysis - Linear Regression #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mgcv)
library(margins)

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
model <- gam(vio_emo_año ~ eda_sex + desempleo + vio_inf + vio_exp_inf + 
               vio_sex_inf + con_sex + mot_mat + vio_inf_par + 
               vio_exp_inf_par + act_distboth + act_distmales + lib_sex_gradmedium +
             lib_eco_gradmedium + redsoc_gradhigh + rout_gradmedium + MasNoDen, 
             family = binomial(link = "probit"), 
             weights = data$FAC_MUJ,
             offset = offset_var,
             data = data)
summary(model)

### Plot effects
plot(model, all.terms = TRUE)







plot(x = data$EDAD, 
     y = data$vio_emo_año,
     main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "Eda Sex",
     ylab = "VIO",
     pch = 20,
     ylim = c(0, 0.1),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(model, list(EDAD = x), type = "response")

lines(x, y, lwd = 1.5, col = "steelblue")

data$EDAD <- data$EDAD + 40.59427




model <- glm(vio_emo_año ~ EDAD + eda_sex:con_sex + mot_mat:eda_mat + 
               lib_eco_gradmedium + redsoc_gradmedium + act_distmales +
               pea_f + MasPrev,
             family = binomial(link = "probit"),
             data = data)


, 
             weights = data$FAC_MUJ,
             offset = offset_var_authors,
             data = data)
plot(model, all.terms = TRUE)
marg <-margins(model)
model







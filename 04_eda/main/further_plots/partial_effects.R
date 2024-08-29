############################ Partial Effects ################################


## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(mboost)
library(data.table)

## Set path --------------------------------------------------------------------
path_data <- "/Users/clarastrasser/ipv_data/data/final_data/"
path_model <- "/Users/clarastrasser/ipv_data/results/main/"

## Load data -------------------------------------------------------------------
load(paste0(path_data,"data_imp_pmm_m1.RData"))
load(paste0(path_model,"model.RData"))
load(paste0(path_model,"cv.RData"))  

## Change data name-------------------------------------------------------------
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)

## Prepare data ----------------------------------------------------------------

### Log transform --------------------------------------------------------------
data <- data %>%
  mutate(log_ingm_muj = log1p(ingm_muj),
         log_ingm_par = log1p(ingm_par))


### Age difference -------------------------------------------------------------
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

### Add intercept --------------------------------------------------------------
data <- data %>%
  mutate(intercept = 1)

### Convert outcome ------------------------------------------------------------
data <- data %>% 
  mutate(vio_emo_año = recode(vio_emo_año, "no" = "0", "yes" = "1"))

### Correct levels -------------------------------------------------------------
data$cvegeo <- droplevels(data$cvegeo)

## Load function ---------------------------------------------------------------
source("src/partial_effects.R")

## Prep data -------------------------------------------------------------------

# Set optimal mstop
stopemoipv <- mstop(cvemoipv)
modelemoipv[stopemoipv]

# Check
summary(modelemoipv)
names(coef(modelemoipv)) # 54 variables were selected


# Plot -----

## Continuous variables ------
# Plot the linear and smooth effects for the continuous variables 

# Set variable names
con_var <- c(
  "eda_sex",
  "edad_dif",
  "log_ingm_muj",
  "log_ingm_par",
  "eda_mat",
  "FemPrev",
  "num_hij",
  "phogjef_f",
  "gini20",
  "pea_f",
  "hacin",
  "MasNoDen",
  "pres_2020_f",
  "mhr20",
  "cor19"
)

# Generate plots
plots <- partial_effects(modelemoipv, data, con_var)

# Specify the directory to save the plots
save_directory <- "~/ipv_risk_factors/results/partial_effects_plots/"

# Save the plots
for (variable in names(plots)) {
  # Get the list of plot files for the current variable
  plot_files <- plots[[variable]]
  
  # Save each plot file
  for (i in seq_along(plot_files)) {
    plot_file <- plot_files[[i]]
    ggsave(plot = plot_file, filename = paste0(variable, "_plot", i, ".png"), path = save_directory) # Save the plot file by copying it
  }
}


## Continuous variables by categorical ------
# Plot the partial effects for the continuous variables by factor
int_var <- c("eda_sex",
             "con_sex")

# generate df
bols_data <- as.data.frame(data[, c("eda_sex", "con_sex")])
colnames(bols_data)[1] <- "eda_sex"
colnames(bols_data)[2] <- "con_sex"
bols_data <- bols_data[bols_data$con_sex == "yes", ]
bols_data$y <- predict(modelemoipv, which = "bols(eda_sex, by = con_sex, intercept = FALSE)", type = "link", newdata = bols_data)

# plot
plot_int <- ggplot(bols_data, aes(x = eda_sex, y = y)) +
  geom_line(alpha = 0.5, color = "#002b58") +
  geom_rug(aes(y = y), sides = "b", position = "jitter") +
  labs(x = "", y = paste0("f(","eda_sex:con_sexyes", ")"), title = "eda_sex:con_sexyes") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1, 1),
        legend.box.just = "right")
plot_int

# Specify the directory to save the plots
save_directory <- "~/ipv_risk_factors/results/partial_effects_plots/"

# Save the plots
ggsave(plot = plot_int, filename = "eda_sex_and_con_sex.png", path = save_directory) # Save the plot file by copying it

## Categorical variables -----
# Plot the categorical effects for the factor variables

# Set variable names
cat_var <- c(
  "lib_eco_gradhigh",
  "lib_soc_gradhigh",
  "lib_eco_gradmedium",
  "lib_soc_gradmedium",
  "con_sex",
  "vio_inf",
  "vio_exp_inf_par",
  "vio_exp_inf",
  "niv_edmedium",
  "vio_inf_par",
  "niv_edhigh",
  "cct_rec",
  "vio_sex_inf",
  "mot_mat",
  "act_distmales",
  "rout_gradhigh",
  "act_distboth",
  "feminist_gradmedium",
  "extr_inf",
  "lib_sex_gradhigh",
  "desempleo",
  "redsoc_gradhigh",
  "rout_gradmedium",
  "Marg20low",
  "Marg20high",
  "Marg20very_high",
  "indigena",
  "Type_comlow_urban"
)

# Generate plots
plots <- partial_effects_categorical(modelemoipv, data, cat_var)


# Save plots
for (variable in names(plots)) {
  # Get the list of plot files for the current variable
  plot_files <- plots[[variable]]
  
  # Save each plot file
  for (i in seq_along(plot_files)) {
    plot_file <- plot_files[[i]]
    ggsave(plot = plot_file, filename = paste0(variable, "_plot", i, ".png"), path = save_directory) # Save the plot file by copying it
  }
}















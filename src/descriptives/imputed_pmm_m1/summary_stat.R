############################ Summary Statistics ################################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(xtable)
library(psych)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/imputed_pmm_m1/corrplot/"

## Load data -------
load(paste0(path_data, "step3_endireh.RData"))

## Change data name -----
data <- step3_endireh

## Subset not needed -----
data <- data %>%
  select(-c("num_hij_par", "num_hij_par_muj"))

## Summary Stat --------
summary(data)

## Create vectors -----
individual_num <- c("EDAD","ingm_muj","num_hij", "eda_hij", "eda_sex", "eda_mat")
individual_cat <- c("niv_edlow", "niv_edmedium", "niv_edhigh", "indigena", "cct_rec", "empleo_vida", "empleo_5_años", "desempleo", "extr_inf", "pareja_prev", "vio_inf", "vio_exp_inf", "vio_sex_inf", "con_sex", "mot_mat", "feminist_gradlow", "feminist_gradmedium", "feminist_gradhigh")

relationship_num <- c("eda_par2", "ingm_par", "hacin")
relationship_cat <- c("vio_inf_par", "vio_exp_inf_par", "act_distmales", "act_distfemales", "act_distboth", "lib_sex_gradlow", "lib_sex_gradmedium", "lib_sex_gradhigh", "lib_eco_gradlow", "lib_eco_gradmedium", "lib_eco_gradhigh", "lib_soc_gradlow", "lib_soc_gradmedium", "lib_soc_gradhigh", "redsoc_gradlow", "redsoc_gradmedium", "redsoc_gradhigh", "rout_gradlow", "rout_gradmedium", "rout_gradhigh")

community_num <- c("mhr20", "fhr20", "ghr20", "phogjef_f", "pres_2020_f", "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m", "ParPolF")
community_cat <- c("Marg15very_low", "Marg15low", "Marg15medium", "Marg15high", "Marg15very_high", "Type_comrural", "Type_comlow_urban", "Type_commedium_urban", "Type_comhigh_urban")

society_num <- c("MasPrev", "FemPrev", "MasNoDen", "FemNoDen", "cor19", "satis19")


# Convert to numerical
data[individual_num] <- lapply(data[individual_num], as.numeric)
data[community_num] <- lapply(data[community_num], as.numeric)
data[society_num] <- lapply(data[society_num], as.numeric)


# Summary Stat: Continuous Variables ------

## Individual Risk Factors ------
summary_stats_individual <- data.frame(
  SD = apply(data[individual_num], 2, sd),
  Min. = apply(data[individual_num], 2, min),
  Q1 = apply(data[individual_num], 2, quantile, probs = 0.25),
  Median = apply(data[individual_num], 2, median),
  Mean = apply(data[individual_num], 2, mean),
  Q3 = apply(data[individual_num], 2, quantile, probs = 0.75),
  Max. = apply(data[individual_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_ind <- xtable::xtable(summary_stats_individual, caption = "Summary Statistics", label = "tab:summary")

# Print
print(xtable(latex_table_ind), include.rownames=FALSE)



## Relationship Risk Factors ------
summary_stats_relationship <- data.frame(
  SD = apply(data[relationship_num], 2, sd),
  Min. = apply(data[relationship_num], 2, min),
  Q1 = apply(data[relationship_num], 2, quantile, probs = 0.25),
  Median = apply(data[relationship_num], 2, median),
  Mean = apply(data[relationship_num], 2, mean),
  Q3 = apply(data[relationship_num], 2, quantile, probs = 0.75),
  Max. = apply(data[relationship_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_relationship <- xtable::xtable(summary_stats_relationship, caption = "Summary Statistics for Relationship-Level Risk Factors", label = "tab:summary_relationship")

# Print
print(xtable(latex_table_relationship), include.rownames=FALSE)



## Community Risk Factors ------
summary_stats_community <- data.frame(
  SD = apply(data[community_num], 2, sd),
  Min. = apply(data[community_num], 2, min),
  Q1 = apply(data[community_num], 2, quantile, probs = 0.25),
  Median = apply(data[community_num], 2, median),
  Mean = apply(data[community_num], 2, mean),
  Q3 = apply(data[community_num], 2, quantile, probs = 0.75),
  Max. = apply(data[community_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_community <- xtable::xtable(summary_stats_community, caption = "Summary Statistics for Community-Level Risk Factors", label = "tab:summary_community")

# Print
print(xtable(latex_table_community), include.rownames=FALSE)



## Society Risk Factors ------
summary_stats_society <- data.frame(
  SD = apply(data[society_num], 2, sd),
  Min. = apply(data[society_num], 2, min),
  Q1 = apply(data[society_num], 2, quantile, probs = 0.25),
  Median = apply(data[society_num], 2, median),
  Mean = apply(data[society_num], 2, mean),
  Q3 = apply(data[society_num], 2, quantile, probs = 0.75),
  Max. = apply(data[society_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_society <- xtable::xtable(summary_stats_society, caption = "Summary Statistics for Society-Level Risk Factors", label = "tab:summary_society")

# Print
print(xtable(latex_table_society), include.rownames=FALSE)



# Summary Stat: Categorical Variables ------


## Individual Risk Factors ------
summary_stats_individual <- data.frame(
  Variable = character(0),
  Category = character(0),
  Frequency = numeric(0),
  Percentage = numeric(0),
  stringsAsFactors = FALSE
)

for (var in individual_cat) {
  var_counts <- table(data[[var]])
  var_perc <- var_counts / sum(var_counts) * 100
  
  var_summary <- data.frame(
    Variable = rep(var, length(var_counts)),
    Category = names(var_counts),
    Frequency = as.numeric(var_counts),
    Percentage = as.numeric(var_perc),
    stringsAsFactors = FALSE
  )
  
  summary_stats_individual <- rbind(summary_stats_individual, var_summary)
}

# Convert DataFrame to LaTeX table
latex_table_individual <- xtable::xtable(summary_stats_individual)

# Print
print(xtable(latex_table_individual), include.rownames=FALSE)



## Relationship Risk Factors ------
summary_stats_relationship <- data.frame(
  Variable = character(0),
  Category = character(0),
  Frequency = numeric(0),
  Percentage = numeric(0),
  stringsAsFactors = FALSE
)

for (var in relationship_cat) {
  var_counts <- table(data[[var]])
  var_perc <- var_counts / sum(var_counts) * 100
  
  var_summary <- data.frame(
    Variable = rep(var, length(var_counts)),
    Category = names(var_counts),
    Frequency = as.numeric(var_counts),
    Percentage = as.numeric(var_perc),
    stringsAsFactors = FALSE
  )
  
  summary_stats_relationship <- rbind(summary_stats_relationship, var_summary)
}

# Convert DataFrame to LaTeX table
latex_table_relationship <- xtable::xtable(summary_stats_relationship, caption = "Summary Statistics for Relationship-Level Risk Factors", label = "tab:summary_relationship")

# Print
print(xtable(latex_table_relationship), include.rownames=FALSE)



## Community Risk Factors ------
summary_stats_community <- data.frame(
  Variable = character(0),
  Category = character(0),
  Frequency = numeric(0),
  Percentage = numeric(0),
  stringsAsFactors = FALSE
)

for (var in community_cat) {
  var_counts <- table(data[[var]])
  var_perc <- var_counts / sum(var_counts) * 100
  
  var_summary <- data.frame(
    Variable = rep(var, length(var_counts)),
    Category = names(var_counts),
    Frequency = as.numeric(var_counts),
    Percentage = as.numeric(var_perc),
    stringsAsFactors = FALSE
  )
  
  summary_stats_community <- rbind(summary_stats_community, var_summary)
}

# Convert DataFrame to LaTeX table
latex_table_community <- xtable::xtable(summary_stats_community, caption = "Summary Statistics for Community-Level Risk Factors", label = "tab:summary_community")

# Print
print(xtable(latex_table_community), include.rownames=FALSE)



























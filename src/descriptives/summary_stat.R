############################ Summary Statistics ################################

#############################  Prepare Data ##################################

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
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/"


## Load data -------
load(paste0(path, "emo_ipv_final.RData")) # main data set

## Summary Stat --------
summary(emo_ipv_final)

## Create vectors -----
individual_num <- c("EDAD","ingm_muj","num_hij", "eda_hij", "eda_sex", "eda_mat")
individual_cat <- c("niv_edlow", "niv_edmedium", "niv_edhigh", "indigena", "cct_rec", "empleo_vida", "empleo_5_años", "desempleo", "extr_inf", "pareja_prev", "vio_inf", "vio_exp_inf", "vio_sex_inf", "con_sex", "mot_mat", "feminist_gradlow", "feminist_gradmedium", "feminist_gradhigh")

relationship_num <- c("eda_par2", "ingm_par", "hacin")
relationship_cat <- c("vio_inf_par", "vio_exp_inf_par", "act_distmales", "act_distfemales", "act_distboth", "lib_sex_gradlow", "lib_sex_gradmedium", "lib_sex_gradhigh", "lib_eco_gradlow", "lib_eco_gradmedium", "lib_eco_gradhigh", "lib_soc_gradlow", "lib_soc_gradmedium", "lib_soc_gradhigh", "redsoc_gradlow", "redsoc_gradmedium", "redsoc_gradhigh", "rout_gradlow", "rout_gradmedium", "rout_gradhigh")

community_num <- c("mhr20", "fhr20", "ghr20", "phogjef_f", "pres_2020_f", "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m", "ParPolF")
community_cat <- c("Marg15very_low", "Marg15low", "Marg15medium", "Marg15high", "Marg15very_high", "type_comrural", "type_comlow_urban", "type_commedium_urban", "type_comhigh_urban")

society_num <- c("MasPrev", "FemPrev", "MasNoDen", "FemNoDen", "cor19", "satis19")


# Convert to numerical
emo_ipv_final[individual_num] <- lapply(emo_ipv_final[individual_num], as.numeric)
emo_ipv_final[community_num] <- lapply(emo_ipv_final[community_num], as.numeric)
emo_ipv_final[society_num] <- lapply(emo_ipv_final[society_num], as.numeric)


# Summary Stat: Continuous Variables ------

## Individual Risk Factors ------
summary_stats_individual <- data.frame(
  SD = apply(emo_ipv_final[individual_num], 2, sd),
  Min. = apply(emo_ipv_final[individual_num], 2, min),
  Q1 = apply(emo_ipv_final[individual_num], 2, quantile, probs = 0.25),
  Median = apply(emo_ipv_final[individual_num], 2, median),
  Mean = apply(emo_ipv_final[individual_num], 2, mean),
  Q3 = apply(emo_ipv_final[individual_num], 2, quantile, probs = 0.75),
  Max. = apply(emo_ipv_final[individual_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_ind <- xtable::xtable(summary_stats_individual, caption = "Summary Statistics", label = "tab:summary")

# Print
print(latex_table_ind)


## Relationship Risk Factors ------
summary_stats_relationship <- data.frame(
  SD = apply(emo_ipv_final[relationship_num], 2, sd),
  Min. = apply(emo_ipv_final[relationship_num], 2, min),
  Q1 = apply(emo_ipv_final[relationship_num], 2, quantile, probs = 0.25),
  Median = apply(emo_ipv_final[relationship_num], 2, median),
  Mean = apply(emo_ipv_final[relationship_num], 2, mean),
  Q3 = apply(emo_ipv_final[relationship_num], 2, quantile, probs = 0.75),
  Max. = apply(emo_ipv_final[relationship_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_relationship <- xtable::xtable(summary_stats_relationship, caption = "Summary Statistics for Relationship-Level Risk Factors", label = "tab:summary_relationship")

# Print
print(latex_table_relationship)


## Community Risk Factors ------
summary_stats_community <- data.frame(
  SD = apply(emo_ipv_final[community_num], 2, sd),
  Min. = apply(emo_ipv_final[community_num], 2, min),
  Q1 = apply(emo_ipv_final[community_num], 2, quantile, probs = 0.25),
  Median = apply(emo_ipv_final[community_num], 2, median),
  Mean = apply(emo_ipv_final[community_num], 2, mean),
  Q3 = apply(emo_ipv_final[community_num], 2, quantile, probs = 0.75),
  Max. = apply(emo_ipv_final[community_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_community <- xtable::xtable(summary_stats_community, caption = "Summary Statistics for Community-Level Risk Factors", label = "tab:summary_community")

# Print
print(latex_table_community)


## Society Risk Factors ------
summary_stats_society <- data.frame(
  SD = apply(emo_ipv_final[society_num], 2, sd),
  Min. = apply(emo_ipv_final[society_num], 2, min),
  Q1 = apply(emo_ipv_final[society_num], 2, quantile, probs = 0.25),
  Median = apply(emo_ipv_final[society_num], 2, median),
  Mean = apply(emo_ipv_final[society_num], 2, mean),
  Q3 = apply(emo_ipv_final[society_num], 2, quantile, probs = 0.75),
  Max. = apply(emo_ipv_final[society_num], 2, max)
)

# Convert DataFrame to LaTeX table
latex_table_society <- xtable::xtable(summary_stats_society, caption = "Summary Statistics for Society-Level Risk Factors", label = "tab:summary_society")

# Print
print(latex_table_society)


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
  var_counts <- table(emo_ipv_final[[var]])
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
print(latex_table_individual)


## Relationship Risk Factors ------
summary_stats_relationship <- data.frame(
  Variable = character(0),
  Category = character(0),
  Frequency = numeric(0),
  Percentage = numeric(0),
  stringsAsFactors = FALSE
)

for (var in relationship_cat) {
  var_counts <- table(emo_ipv_final[[var]])
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
print(latex_table_relationship)



## Community Risk Factors ------
summary_stats_community <- data.frame(
  Variable = character(0),
  Category = character(0),
  Frequency = numeric(0),
  Percentage = numeric(0),
  stringsAsFactors = FALSE
)

for (var in community_cat) {
  var_counts <- table(emo_ipv_final[[var]])
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
print(latex_table_community)


























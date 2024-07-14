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
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data_robustness2/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/"


## Load data -------
load(paste0(path, "step3_endireh.RData")) # main data set

## Change data name -----
data <- step3_endireh

## Summary Stat --------
summary(data)

## Create vectors -----
individual_num <- c("EDAD","ingm_muj","num_hij", "eda_hij", "eda_sex", "eda_mat")
individual_cat <- c("niv_edlow", "niv_edmedium", "niv_edhigh", "indigena", "cct_rec", "desempleo", "pareja_prev", "vio_inf", "vio_exp_inf", "vio_sex_inf", "con_sex", "mot_mat", "feminist_gradlow", "feminist_gradmedium", "feminist_gradhigh")

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
print(latex_table_ind)


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
print(latex_table_relationship)

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
print(latex_table_community)


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
print(latex_table_society)

## Combine ----
combine_table <- rbind(latex_table_ind, latex_table_relationship, latex_table_community, latex_table_society)

## Set row names -----
# Create a vector with the desired row names
row_names <- c(
  "Age of woman",
  "Income per month woman (in Mexican Peso)",
  "Number of children",
  "Age at first child",
  "Age at first sexual intercourse",
  "Age at marriage/cohabitation",
  "Age of partner",
  "Income per month partner (in Mexican Peso)",
  "Household members per room",
  "Homicide rate of men",
  "Homicide rate of women",
  "Total homicide rate",
  "Share of the population living in women-headed households 2020",
  "Female share of migrant population",
  "Male share of migrant population",
  "Gini index 2020",
  "Human development index 2020",
  "Women's economically active population 2020",
  "Men's economically active population 2020",
  "Share of senior positions in the municipality held by women in 2020",
  "Prevalence rate of common crimes against men",
  "Prevalence rate of common crimes against women",
  "Share of non-reported common crimes against men 2020",
  "Share of non-reported common crimes against women 2020",
  "Share of population that considered corruption a common/very common problem 2019",
  "Satisfaction of population with public services 2019"
)

# Assuming your data frame is named "summary_stats_community"
# Set the row names using the vector
rownames(combine_table) <- row_names

# In latex
xtable::xtable(combine_table, caption = "Summary Statistics for Continous Variables", label = "tab:summary_all")


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
print(latex_table_community)

## Combine ----
combine_table <- rbind(latex_table_individual, latex_table_relationship, latex_table_community)
first_part <- combine_table[1:52, ]
second_part <- combine_table[53:nrow(combine_table), ]

# Part I
risk_factors_1 <- c(
  "Education level of woman low",
  "",
  "Education level of woman medium",
  "",
  "Education level of woman high",
  "",
  "Indigenous",
  "",
  "CCT receiver",
  "",
  "Unemployment in the last 12 months",
  "",
  "Previous free-union or marriage",
  "",
  "Violence witnessed in childhood",
  "",
  "Violence experienced in childhood",
  "",
  "Sexual violence experienced in childhood",
  "",
  "Consent to first sexual intercourse",
  "",
  "Consent to current marriage or cohabitation",
  "",
  "Pro-gender equality attitude of woman low",
  "",
  "Pro-gender equality attitude of woman medium",
  "",
  "Pro-gender equality attitude of woman high",
  "",
  "Violence witnessed in childhood by partner",
  "",
  "Violence experienced in childhood by partner",
  "",
  "Division of housework male",
  "",
  "Division of housework females",
  "",
  "Division of housework both",
  "",
  "Woman's level autonomy about sex-life low",
  "",
  "Woman's level autonomy about sex-life medium",
  "",
  "Woman's level autonomy about sex-life high",
  "",
  "Woman's level autonomy about professional life and economic resources low",
  "",
  "Woman's level autonomy about professional life and economic resources medium",
  "",
  "Woman's level autonomy about professional life and economic resources high",
  ""
)
for (i in 2:length(risk_factors_1)) {
  if (risk_factors_1[i] == "") {
    risk_factors_1[i] <- paste0(risk_factors_1[i - 1], " no")
  }
}

# Set row names
rownames(first_part) <- risk_factors_1

# remove col
first_part <- first_part %>%
  select(-c("Variable"))

# In latex
xtable::xtable(first_part, caption = "Summary Statistics for Continous Variables", label = "tab:summary_all")


# Part II

risk_factors_2 <- c(
  "Woman's level autonomy about social and political activities low",
  "",
  "Woman's level autonomy about social and political activities medium",
  "",
  "Woman's level autonomy about social and political activities high",
  "",
  "Women’s perception about support from social networks low",
  "",
  "Women’s perception about support from social networks medium",
  "",
  "Women’s perception about support from social networks high",
  "",
  "Level of social interaction low",
  "",
  "Level of social interaction medium",
  "",
  "Level of social interaction high",
  "",
  "Level of social marginalization very low",
  "",
  "Level of social marginalization low",
  "",
  "Level of social marginalization medium",
  "",
  "Level of social marginalization high",
  "",
  "Level of social marginalization very high",
  "",
  "Type of community rural",
  "",
  "Type of community low urban",
  "",
  "Type of community medium urban",
  "",
  "Type of community high urban",
  ""
)

for (i in 2:length(risk_factors_2)) {
  if (risk_factors_2[i] == "") {
    risk_factors_2[i] <- paste0(risk_factors_2[i - 1], " yes")
  }
}

# Set row names
rownames(second_part) <- risk_factors_2

# remove col variable
second_part <- second_part %>%
  select(-c("Variable"))

# In latex
xtable::xtable(second_part, caption = "Summary Statistics for Continous Variables", label = "tab:summary_all")













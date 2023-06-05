############################ Correlation Matrix ################################

#############################  Prepare Data ##################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggplotify)
library(grid)
library(scales)
library(xtable)
library(psych)
library(corrplot)
library(gganimate)
library(vcd)
library(vcdExtra)
library(rcompanion)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/"


## Load data -------
load(paste0(path, "emo_ipv_final.RData")) # main data set

## Create vectors -----
individual_num <- c("EDAD","ingm_muj","num_hij", "eda_hij", "eda_sex", "eda_mat")
relationship_num <- c("eda_par2", "ingm_par", "hacin")
community_num <- c("mhr20", "fhr20", "ghr20", "phogjef_f", "pres_2020_f", "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m", "ParPolF")
society_num <- c("MasPrev", "FemPrev", "MasNoDen", "FemNoDen", "cor19", "satis19")

individual_cat <- c("niv_edlow", "niv_edmedium", "niv_edhigh", "indigena", "cct_rec", "empleo_vida", "empleo_5_años", "desempleo", "extr_inf", "pareja_prev", "vio_inf", "vio_exp_inf", "vio_sex_inf", "con_sex", "mot_mat", "feminist_gradlow", "feminist_gradmedium", "feminist_gradhigh")
relationship_cat <- c("vio_inf_par", "vio_exp_inf_par", "act_distmales", "act_distfemales", "act_distboth", "lib_sex_gradlow", "lib_sex_gradmedium", "lib_sex_gradhigh", "lib_eco_gradlow", "lib_eco_gradmedium", "lib_eco_gradhigh", "lib_soc_gradlow", "lib_soc_gradmedium", "lib_soc_gradhigh", "redsoc_gradlow", "redsoc_gradmedium", "redsoc_gradhigh", "rout_gradlow", "rout_gradmedium", "rout_gradhigh")
community_cat <- c("Marg15very_low", "Marg15low", "Marg15medium", "Marg15high", "Marg15very_high", "Type_comrural", "Type_comlow_urban", "Type_commedium_urban", "Type_comhigh_urban")


# Convert to numerical
emo_ipv_final[individual_num] <- lapply(emo_ipv_final[individual_num], as.numeric)
emo_ipv_final[relationship_num] <- lapply(emo_ipv_final[relationship_num], as.numeric)
emo_ipv_final[community_num] <- lapply(emo_ipv_final[community_num], as.numeric)
emo_ipv_final[society_num] <- lapply(emo_ipv_final[society_num], as.numeric)

# Continous Variables ------

## Correlation Matrix --------

# Subset the dataframe to include only numerical variables
numerical_df <- emo_ipv_final[, sapply(emo_ipv_final, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_df)

# Generate the correlation plot
corr <- {corrplot(cor_matrix, tl.col = "black"); recordPlot()}

# Save plot
ggsave(plot = replayPlot(corr), filename=paste0(path_save, "correlation_continous.png"))


## Correlation Table -----

# Positive correlation:

# Create a correlation matrix
cor_matrix <- cor(numerical_df)

# Get the upper triangle of the correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(cor_matrix)

# Find the indices of correlations greater than 0.75
strong_cor_indices <- which(cor_matrix > 0.7 & upper_triangle, arr.ind = TRUE)

# Extract the variable pairs and their correlations
strong_cor_table <- data.frame("Risk Factor 1" = rownames(cor_matrix)[strong_cor_indices[,1]],
                               "Risk Factor 2" = colnames(cor_matrix)[strong_cor_indices[,2]],
                               Correlation = cor_matrix[strong_cor_indices])
strong_cor_table <- strong_cor_table[order(strong_cor_table$Correlation, decreasing = TRUE), ]
  

# Convert DataFrame to LaTeX table
latex_table_cor <- xtable::xtable(strong_cor_table, caption = "Positive Correlation Continous Variabes", label = "tab:corrpos")

# Print
print(latex_table_cor)

# Positive correlation:

# Create a correlation matrix
cor_matrix <- cor(numerical_df)

# Get the upper triangle of the correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(cor_matrix)

# Find the indices of correlations greater than 0.75
strong_cor_indices_neg <- which(cor_matrix < -0.7 & upper_triangle, arr.ind = TRUE)

# Extract the variable pairs and their correlations
strong_cor_neg  <- data.frame("Risk Factor 1" = rownames(cor_matrix)[strong_cor_indices_neg [,1]],
                              "Risk Factor 2" = colnames(cor_matrix)[strong_cor_indices_neg [,2]],
                               Correlation = cor_matrix[strong_cor_indices_neg ])
strong_cor_neg <- strong_cor_neg[order(strong_cor_neg$Correlation, decreasing = TRUE), ]

# Convert DataFrame to LaTeX table
latex_table_cor <- xtable::xtable(strong_cor_neg, caption = "Negative Correlation Continous Variabes", label = "tab:corrneg")

# Print
print(latex_table_cor)


# Categorical Variables -------

cat_df <- emo_ipv_final[, sapply(emo_ipv_final, is.factor)]
cat_df <- cat_df %>%
  select(-c("SEXO", "par_sex"))

## Association Matrix -----

# Example
# Create a contingency table for the two variables
cont_table <- table(emo_ipv_final$feminist_gradhigh, emo_ipv_final$feminist_gradmedium)

# Calculate Cramer's V
cramers_v <- assocstats(cont_table)$cramer

# Print the result
print(cramers_v)

# Correlations plot with Cramers V
corrplot::corrplot(DescTools::PairApply(cat_df, DescTools::CramerV), is.corr = F, tl.col = "black", tl.cex = 0.6)



## Association Table -----

# Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(cat_df),
                  nrow = length(cat_df),
                  dimnames = list(names(cat_df), 
                                  names(cat_df)))
# Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m, cat_df)


# Get the upper triangle of the correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(cor_matrix)

# Find the indices of correlations greater than 0.75
strong_cramers <- which(cor_matrix >0.7 & upper_triangle, arr.ind = TRUE)

# Extract the variable pairs and their correlations
strong_cramers_df  <- data.frame("Risk Factor 1" = rownames(cor_matrix)[strong_cramers [,1]],
                              "Risk Factor 2" = colnames(cor_matrix)[strong_cramers [,2]],
                              Correlation = cor_matrix[strong_cramers ])
strong_cramers_df <- strong_cramers_df[order(strong_cramers_df$Correlation, decreasing = TRUE), ]


# Convert DataFrame to LaTeX table
latex_table_cor <- xtable::xtable(strong_cramers_df, caption = "Moderato to Strong Association Categorical Variables", label = "tab:cramersv")

# Print
print(latex_table_cor)












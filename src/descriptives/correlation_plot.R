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


# Convert to numerical
emo_ipv_final[individual_num] <- lapply(emo_ipv_final[individual_num], as.numeric)
emo_ipv_final[relationship_num] <- lapply(emo_ipv_final[relationship_num], as.numeric)
emo_ipv_final[community_num] <- lapply(emo_ipv_final[community_num], as.numeric)
emo_ipv_final[society_num] <- lapply(emo_ipv_final[society_num], as.numeric)


# Correlation Matrix --------

# Subset the dataframe to include only numerical variables
numerical_df <- emo_ipv_final[, sapply(emo_ipv_final, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_df)

# Generate the correlation plot
corr <- {corrplot(cor_matrix, tl.col = "black"); recordPlot()}

# Save plot
ggsave(plot = replayPlot(corr), filename=paste0(path_save, "correlation_continous.png"))


# Correlation Table -----

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


# Convert DataFrame to LaTeX table
latex_table_cor <- xtable::xtable(strong_cor_neg, caption = "Negative Correlation Continous Variabes", label = "tab:corrneg")

# Print
print(latex_table_cor)





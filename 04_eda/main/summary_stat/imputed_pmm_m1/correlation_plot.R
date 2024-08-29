############################ Correlation Matrix ################################

## Load packages ---------------------------------------------------------------
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

## Set path --------------------------------------------------------------------
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/imputed_pmm_m1/corrplot/"

## Load data -------------------------------------------------------------------
load(paste0(path_data, "step3_endireh.RData"))

## Change data name ------------------------------------------------------------
data <- step3_endireh

## Subset not needed -----------------------------------------------------------
data <- data %>%
  select(-c("num_hij_par", "num_hij_par_muj"))

## Create vectors -----
individual_num <- c("EDAD","ingm_muj","num_hij", "eda_hij", "eda_sex", "eda_mat")
relationship_num <- c("eda_par2", "ingm_par", "hacin")
community_num <- c("mhr20", "fhr20", "ghr20", "phogjef_f", "pres_2020_f", 
                   "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m", "ParPolF")
society_num <- c("MasPrev", "FemPrev", "MasNoDen", "FemNoDen", "cor19", "satis19")

# Convert to numerical
data[individual_num] <- lapply(data[individual_num], as.numeric)
data[relationship_num] <- lapply(data[relationship_num], as.numeric)
data[community_num] <- lapply(data[community_num], as.numeric)
data[society_num] <- lapply(data[society_num], as.numeric)

# Continuous Variables ------

## Correlation Plot ------------------------------------------------------------

# Subset the data set to include only numerical variables
numerical_df <- data[, sapply(data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_df)

# Generate the correlation plot
corr <- {corrplot(cor_matrix, tl.col = "black"); recordPlot()}

# Save plot
#ggsave(plot = replayPlot(corr), filename=paste0(path_save, "correlation_continous.png"))

## Correlation Table -----------------------------------------------------------

# Positive correlation:

# Create a correlation matrix
cor_matrix <- cor(numerical_df)

# Get the upper triangle of the correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(cor_matrix)

# Find correlations greater than 0.7
cor_positive <- which(cor_matrix >= 0.7 & upper_triangle, arr.ind = TRUE)

# Extract the variable pairs and their correlations
cor_positive_table <- data.frame("Risk Factor 1" = rownames(cor_matrix)[cor_positive[,1]],
                                 "Risk Factor 2" = colnames(cor_matrix)[cor_positive[,2]],
                                 Correlation = cor_matrix[cor_positive])
cor_positive_table <- cor_positive_table[order(cor_positive_table$Correlation, decreasing = TRUE), ]


# Convert DataFrame to LaTeX table
cor_positive_table <- xtable::xtable(cor_positive_table, caption = "Positive Correlation Continous Variabes", label = "tab:corrpos")

# Print
print(xtable(cor_positive_table), include.rownames=FALSE)

# Negative correlation:

# Create a correlation matrix
cor_matrix <- cor(numerical_df)

# Get the upper triangle of the correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(cor_matrix)

# Find the indices of correlations greater than 0.75
cor_negative <- which(cor_matrix < -0.7 & upper_triangle, arr.ind = TRUE)

# Extract the variable pairs and their correlations
cor_negative_table  <- data.frame("Risk Factor 1" = rownames(cor_matrix)[cor_negative[,1]],
                                  "Risk Factor 2" = colnames(cor_matrix)[cor_negative[,2]],
                                  Correlation = cor_matrix[cor_negative])
cor_negative_table <- cor_negative_table[order(cor_negative_table$Correlation, decreasing = TRUE), ]

# Convert data frame to LaTeX table
cor_negative_table <- xtable::xtable(cor_negative_table, caption = "Negative Correlation Continous Variabes", label = "tab:corrneg")

# Print
print(xtable(cor_negative_table), include.rownames=FALSE)

# Categorical Variables -------

## Association Plot -----

cat_df <- data[, sapply(data, is.factor)]
cat_df <- cat_df %>%
  select(-c("SEXO", "par_sex"))

# Association plot
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

# Fill association matrix using Cramers V
cor_matrix <- calculate_cramer(empty_m, cat_df)

# Get the upper triangle of the correlation matrix (excluding the diagonal)
upper_triangle <- upper.tri(cor_matrix)

# Find the indices of associations greater than 0.75
strong_cramers <- which(cor_matrix >0.7 & upper_triangle, arr.ind = TRUE)

# Extract the variable pairs and their correlations
strong_cramers_table  <- data.frame("Risk Factor 1" = rownames(cor_matrix)[strong_cramers[,1]],
                                    "Risk Factor 2" = colnames(cor_matrix)[strong_cramers[,2]],
                                    Association = cor_matrix[strong_cramers])
strong_cramers_table <- strong_cramers_table[order(strong_cramers_table$Association, decreasing = TRUE), ]


# Convert data frame to LaTeX table
strong_cramers_table <- xtable::xtable(strong_cramers_table, caption = "Moderato to Strong Association Categorical Variables", label = "tab:cramersv",  include.rownames = FALSE)

# Print
print(xtable(strong_cramers_table), include.rownames=FALSE)













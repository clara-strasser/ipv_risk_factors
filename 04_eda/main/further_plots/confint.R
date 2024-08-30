############################ Confidence Intervals #############################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)
library(stabs)

## Set path ---------------------------------------------------------------
path_model <- "/Users/clarastrasser/ipv_data/results/main/"
path_data <- "/Users/clarastrasser/ipv_data/data/final_data/"

## Load data ---------------------------------------------------------------
load(paste0(path_model, "confintemoipv.RData")) 
load(paste0(path_model, "model.RData"))
load(paste0(path_data, "data_imp_pmm_m1.RData")) 

## Load function --------------------------------------------------------------------
source("src/conf_table.R")
source("src/custom_mboost.R")

# CI All -----------------------------------------------------------------------

## Table ------

# Create a list of the factor variables
which_values <- c(14, 16, 17, 18, 26, 32, 36, 37, 40, 41, 44, 46, 51, 52)
which_values <- c(85)

# Initialize an empty list to store the results
result_list <- list()

# Loop through each which value and call the conf_table function
for (which_value in which_values) {
  result_list[[as.character(which_value)]] <- conf_table(confintemoipv, which = which_value)
}

# Combine the results into a single data frame
result_df <- do.call(rbind, result_list)

# Print the resulting data frame
result_df[, 2:3] <- round(result_df[, 2:3], 3)
print(result_df)

## Plot ------

# Define Plots
plots <- list(
  list(which = 14, xlab = "Unemployment"),
  list(which = 16, xlab = "Violence Witnessed in Childhood"),
  list(which = 17, xlab = "Violence Experienced in Childhood"),
  list(which = 18, xlab = "Sexual Violence Experienced in Childhood"),
  list(which = 26, xlab = "Consent to Sex at First Sexual Intercourse"),
  list(which = 27, xlab = "Women's Age at First Sexual Intercourse by Consent to Sex Yes"),
  list(which = 32, xlab = "Consent to Current Marriage or Cohabitation"),
  list(which = 36, xlab = "Violence Witnessed by Partner in Childhood"),
  list(which = 37, xlab = "Violence Experienced by Partner in Childhood"),
  list(which = 40, xlab = "Division of Housework Among Both"),
  list(which = 41, xlab = "Division of Housework Among Men"),
  list(which = 44, xlab = "Woman’s Level Autonomy about Sex Life Medium"),
  list(which = 46, xlab = "Woman’s Level Autonomy about Economic Res. Medium"),
  list(which = 51, xlab = "Women’s Social Network Support High"),
  list(which = 52, xlab = "Level of Social Interaction Medium"),
  list(which = 85, xlab = "Share of Non-Reported Common Crimes Against Men")
)
plots <- list(
  list(which = 85, xlab = "Share of Non-Reported Common Crimes Against Men")
)
# Loop through the list of plots and generate and save each plot
for (plot_info in plots) {
  # Set up plot parameters
  filename <- paste("plot", plot_info$which, ".png", sep = "")
  png(filename, width = 800, height = 800)
  par(cex.lab = 2.5, cex.axis = 2.5, cex.main = 3, cex.sub = 3, mar = c(6, 6, 4, 2), font = 2 )
  plot(confintemoipv, which = plot_info$which, col = "#002b58", xlab = plot_info$xlab, type = "link")
  dev.off()
}

# CI Selected ------------------------------------------------------------------

# MasNoDen
mean_masnoden <- mean(data_imp_pmm_m1$MasNoDen)

# Plot 
custom_plot_mboost_ci(confintemoipv, which = "bols(MasNoDen, intercept = FALSE)",
                      col = "#002b58", xlab = "Share of Non-Reported Common Crimes Against Men",
                      ylab = "Mean-Centered Partial Effects",
                      mean=mean_masnoden)
grid()

# eda_sex
mean_eda_sex <- mean(data_imp_pmm_m1$eda_sex)

# Plot
custom_plot_mboost_ci(confintemoipv,
                      which = "bols(eda_sex, by = con_sex, intercept = FALSE)",
                      col = "#002b58",
                      xlab = "Woman's Age at First Sexual Intercourse by Consent to Sex yes",
                      ylab = "Mean-Centered Partial Effects",
                      mean=mean_eda_sex)
grid()

######################### Cross - Validation Results #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(mboost)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/results/model1_imputed/"

## Load data -------
load(paste0(path_data,"cv1.RData")) # cross-validation results

# Plot -----
setwd("/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/results/plots")

# Number of boosting iterations
cross_validation_model1 <- plot(cvemoipv,
     ylim = c(0.45, 0.55),
     xlab = "Number of Boosting Iterations",
     ylab = "Average Empirical Risk",
     main = "Subsampling")

# Save ----
png("cross_validation_model1.png", width = 800, height = 600)
print(cross_validation_model1)
dev.off()


# Save in folder
ggsave("/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/results/plots/cross_validation_model1.png")






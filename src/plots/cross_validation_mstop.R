######################### Cross - Validation Results #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(mboost)

## Set path -----
path_data <- ""

## Load data -------
load("~/cvemoipv.RData") # cross-validation results

# Plot -----

# Number of boosting iterations
plot(cvemoipv,
     ylim = c(0.45, 0.55),
     xlab = "Number of Boosting Iterations",
     ylab = "Average Empirical Risk",
     main = "Cross-Validation Method: Subsampling")


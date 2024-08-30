############################ Confidence Intervals #############################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)
library(stabs)

## Set path ---------------------------------------------------------------
path_model <- "/Users/clarastrasser/ipv_data/results/robustness/"
path_data <- "/Users/clarastrasser/ipv_data/data/final_data/"

## Load data -------------------------------------------------------------------
load(paste0(path_model, "confintemoipv.RData")) 
load(paste0(path_data, "data_no_imp.RData")) 

## Load function ---------------------------------------------------------------
source("src/conf_table.R")
source("src/custom_mboost.R")

## Plot ------------------------------------------------------------------------

# eda_sex
mean_eda_sex <- mean(data_no_imp2$eda_sex)

# Plot
custom_plot_mboost_ci(confintemoipv,
                      which = "bols(eda_sex, by = con_sex, intercept = FALSE)",
                      col = "#002b58",
                      xlab = "Woman's Age at First Sexual Intercourse by Consent to Sex yes",
                      ylab = "Mean-Centered Partial Effects",
                      mean=mean_eda_sex)
grid()

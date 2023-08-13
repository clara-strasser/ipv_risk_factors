######################### Step 1: Complete Cases - TEST ########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(arsenal) # useful for comparison
library(cowplot)


## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/imputed_vs_not_imputed/"

## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set
load(paste0(path_data, "step1_endireh.RData"))


## Prepare data -----

# Convert characters to numeric
endireh_2021 <- endireh_2021 %>%
  mutate(pres_2020_f = as.numeric(pres_2020_f),
         pres_2020_m = as.numeric(pres_2020_m),
         gini20 = as.numeric(gini20),
         idh2020 = as.numeric(idh2020),
         pea_f = as.numeric(pea_f),
         pea_m = as.numeric(pea_m),
         MasPrev = as.numeric(MasPrev),
         FemPrev = as.numeric(FemPrev))

# Remove unnecessary risk factors 
# edu_parlow
# edu_parmedium
# edu_parhigh        
# ind_par 
# sep_ex 
# vio_fis_ex 
# vio_emo_ex 
# vio_sex_ex  
# vio_eco_ex  
# ing_par
# ing_muj

endireh_2021 <- endireh_2021 %>%
  select(-c("edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par", 
            "sep_ex", "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex", "ing_par", "ing_muj"))

# Select numeric values
continuous_vars <- names(endireh_2021)[sapply(endireh_2021, is.numeric)]

# Select factor variables
factor_vars <- names(endireh_2021)[sapply(endireh_2021, is.factor)]


# Test Single Imputation ------
# Remark: single imputation (instead of multiple imputation) was done using "pmm"
# Trade-off: bias and variance

## Graphical comparison ------

# Numeric variables
for (var in continuous_vars) {
  
  # Create the histogram plot - original data
  plot_1 <- ggplot(endireh_2021, aes(x = !!sym(var))) +
    geom_histogram(fill = "#002b58", color = "black", bins = 20) +
    labs(title = paste("Distribution of", var, "- Orginal Data"),
         x = var, y = "Absolute Frequency")
  
  # Create the histogram plot - imputed data
  plot_2 <- ggplot(step1_endireh, aes(x = !!sym(var))) +
    geom_histogram(fill = "#002b58", color = "black", bins = 20) +
    labs(title = paste("Distribution of", var, "- Imputed Data"),
         x = var, y = "Absolute Frequency")
  
  # Combine the histograms into a single plot
  combined_plot <- plot_grid(plot_1, plot_2, nrow = 1)
  

  # Save the plot as a PNG file
  ggsave(filename = paste0(path_save, var, ".png"), plot = combined_plot)
}

# Factor variables
for (var in factor_vars) {
  
  # Prepare the data for plotting
  plot_data <- endireh_2021 %>%
    group_by(!!sym(var)) %>%
    summarize(Frequency = n())
  
  # Create the bar chart plot - original data
  plot_1 <- ggplot(plot_data, aes(x = !!sym(var), y = Frequency, fill = !!sym(var))) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(title = paste("Distribution of", var, "- Orginal Data"),
         x = var, y = "Absolute Frequency") +
    scale_fill_manual(values = c("no" = "#002b58", "yes" = "#620042", "NA" = "gray"),
                      labels = c("no" = "No", "yes" = "Yes", "NA" = "Missing"))
  
  # Prepare the data for plotting
  plot_data_2 <- step1_endireh %>%
    group_by(!!sym(var)) %>%
    summarize(Frequency = n())
  
  # Create the bar chart plot - imputed data
  plot_2 <- ggplot(plot_data_2, aes(x = !!sym(var), y = Frequency, fill = !!sym(var))) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(title = paste("Distribution of", var, "- Imputed Data"),
         x = var, y = "Absolute Frequency") +
    scale_fill_manual(values = c("no" = "#002b58", "yes" = "#620042", "NA" = "gray"),
                      labels = c("no" = "No", "yes" = "Yes", "NA" = "Missing"))
  
  # Combine the histograms into a single plot
  combined_plot <- plot_grid(plot_1, plot_2, nrow = 1)
  
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(path_save, var, ".png"), plot = combined_plot)
  
}



## Tabular comparison -------
# Remarks: attention to the following variables is needed:
# idh2020
# fhr20

# EDAD:
summary(endireh_2021$EDAD)
summary(step1_endireh$EDAD)

# ingm_muj
summary(endireh_2021$ingm_muj)
summary(step1_endireh$ingm_muj) # remark: the mean is higher


# ingm_par
summary(endireh_2021$ingm_par)
summary(step1_endireh$ingm_par) # remark: the mean is lower

# vio_inf_par
summary(endireh_2021$vio_inf_par)
summary(step1_endireh$vio_inf_par)

# vio_exp_inf_par
summary(endireh_2021$vio_exp_inf_par)
summary(step1_endireh$vio_exp_inf_par)

# fhr20
summary(endireh_2021$fhr20)
summary(step1_endireh$fhr20) # remark: the mean is lower

# idh2020
summary(endireh_2021$idh2020)
summary(step1_endireh$idh2020) # remark: the mean is lower


# Comparison
summary <- summary(comparedf(endireh_2021, step1_endireh))
summary[["frame.summary.table"]] # shows how the NAs were substituted


## Statistical test -------

# Continuous variables
# Perform statistical tests on numerical variables before and after imputation

# Assuming you have two data frames 'original_data' and 'imputed_data'

# Initialize an empty list to store the test results
test_results <- list()

# Perform statistical tests for each numerical variable
for (var in continuous_vars) {
  
  # Extract the variable from original data
  original_var <- endireh_2021[[var]]
  
  # Extract the variable from imputed data
  imputed_var <- step1_endireh[[var]]
  
  # Perform the statistical test (e.g., Kolmogorov-Smirnov test)
  test_result <- ks.test(original_var, imputed_var)
  
  # Store the test result in the list
  test_results[[var]] <- test_result
}

# Print the test results
for (var in continuous_vars) {
  test_result <- test_results[[var]]
  cat("Variable:", var, "\n")
  cat("Test statistic:", test_result$statistic, "\n")
  cat("P-value:", test_result$p.value, "\n")
  cat("----------------------------------\n")
}


# Results:
# small p-values for: 
# ingm_muj
# fhr20

# Factor variables
# Perform chi-square test on factor variables
for (var in factor_vars) {
  # Create a contingency table - original data
  table1 <- table(endireh_2021[[var]], useNA = "always")
  
  # Create a contingency table - imputed data
  table2 <- table(step1_endireh[[var]], useNA = "always")
  
  # Perform chi-square test
  chi_square_test <- chisq.test(table1, table2)
  
  # Extract p-value
  p_value <- chi_square_test$p.value
  
  # Print the variable name and p-value
  cat("Variable:", var, "\tP-value:", p_value, "\n")
}






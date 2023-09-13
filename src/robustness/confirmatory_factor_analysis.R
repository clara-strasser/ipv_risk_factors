####################### Confirmatory Factor Analysis ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(corrplot)
library(lavaan)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data -------
load(paste0(path, "TB_SEC_IVaVD.RData")) # all women
load(paste0(path_data, "endireh_2021.RData")) # subset women (married/cohabiting, male partner, >= 1 child)

## Keep relevant observations -----
ids <- endireh_2021$ID_PER

## Subset -----

# Set columns
vio_emo <- paste0("P14_3_", c(10:22, "23AB", "24AB"))

# Reduce data frame
data <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", vio_emo))

# Subset
data <- data[data$ID_PER %in% ids,]

# Remove df
rm(TB_SEC_IVaVD)
rm(endireh_2021)
rm(ids)

# Manipulate -----

# Convert NAs to 0
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), ~ifelse(is.na(.), 4, .))

# Convert character variables to factors
# 1 = many times
# 2 = a few times
# 3 = once
# 4 = no
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), ~factor(.x, levels = c("1", "2", "3", "4")))

# Convert columns to numeric
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), as.numeric)

# Remove columns
factor_data <- data %>%
  select(-c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

# Confirmatory Factor Analysis ------

# Keep columns
# Your vector of values
values <- c("P14_3_10", "P14_3_11", "P14_3_12", "P14_3_13", "P14_3_14", "P14_3_15",
            "P14_3_16", "P14_3_17", "P14_3_18", "P14_3_19", "P14_3_20", "P14_3_21",
            "P14_3_22", "P14_3_23AB", "P14_3_24AB")

# Concatenate the values with a plus sign between each value
result <- paste(values, collapse = " + ")

# Print the result
cat(result)


## One-Factor Model ------

# Model
one_fac_model <- "psych_vio =~ P14_3_10 + P14_3_11 + P14_3_12 +
P14_3_13 + P14_3_14 + P14_3_15 + P14_3_16 + P14_3_17 + P14_3_18 + 
P14_3_19 + P14_3_20 + P14_3_21 + P14_3_22 + P14_3_23AB + P14_3_24AB"

# Fit
fit_one <- cfa(one_fac_model, data = factor_data_test)

# Results
summary(fit_one)

## Two-Factor Model ------

## Three-Factor Model ------
three_fac_model <- "emo_abuse =~ P14_3_10 + P14_3_11 + P14_3_13 + P14_3_14 + P14_3_23AB + P14_3_24AB
                 threats =~ P14_3_15 + P14_3_18 + P14_3_19 + P14_3_20
                  surv =~ P14_3_12 + P14_3_16 + P14_3_17 + P14_3_22"

# Fit
fit_three <- cfa(three_fac_model, data = factor_data_test)

# Results
summary(fit_three, fit.measures = TRUE)

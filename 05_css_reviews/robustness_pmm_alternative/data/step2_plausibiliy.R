######################### Step 2: Plausbiliity Analysis ######################

## Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


## Set path --------------------------------------------------------------------
path_data <- "/Users/clarastrasser/ipv_data/data/final_data/rob_pmm_alt/"

## Load data -------------------------------------------------------------------
load(paste0(path_data, "step1_endireh_pmm_alt.RData"))


## Change data name ------------------------------------------------------------
data <- step1_endireh


# Part I: Implausible Results ---------
# Initial data set: 63.152

# Create an empty vector to store implausible IDs
implausible <- integer()

## vio_emo_año and vio_emo_vida
# Explanation: vio_emo_año may not be "yes" if vio_emo_vida is "no"
plaus_1a <- data[data$vio_emo_año_alt == "yes" & data$vio_emo_vida == "no", ] # 0
plaus_1b <- data[data$vio_emo_año == "yes" & !is.na(data$vio_emo_año) & data$vio_emo_vida == "no", ] # 0

# Add implausible IDs to the vector
implausible <- c(implausible, plaus_1a$ID_PER, plaus_1b$ID_PER)

## EDAD and eda_sex
# Explanation: Women’s age at first sexual intercourse cannot be greater than women’s age at the time of being surveyed
plaus_2 <- data[!is.na(data$eda_sex) & !is.na(data$EDAD) & data$eda_sex > data$EDAD, ] # 6

# Add implausible IDs to the vector
implausible <- c(implausible, plaus_2$ID_PER)

## EDAD and eda_mat
# Explanation: Women’s age at first marriage (or at cohabitation) cannot be greater than women’s age at the time of being surveyed
plaus_3 <- data[!is.na(data$eda_mat) & !is.na(data$EDAD) & data$eda_mat > data$EDAD, ] # 35

# Add implausible IDs to the vector
implausible <- c(implausible, plaus_3$ID_PER)

## EDAD and eda_hij
# Explanation: Women’s age at first childbirth cannot be greater than women’s age at the time of being surveyed
plaus_4 <- data[!is.na(data$eda_hij) & !is.na(data$EDAD) & data$eda_hij > data$EDAD, ] # 19

# Add implausible IDs to the vector
implausible <- c(implausible, plaus_4$ID_PER)

# eda_hij and eda_sex
# Explanation: Women’s age at first sexual intercourse cannot be greater than women’s age at first childbirth
plaus_5 <- data[!is.na(data$eda_sex) & !is.na(data$eda_hij) & data$eda_sex > data$eda_hij, ] # 176

# Add implausible IDs to the vector
implausible <- c(implausible, plaus_5$ID_PER)


## ingm_muj and empleo_vida
plaus_6 <- data[data$ingm_muj > 0 & data$empleo_vida == "no", ] # 154

# Add implausible IDs to the vector
implausible <- c(implausible, plaus_6$ID_PER)

rm(plaus_1a, plaus_1b, plaus_2, plaus_3, plaus_4, plaus_5, plaus_6)


# Part II: Implausible Results ---------

# Define the patterns to match
patterns <- c("niv_ed", "feminist_grad", "lib_sex_", "lib_eco_", "lib_soc_", "redsoc_", "rout_", "act_dist", "Type_", "Marg")

# Initialize an empty vector to store the rows
rows_all_no <- vector("list", length(patterns))

# Check each pattern separately
for (i in seq_along(patterns)) {
  pattern <- patterns[i]
  
  # Check if all combinations are "no" using startsWith()
  all_no_combinations <- rowSums(data[, startsWith(names(data), pattern)] == "no") == sum(startsWith(names(data), pattern))
  
  # Get the rows where all combinations are "no"
  rows_all_no[[i]] <- data[all_no_combinations, ]
}


# Remove implausible -----
step2_endireh <- data[!(data$ID_PER %in% implausible),]


# Save data ----------
# Data set: 62.774
save(step2_endireh, file = paste0(path_data,"step2_endireh.RData"))




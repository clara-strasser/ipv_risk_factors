##############################  Prepare Data ##################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)


## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/non_imputed2/"
path_data_save <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data_robustness2/"

## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set

# Data Preparation Process ------

## STEP 1: Remove NA Observations -----
# Initial data set: 63.152

# Calculate NAs of all variables
print(data.frame(Variable = names(endireh_2021), 
                 Count = sapply(endireh_2021, function(x) sum(is.na(x))), 
                 Percentage = sapply(endireh_2021, function(x) sum(is.na(x)) / length(x) * 100)))

# Remove non-relevant risk factors 
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


# Keep only columns with missing data
endireh_missing <- endireh_2021 %>%
  select(where(~ any(is.na(.))))

# Keep complete cases
# Meaning: remove all observations with min. one missing value in the covariates 
data <- endireh_2021[complete.cases(endireh_2021), ]

## STEP 2: Plausibility Analysis ---------
# Initial data set: 27.554

## vio_emo_año and vio_emo_vida
# Explanation: vio_emo_año may not be "yes" if vio_emo_vida is "no"
plaus_1a <- data[data$vio_emo_año_alt == "yes" & data$vio_emo_vida == "no", ]
plaus_1b <- data[data$vio_emo_año == "yes" & !is.na(data$vio_emo_año) & data$vio_emo_vida == "no", ]

## eda_sex
# Explanation: Women’s age at first sexual intercourse cannot be greater than women’s age at the time of being surveyed
plaus_2 <- data[!is.na(data$eda_sex) & !is.na(data$EDAD) & data$eda_sex > data$EDAD, ]

## eda_mat
# Explanation: Women’s age at first marriage (or at cohabitation) cannot be greater than women’s age at the time of being surveyed
plaus_3 <- data[!is.na(data$eda_mat) & !is.na(data$EDAD) & data$eda_mat > data$EDAD, ]

## eda_hij
# Explanation: Women’s age at first childbirth cannot be greater than women’s age at the time of being surveyed
plaus_4 <- data[!is.na(data$eda_hij) & !is.na(data$EDAD) & data$eda_hij > data$EDAD, ]

## ingm_muj and empleo_vida
plaus_5 <- data[data$ingm_muj > 0 & data$empleo_vida == "no", ] # 57 implausible results
implausbile <- plaus_5$ID_PER

rm(plaus_1a, plaus_1b, plaus_2, plaus_3, plaus_4, plaus_5)

# Results: 94 implausible results

# Remove implausible observations
data <- data[!(data$ID_PER %in% implausbile),]

## STEP 3: Outlier Detection --------
# Initial data set: 27.497

## Age Woman
summary(data$EDAD)
table(data$EDAD)
hist(data$EDAD, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_age <- ggplot(data, aes(x = "", y = EDAD)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "EDAD", title = "Age of Woman in Years") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_age.png"), plot = boxplot_age)


## Age at childbirth
summary(data$eda_hij)
table(data$eda_hij)
hist(data$eda_hij, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_age_children <- ggplot(data, aes(x = "", y = eda_hij)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "eda_hij", title = "Age of Woman at First Childbirth in Years") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_age_children.png"), plot = boxplot_age_children)

## Age at first sexual intercourse
summary(data$eda_sex)
table(data$eda_sex)
hist(data$eda_sex, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_age_sex <- ggplot(data, aes(x = "", y = eda_sex)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "eda_sex", title = "Age of Woman at First Sexual Intercourse in Years") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_age_sex.png"), plot = boxplot_age_sex)

## Age at marriage
summary(data$eda_mat)
table(data$eda_mat)
hist(data$eda_mat, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_age_mat <- ggplot(data, aes(x = "", y = eda_mat)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "eda_mat", title = "Age of Woman at Marriage/Cohabitation in Years") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_age_mat.png"), plot = boxplot_age_mat)

## Age of partner
summary(data$eda_par2)
table(data$eda_par2)
hist(data$eda_par2, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_age_partner <- ggplot(data, aes(x = "", y = eda_par2)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "eda_par", title = "Age of Partner in Years") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_age_partner.png"), plot = boxplot_age_partner)

## Average number of househod members in the dwelling
summary(data$hacin)
table(data$hacin)
hist(data$hacin, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_hacin <- ggplot(data, aes(x = "", y = hacin)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "hacin", title = "Household Members per Room in the Dwelling") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_hacin.png"), plot = boxplot_hacin)

## Number of children 
summary(data$num_hij)
table(data$num_hij)
hist(data$num_hij, breaks = nrow(data))

# Create a boxplot with outliers marked
boxplot_num_hij <- ggplot(data, aes(x = "", y = num_hij)) +
  geom_boxplot(width = 0.5, fill = "#e9e9e9", color = "#002b58") +
  stat_boxplot(geom='errorbar', width = 0.3) +
  labs(x = "", y = "num_hij", title = "Number of Children") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "boxplot_num_hij.png"), plot = boxplot_num_hij)


## Income per Month Woman

# Remove all observations with igmn_muj > 800.000$ (definitely outliers, > 42.000 Euros)
data <- data %>%
  filter(ingm_muj < 800000)

# Get summary of "ingm_muj"
table(data$ingm_muj, useNA = "ifany")
summary(data$ingm_muj)

# Save mean, median and max_freq
mean_income <- mean(data$ingm_muj, na.rm = TRUE)
median_income <- median(data$ingm_muj, na.rm = TRUE)
min_value <- min(data$ingm_muj)
max_value <- max(data$ingm_muj)
bin_width <- 1000
breaks <- seq(min_value, max_value + bin_width, by = bin_width)
max_freq <- max(hist(data$ingm_muj, breaks = breaks, plot = FALSE)$counts)

# Plot histogram
income_distribution_women <- ggplot(data, aes(x = ingm_muj)) +
  geom_histogram(binwidth = 1000, fill = "#e9e9e9", color = "#4d565e") +
  geom_vline(aes(xintercept = mean_income, color = "Mean"), linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = median_income, color = "Median"), linetype = "dashed", size = 0.8) +
  labs(x = "Monthly Income of Woman in Mexican Pesos $", y = "Absolute Frequency", title = "Distribution of Women's Monthly Income in Mexican Pesos $") +
  scale_x_continuous(breaks = seq(min_value, max_value, by = 10000), labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(limits = c(0, max_freq), expand = c(0, 0), breaks = seq(0, max_freq, 5000), labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c("Median" = "#620042", "Mean" = "#002b58"), labels = c("Mean", "Median"), guide = "none") +
  scale_linetype_manual(values = c("dashed", "dashed"), guide = guide_legend(override.aes = list(colour = c("#620042", "#002b58")))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "income_distribution_women.png"), plot = income_distribution_women)


## Income per Month Partner

# Remove all observations with igmn_par > 999.997$ (definitely outliers, > 52.000 Euros)
data <- data %>%
  filter(ingm_par < 250000) 

# Get summary of "ingm_par"
table(data$ingm_par, useNA = "ifany")
summary(data$ingm_par)

# Save mean, median and max_freq
mean_income <- mean(data$ingm_par, na.rm = TRUE)
median_income <- median(data$ingm_par, na.rm = TRUE)
min_value <- min(data$ingm_par)
max_value <- max(data$ingm_par)
bin_width <- 1000
breaks <- seq(min_value, max_value + bin_width, by = bin_width)
max_freq <- max(hist(data$ingm_par, breaks = breaks, plot = FALSE)$counts)

# Plot histogram
income_distribution_partner <- ggplot(data, aes(x = ingm_par)) +
  geom_histogram(binwidth = 1000, fill = "#e9e9e9", color = "#4d565e") +
  geom_vline(aes(xintercept = mean_income, color = "Mean"), linetype = "dashed", size = 0.5) +
  geom_vline(aes(xintercept = median_income, color = "Median"), linetype = "dashed", size = 0.5) +
  labs(x = "Monthly Income of Partner in Mexican Pesos $", y = "Absolute Frequency", title = "Distribution of Partner's Monthly Income in Mexican Pesos $") +
  scale_x_continuous(breaks = seq(min_value, max_value, by = 25000), labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(limits = c(0, max_freq), expand = c(0, 0), breaks = seq(0, max_freq, 1000), labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = c("Median" = "#620042", "Mean" = "#002b58"), labels = c("Mean", "Median"), guide = "none") +
  scale_linetype_manual(values = c("dashed", "dashed"), guide = guide_legend(override.aes = list(colour = c("#620042", "#002b58")))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1, 1),
        legend.box.just = "right")

# Save png
ggsave(paste0(path_save, "income_distribution_partner.png"), plot = income_distribution_partner)



### Set Outlier Boundaries ---------

# Age Woman
# Notes: Age <= 85 of interest
table(data$EDAD)
quantile(data$EDAD, probs = 0.01)
quantile(data$EDAD, probs = 0.99)
edad_max <- 85

# Age first child
# Notes: Age child <= 45
table(data$eda_hij)
quantile(data$eda_hij, probs = 0.01)
quantile(data$eda_hij, probs = 0.99)
eda_hij_max <- 45

# Age first sexual intercourse
# Notes: Age sex <= 35
table(data$eda_sex)
quantile(data$eda_sex, probs = 0.01)
quantile(data$eda_sex, probs = 0.99)
eda_sex_max <- 35

# Age marriage
# Notes: Age marriage <= 45
#        Age marriage >= 12
table(data$eda_mat)
quantile(data$eda_mat, probs = 0.01)
quantile(data$eda_mat, probs = 0.99)
eda_mat_max <- 45
eda_mat_min <- 12

# Number children
# Notes: Number children <= 15
table(data$num_hij)
quantile(data$num_hij, probs = 0.01)
quantile(data$num_hij, probs = 0.99)
num_hij_max <- 15

# Age partner
# Notes: Age partner <= 90
table(data$eda_par2)
quantile(data$eda_par2, probs = 0.01)
quantile(data$eda_par2, probs = 0.99)
eda_par2_max <- 90

# Household members per room
# Notes: Members dwelling <=7
table(data$hacin)
quantile(data$hacin, probs = 0.01)
quantile(data$hacin, probs = 0.99)
hacin_max <- 7

# Income per month woman
# Notes: income < 800000
table(data$ingm_muj)
ingm_muj_max <- 800000

# Income per month partner
# Notes: income < 250000
table(data$ingm_par)
ingm_par_max <- 250000


# Filer data ---------
# Data set: 38.971
data <- data %>%
  filter(EDAD <= edad_max) %>%
  filter(eda_hij <= eda_hij_max) %>%
  filter(eda_sex <= eda_sex_max) %>%
  filter(eda_mat >= eda_mat_min & eda_mat <= eda_mat_max) %>%
  filter(num_hij <= num_hij_max) %>%
  filter(eda_par2 <= eda_par2_max) %>%
  filter(hacin <= hacin_max) %>%
  filter(ingm_muj < ingm_muj_max)  %>%
  filter(ingm_par < ingm_par_max)

# Finalisation -----
# Data set: 26.889

# Check missings:
print(data.frame(Variable = names(data), 
                 Count = sapply(data, function(x) sum(is.na(x))), 
                 Percentage = sapply(data, function(x) sum(is.na(x)) / length(x) * 100)))

# Complete cases:
data <- data[complete.cases(data), ]

# Save data -----
# Final data: 26.889
step3_endireh <- data
save(step3_endireh, file = paste0(path_data_save,"step3_endireh.RData"))







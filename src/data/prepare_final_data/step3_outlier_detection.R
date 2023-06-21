########################### Step 3: Outlier Detection ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/imputed_pmm_m1/"

## Load data -------
load(paste0(path_data, "step2_endireh.RData"))

## Change data name -----
data <- step2_endireh

# Visualize Outliers ---------
# Initial data set: 62.774

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



# Set Outlier Boundaries ---------


# Age Woman
# Notes: Age < 90 of interest
edad_max <- 90

# Age first child
# Notes: Age child < 50
eda_hij_max <- 50

# Age first sexual intercourse
# Notes: Age sex <= 35
eda_sex <- 35

# Age marriage
# Notes:

# Number children
# Notes: Number children <= 15
num_hij_max <- 15

# Age partner
# Notes: 


# Household members per room
# Notes: Members dwelling <=7
hacin_max <- 7


# Save data ----------
# Data set: 62.774
save(step2_endireh, file = paste0(path_data,"step2_endireh.RData"))




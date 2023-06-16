#############################  Prepare Data ##################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(mice) # relevant for step 1
library(VIM) # relevant for step 1
library(naniar)


## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/"


## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set

# Data Preparation Process ------

## STEP 1: Remove NA Observations -----
# Initial data set: 63.152

# Calculate NAs of all variables
print(data.frame(Variable = names(endireh_2021), 
                 Count = sapply(endireh_2021, function(x) sum(is.na(x))), 
                 Percentage = sapply(endireh_2021, function(x) sum(is.na(x)) / length(x) * 100)))

# Remove risk factors with missings > 20 %
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

# Distribution of emotional ipv
# vio_emo_año:
table(endireh_2021$vio_emo_año)
#  no   yes 
# 48721 14431 
# ratio no/yes: 3.3

# vio_emo_vida
table(endireh_2021$vio_emo_vida)
#   no   yes 
# 42728 20424
# ratio no/yes: 2.1

# Keep only columns with missing data
endireh_missing <- endireh_2021 %>%
  select(where(~ any(is.na(.))))

# See percent of missings
gg_miss_var(endireh_missing)

# Pattern of missing data
md.pattern(endireh_missing)

# Plot missing data combinations
gg_miss_upset(endireh_missing, nsets =30, nintersects = 60, 
              sets.x.label = "Number of Missings by Variable", mainbar.y.label = "Number of Missings by Combination",
              mb.ratio = c(0.4, 0.6), shade.alpha = 0.5, matrix.color = "#002b58", sets.bar.color = "#002b58",
              main.bar.color = "#002b58")

# Check for MCAR (Missing Completly at Random)
mcar_df <- endireh_2021 %>% select(vio_inf_par, vio_exp_inf_par)
mcar_test(mcar_df)
marginplot(endireh_missing[c("vio_inf_par", "vio_exp_inf_par")])




# Keep complete cases
# Meaning: remove all observations with min. one missing value in the covariates 
emo_ipv_final <- endireh_2021[complete.cases(endireh_2021), ]

## STEP 2: Plausibility Analysis ---------
# Initial data set: 40.041

## vio_emo_año and vio_emo_vida
# Explanation: vio_emo_año may not be "yes" if vio_emo_vida is "no"
plaus_1a <- emo_ipv_final[emo_ipv_final$vio_emo_año_alt == "yes" & emo_ipv_final$vio_emo_vida == "no", ]
plaus_1b <- emo_ipv_final[emo_ipv_final$vio_emo_año == "yes" & !is.na(emo_ipv_final$vio_emo_año) & emo_ipv_final$vio_emo_vida == "no", ]

## eda_sex
# Explanation: Women’s age at first sexual intercourse cannot be greater than women’s age at the time of being surveyed
plaus_2 <- emo_ipv_final[!is.na(emo_ipv_final$eda_sex) & !is.na(emo_ipv_final$EDAD) & emo_ipv_final$eda_sex > emo_ipv_final$EDAD, ]

## eda_mat
# Explanation: Women’s age at first marriage (or at cohabitation) cannot be greater than women’s age at the time of being surveyed
plaus_3 <- emo_ipv_final[!is.na(emo_ipv_final$eda_mat) & !is.na(emo_ipv_final$EDAD) & emo_ipv_final$eda_mat > emo_ipv_final$EDAD, ]

## eda_hij
# Explanation: Women’s age at first childbirth cannot be greater than women’s age at the time of being surveyed
plaus_4 <- emo_ipv_final[!is.na(emo_ipv_final$eda_hij) & !is.na(emo_ipv_final$EDAD) & emo_ipv_final$eda_hij > emo_ipv_final$EDAD, ]

## ingm_muj and empleo_vida
plaus_5 <- emo_ipv_final[emo_ipv_final$ingm_muj > 0 & emo_ipv_final$empleo_vida == "no", ] # 94 implausible results
implausbile <- plaus_5$ID_PER

rm(plaus_1a, plaus_1b, plaus_2, plaus_3, plaus_4, plaus_5)

# Results: 94 implausible results

# Remove implausible observations
emo_ipv_final <- emo_ipv_final[!(emo_ipv_final$ID_PER %in% implausbile),]

## STEP 3: Outlier Detection --------
# Initial data set: 39.947

## Age Woman
summary(emo_ipv_final$EDAD)
hist(emo_ipv_final$EDAD, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_age <- ggplot(emo_ipv_final, aes(x = "", y = EDAD)) +
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

# Get outliers
summary(emo_ipv_final$EDAD %in% boxplot.stats(emo_ipv_final$EDAD)$out)
emo_ipv_final[emo_ipv_final$EDAD %in% boxplot.stats(emo_ipv_final$EDAD)$out, "EDAD"] <- NA # 28 NA's introduced 

## Age at childbirth
summary(emo_ipv_final$eda_hij)
hist(emo_ipv_final$eda_hij, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_age_children <- ggplot(emo_ipv_final, aes(x = "", y = eda_hij)) +
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

# Get outliers
summary(emo_ipv_final$eda_hij %in% boxplot.stats(emo_ipv_final$eda_hij)$out)
emo_ipv_final[emo_ipv_final$eda_hij %in% boxplot.stats(emo_ipv_final$eda_hij)$out, "eda_hij"] <- NA # 963 NA's introduced 

## Age at first sexual intercourse
summary(emo_ipv_final$eda_sex)
hist(emo_ipv_final$eda_sex, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_age_sex <- ggplot(emo_ipv_final, aes(x = "", y = eda_sex)) +
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

# Get outliers
summary(emo_ipv_final$eda_sex %in% boxplot.stats(emo_ipv_final$eda_sex)$out)
emo_ipv_final[emo_ipv_final$eda_sex %in% boxplot.stats(emo_ipv_final$eda_sex)$out, "eda_sex"] <- NA # 1898 NA's introduced 

## Age at marriage
summary(emo_ipv_final$eda_mat)
hist(emo_ipv_final$eda_mat, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_age_mat <- ggplot(emo_ipv_final, aes(x = "", y = eda_mat)) +
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

# Get outliers
summary(emo_ipv_final$eda_mat %in% boxplot.stats(emo_ipv_final$eda_mat)$out)
emo_ipv_final[emo_ipv_final$eda_mat %in% boxplot.stats(emo_ipv_final$eda_mat)$out, "eda_mat"] <- NA # 2380 NA's introduced 

## Age of partner
summary(emo_ipv_final$eda_par2)
hist(emo_ipv_final$eda_par2, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_age_partner <- ggplot(emo_ipv_final, aes(x = "", y = eda_par2)) +
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

# Get outliers
summary(emo_ipv_final$eda_par2 %in% boxplot.stats(emo_ipv_final$eda_par2)$out)
emo_ipv_final[emo_ipv_final$eda_par2 %in% boxplot.stats(emo_ipv_final$eda_par2)$out, "eda_par2"] <- NA # 67 NA's introduced 

## Average number of househod members in the dwelling
summary(emo_ipv_final$hacin)
table(emo_ipv_final$hacin)
hist(emo_ipv_final$hacin, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_hacin <- ggplot(emo_ipv_final, aes(x = "", y = hacin)) +
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

# Get outliers
summary(emo_ipv_final$hacin %in% boxplot.stats(emo_ipv_final$hacin)$out)
emo_ipv_final[emo_ipv_final$hacin %in% boxplot.stats(emo_ipv_final$hacin)$out, "hacin"] <- NA # 633 NA's introduced 

## Number of children 
summary(emo_ipv_final$num_hij)
table(emo_ipv_final$num_hij)
hist(emo_ipv_final$num_hij, breaks = nrow(emo_ipv_final))

# Create a boxplot with outliers marked
boxplot_num_hij <- ggplot(emo_ipv_final, aes(x = "", y = num_hij)) +
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

# Get outliers
emo_ipv_final <- emo_ipv_final %>%
  filter(num_hij <= 15)
summary(emo_ipv_final$num_hij %in% boxplot.stats(emo_ipv_final$num_hij)$out) #  4609 outliers

## Income per Month Woman

# Remove all observations with igmn_muj > 800.000$ (definitely outliers, > 42.000 Euros)
emo_ipv_final <- emo_ipv_final %>%
  filter(ingm_muj < 800000)

# Get summary of "ingm_muj"
table(emo_ipv_final$ingm_muj, useNA = "ifany")
summary(emo_ipv_final$ingm_muj)

# Save mean, median and max_freq
mean_income <- mean(emo_ipv_final$ingm_muj, na.rm = TRUE)
median_income <- median(emo_ipv_final$ingm_muj, na.rm = TRUE)
min_value <- min(emo_ipv_final$ingm_muj)
max_value <- max(emo_ipv_final$ingm_muj)
bin_width <- 1000
breaks <- seq(min_value, max_value + bin_width, by = bin_width)
max_freq <- max(hist(emo_ipv_final$ingm_muj, breaks = breaks, plot = FALSE)$counts)
                     
# Plot histogram
income_distribution_women <- ggplot(emo_ipv_final, aes(x = ingm_muj)) +
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
emo_ipv_final <- emo_ipv_final %>%
  filter(ingm_par < 999997) # 18 observations removed (from 39932 to 39914)

# Get summary of "ingm_par"
table(emo_ipv_final$ingm_par, useNA = "ifany")
summary(emo_ipv_final$ingm_par)

# Save mean, median and max_freq
mean_income <- mean(emo_ipv_final$ingm_par, na.rm = TRUE)
median_income <- median(emo_ipv_final$ingm_par, na.rm = TRUE)
min_value <- min(emo_ipv_final$ingm_par)
max_value <- max(emo_ipv_final$ingm_par)
bin_width <- 1000
breaks <- seq(min_value, max_value + bin_width, by = bin_width)
max_freq <- max(hist(emo_ipv_final$ingm_par, breaks = breaks, plot = FALSE)$counts)

# Plot histogram
income_distribution_partner <- ggplot(emo_ipv_final, aes(x = ingm_par)) +
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

## Finalisation -----
# Data set: 39.904

# Keep complete cases ----

# Check missings:
print(data.frame(Variable = names(emo_ipv_final), 
                 Count = sapply(emo_ipv_final, function(x) sum(is.na(x))), 
                 Percentage = sapply(emo_ipv_final, function(x) sum(is.na(x)) / length(x) * 100)))

# Complete cases:
emo_ipv_final <- emo_ipv_final[complete.cases(emo_ipv_final), ]

# Save data -----
# Final data: 34.742
save(emo_ipv_final, file = paste0(path,"emo_ipv_final.RData"))














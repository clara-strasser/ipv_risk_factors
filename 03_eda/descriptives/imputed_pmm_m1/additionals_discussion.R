############################ Additionals ################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggplotify)
library(grid)
library(scales)
library(xtable)
library(psych)
library(corrplot)
library(gganimate)
library(vcd)
library(vcdExtra)
library(rcompanion)

## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/prep_data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/imputed_pmm_m1/discussion/"

## Load data -------
load(paste0(path_data, "step3_endireh.RData"))

## Change data name -----
data <- step3_endireh

## Subset not needed -----
data <- data %>%
  select(-c("num_hij_par", "num_hij_par_muj"))

## Create vectors -----
individual_num <- c("EDAD","ingm_muj","num_hij", "eda_hij", "eda_sex", "eda_mat")
relationship_num <- c("eda_par2", "ingm_par", "hacin")
community_num <- c("mhr20", "fhr20", "ghr20", "phogjef_f", "pres_2020_f", 
                   "pres_2020_m", "gini20", "idh2020", "pea_f", "pea_m", "ParPolF")
society_num <- c("MasPrev", "FemPrev", "MasNoDen", "FemNoDen", "cor19", "satis19")

# Convert to numerical
data[individual_num] <- lapply(data[individual_num], as.numeric)
data[relationship_num] <- lapply(data[relationship_num], as.numeric)
data[community_num] <- lapply(data[community_num], as.numeric)
data[society_num] <- lapply(data[society_num], as.numeric)

# Additionals ------

## Age Difference -----

# Keep relevant variables
data$eda_dif <- data$EDAD - data$eda_sex

# Keep only the three columns: "EDAD," "eda_sex," and "eda_dif"
df <- data[, c("EDAD", "eda_sex", "eda_dif")]

# Histogram
dist <- ggplot(data, aes(x = eda_dif)) +
  geom_histogram(binwidth = 1, fill = "#002b58", color = "#4d565e") +
  labs(x = "Age Difference", y = "Absoulte Frequency") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(18, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1,1),
        legend.box.just = "right") 

# Save
ggsave(paste0(path_save, "age_difference_dist.png"), plot = dist)






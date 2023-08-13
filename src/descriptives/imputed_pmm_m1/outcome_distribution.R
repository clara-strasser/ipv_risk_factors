########################## Distribution Outcome Variable ###########################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats) 


## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/imputed_pmm_m1/outcome_var_dist/"

## Load data -------
load(paste0(path_data, "data_imp_pmm_m1.RData"))

## Change data name -----
data <- data_imp_pmm_m1

# Plot -----

## Variable "vio_emo_año" ----

# Calculate frequency counts
freq_table <- table(data$vio_emo_año, useNA = "ifany")

# Convert the table to a data frame for easier plotting
freq_df <- as.data.frame(freq_table)
freq_df$category <- rownames(freq_df)

# Re-code category values
freq_df$category <- recode(freq_df$category, "1" = "no", "2" = "yes", "3" = "NA")

# Add Freq percent
freq_df <- freq_df %>%
  mutate(Freq_per = (Freq * 100)/nrow(data))

# Re-order
freq_df$category <- fct_reorder(freq_df$category, freq_df$Freq_per, .desc = TRUE)


# Plotting
bar_chart_vio_emo_año <- ggplot(freq_df, aes(x = category, y = Freq_per, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("no" = "#002b58", "yes" = "#620042")) +
  labs(x = "Emotional Intimate Partner Violence Occurrence", y = "Relative Frequency (%)") +
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
        legend.box.just = "right") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  #geom_text(aes(label = paste0("n = ", Freq)), vjust = -0.5) +
  guides(fill = guide_legend(title = "Categories"))

# Save the ggplot as a PNG image
ggsave(paste0(path_save, "bar_chart_vio_emo_año.png"), plot = bar_chart_vio_emo_año)


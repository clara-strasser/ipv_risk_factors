#############################  Descriptives ##################################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/plots/descriptives/"

## Load data -------
load(paste0(path, "endireh_2021.RData")) # main data set


# Figures ----

## Bar Chart -----

## Variable "vio_emo_año"

# Calculate frequency counts
freq_table <- table(endireh_2021$vio_emo_año, useNA = "ifany")

# Convert the table to a data frame for easier plotting
freq_df <- as.data.frame(freq_table)
freq_df$category <- rownames(freq_df)

# Re-code category values
freq_df$category <- recode(freq_df$category, "1" = "no", "2" = "yes", "3" = "NA")

# Add Freq in percent
freq_df <- freq_df %>%
  mutate(Freq_per = (Freq * 100)/nrow(endireh_2021))

# Plot
bar_chart_vio_emo_año <- ggplot(freq_df, aes(x = reorder(category, Freq_per), y = Freq_per, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("no" = "#002b58", "yes" = "#620042", `NA` = "#4d565e")) +
  labs(x = "vio_emo_año", y = "Relative Frequency (%)") +
  ggtitle("Emotional IPV Occurrence in the Last Year (Alternative 1)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1,1),
        legend.box.just = "right") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n = ", Freq)), vjust = -0.5) +
  guides(fill = guide_legend(title = "Categories"))

# Save the ggplot as a PNG image
ggsave(paste0(path_save, "bar_chart_vio_emo_año.png"), plot = bar_chart_vio_emo_año)


## Variable "vio_emo_año_alt"

# Calculate frequency counts
freq_table <- table(endireh_2021$vio_emo_año_alt, useNA = "ifany")

# Convert the table to a data frame for easier plotting
freq_df <- as.data.frame(freq_table)
freq_df$category <- rownames(freq_df)

# Re-code category values
freq_df$category <- recode(freq_df$category, "1" = "no", "2" = "yes", "3" = "NA")

# Add Freq percent
freq_df <- freq_df %>%
  mutate(Freq_per = (Freq * 100)/nrow(endireh_2021))

# Plotting
bar_chart_vio_emo_año_alt <- ggplot(freq_df, aes(x = reorder(category, Freq_per), y = Freq_per, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("no" = "#002b58", "yes" = "#620042")) +
  labs(x = "vio_emo_año_alt", y = "Relative Frequency (%)") +
  ggtitle("Emotional IPV Occurrence in the Last Year (Alternative 2)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1,1),
        legend.box.just = "right") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n = ", Freq)), vjust = -0.5) +
  guides(fill = guide_legend(title = "Categories"))

# Save the ggplot as a PNG image
ggsave(paste0(path_save, "bar_chart_vio_emo_año_alt.png"), plot = bar_chart_vio_emo_año_alt)


## Variable "vio_emo_vida"

# Calculate frequency counts
freq_table <- table(endireh_2021$vio_emo_vida, useNA = "ifany")

# Convert the table to a data frame for easier plotting
freq_df <- as.data.frame(freq_table)
freq_df$category <- rownames(freq_df)

# Re-code category values
freq_df$category <- recode(freq_df$category, "1" = "no", "2" = "yes", "3" = "NA")

# Add Freq percent
freq_df <- freq_df %>%
  mutate(Freq_per = (Freq * 100)/nrow(endireh_2021))

# Plotting
bar_chart_vio_emo_vida <- ggplot(freq_df, aes(x = reorder(category, Freq_per), y = Freq_per, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("no" = "#002b58", "yes" = "#620042")) +
  labs(x = "vio_emo_vida", y = "Relative Frequency (%)") +
  ggtitle("Emotional IPV Occurrence Since Begin of Marriage/Cohabitation (Alternative 3)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1,1),
        legend.box.just = "right") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  geom_text(aes(label = paste0("n = ", Freq)), vjust = -0.5) +
  guides(fill = guide_legend(title = "Categories"))

# Save the ggplot as a PNG image
ggsave(paste0(path_save, "bar_chart_vio_emo_vida.png"), plot = bar_chart_vio_emo_vida)




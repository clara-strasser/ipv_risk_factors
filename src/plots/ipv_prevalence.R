########################### IPV Prevalence ##############################

# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(survey)
library(stringr)

## Load data -------
load("~/ipv_risk_factors/data/final_data/data_imp_pmm_m1.RData")
load("~/data_endireh/TVIV.RData")

## Rename data -----
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)

## Keep columns -------
data <- data %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "FAC_MUJ", "vio_emo_año"))

## Add relevant columns -----
# Thereof: UPM_DIS (TVIV), EST_DIS (TVIV)
data <- data %>%
  left_join(TVIV %>% select(c("UPM_DIS", "EST_DIS", "ID_VIV", "NOM_ENT")), by = ("ID_VIV"))

#data <- data %>%
  #left_join(TB_SEC_IVaVD %>% select(c("FAC_MUJ", "ID_PER")), by = c("ID_PER"))

## Modify for design
data$women <- ifelse((data$T_INSTRUM%in%c('A1','A2')), 1, 0) 
data$vio_emo_año_numeric <- as.numeric(data$vio_emo_año)
data <- data %>%
  mutate(vio_emo_año_numeric = ifelse(vio_emo_año_numeric == 2, 1,0))

# Prevalence -----

# Survey design
# Remarks:
# UPM_DIS: primary sampling units in the survey design
# EST_DIS: stratification variable
design <- svydesign(id=~UPM_DIS, strata=~EST_DIS, data=data, weights=~FAC_MUJ, nest=TRUE) 
design <- subset(design, subset = !(EST_DIS %in% c("0567", "0356", "0550"))) # Attention: data subset!

# Survey mean
svymean(~vio_emo_año_numeric, design=design)

# Prepare por results
prevalence <- vector(mode = "list")


# National Prevalence 
prevalence_national <- svyratio(~vio_emo_año_numeric, denominator=~women, design, na.rm = TRUE)  

# State Prevalence
prevalence_state  <- svyby(~vio_emo_año_numeric, denominator=~women, by=~NOM_ENT,
                        design, svyratio, na.rm = TRUE) 

# Modify
prevalence_state <- as.data.frame(prevalence_state)
colnames(prevalence_state)[2] <- "prevalence"
colnames(prevalence_state)[3] <- "se"
prevalence_state$NOM_ENT <- str_to_title(tolower(prevalence_state$NOM_ENT))
prevalence_state$NOM_ENT <- factor(prevalence_state$NOM_ENT, levels = prevalence_state$NOM_ENT[order(prevalence_state$prevalence, decreasing = TRUE)])

# Plot
prevalence_line_plot <- ggplot(prevalence_state, aes(x = NOM_ENT, y = prevalence)) +
  geom_vline(aes(xintercept = NOM_ENT), linetype = "dashed", color = "gray") +
  geom_point(color = "#620042") +
  geom_errorbar(aes(ymin = prevalence - se, ymax = prevalence + se), width = 0.2, color = "#620042") +
  labs(x = "State", y = "State Level Prevalence of Emotional IPV") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1, 1),
        legend.box.just = "right")

prevalence_line_plot
# Specify the directory to save the plots
save_directory <- "~/ipv_risk_factors/results/plots/"

# Save the plots
ggsave(plot = prevalence_line_plot, filename = "prevalence_line_plot.png", path = save_directory, width = 15, height = 8) # Save the plot file by copying it






















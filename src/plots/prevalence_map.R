######################### IPV Prevalence Mexico Map ##########################

# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(survey)
library(stringr)

## Set path ----
path1 <- "/Users/clara/Desktop/master_thesis/data/"
path2 <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Set save path --------
save_path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/results/plots/"

## Load data ----
load(paste0(path1,"bd_endireh_2021.RData")) # whole data set (for: survey design)
load(paste0(path2, "endireh_2021.RData")) # data subset (for: prevalence of subset)
shape_data <- st_read(dsn = paste0("/Users/clara/Desktop/master_thesis/data/marco_geoestadistico/889463770541_s/mg2022_integrado/conjunto_de_datos/00ent.shp"))

## Modify shape data -----
shapefile_df <- st_transform(shape_data, crs = st_crs(4326))

## Set variables -----
# P14_3<-paste0("P14_3_",1:38); P14_3[c(23,24,35:38)]<-paste0(P14_3[c(23,24,35:38)],"AB") # for total IPV in the last 12 months
P14_3 <- paste0("P14_3_", c(10:22, "23AB", "24AB")) # for emotional IPV in the last 12 months
variables <- c("ID_PER","UPM_DIS","EST_DIS","FAC_MUJ","CVE_ENT", "NOM_ENT","T_INSTRUM", P14_3) 

## Subset data ------
data <- TB_SEC_IVaVD[,variables] 

## Add factor variable -----
# data$vpar_12m_con <- ifelse(
  #apply(muj[, paste0("P14_3_", c(10:22, 25:34))], 1, function(row) any(row %in% c('1', '2', '3'))) |
    #muj$P14_3_23AB %in% c('1', '2', '3') | muj$P14_3_24AB %in% c('1', '2', '3') |
    #muj$P14_3_35AB %in% c('1', '2', '3') | muj$P14_3_36AB %in% c('1', '2', '3') |
    #muj$P14_3_37AB %in% c('1', '2', '3') | muj$P14_3_38AB %in% c('1', '2', '3'), 1, 0) # for total IPV
data$vpar_12m_con <- ifelse(
  apply(muj[, paste0("P14_3_", c(10:22))], 1, function(row) any(row %in% c('1', '2', '3'))) |
  muj$P14_3_23AB %in% c('1', '2', '3') | muj$P14_3_24AB %in% c('1', '2', '3'), 1, 0) # for emotional IPV
  

## Women subset -----
# All women >=15, cohabiting or married with a male partner and ith >=1 children have id=1
id_women <- unique(endireh_2021$ID_PER)
data <- data %>%
 mutate(par_12m = ifelse(ID_PER %in% id_women, 1, 0))

## Survey design -----
disenio <- svydesign(id= ~UPM_DIS, strata= ~EST_DIS, data= data, weights= ~FAC_MUJ, nest=TRUE) 

## Survey Ratio -----
n_vpar_lr_con <- svyratio(~vpar_12m_con, denominator=~par_12m, disenio, na.rm = TRUE)  


## Survey by Factor ------
e_vpar_lr_con <- svyby(~vpar_12m_con, denominator=~par_12m, by=~NOM_ENT, disenio,  svyratio, na.rm = TRUE)  

# Plots ---------

## Line Plot ------

# Prep
prevalence_state <- as.data.frame(e_vpar_lr_con)
colnames(prevalence_state)[2] <- "prevalence"
colnames(prevalence_state)[3] <- "se"
prevalence_state$NOM_ENT <- str_to_title(tolower(prevalence_state$NOM_ENT))
prevalence_state$NOM_ENT <- factor(prevalence_state$NOM_ENT, levels = prevalence_state$NOM_ENT[order(prevalence_state$prevalence, decreasing = TRUE)])

# Add confidence interval
z <- qnorm(0.95) # Z-score for 90% confidence interval
prevalence_state$Lower_CI <- prevalence_state$prevalence - z * prevalence_state$se
prevalence_state$Upper_CI <- prevalence_state$prevalence + z * prevalence_state$se


# Plot
prevalence_line_plot <- ggplot(prevalence_state, aes(x = NOM_ENT, y = prevalence)) +
  geom_vline(aes(xintercept = NOM_ENT), linetype = "dashed", color = "gray") +
  geom_point(color = "#620042") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#620042") +
  labs(x = "Federal State", y = "State Level Prevalence of Emotional IPV") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1, 1),
        legend.box.just = "right") +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), limits = c(0, 0.5)) 
ggsave(plot = prevalence_line_plot, filename = "prevalence_line_plot.png", path = save_path, width = 15, height = 8) # Save the plot file by copying it


## Map plot -----

# Get prevalence state again (for CVE_ENT)
e_vpar_lr_con <- svyby(~vpar_12m_con, denominator=~par_12m, by=~CVE_ENT, disenio,  svyratio, na.rm = TRUE)  

# Modify
prevalence_state <- as.data.frame(e_vpar_lr_con)  
colnames(prevalence_state)[2] <- "prevalence"
colnames(prevalence_state)[3] <- "se"

# Left join
prevalence <- shapefile_df %>%
  left_join(prevalence_state, by = c("CVE_ENT"))

# Plot
prevalence_map_plot <- ggplot() +
  geom_sf(data = prevalence, aes(fill = prevalence), color = "black") +
  geom_sf_label(data = prevalence, aes(label = CVE_ENT),
                fill = "violet", size = 2, nudge_y = 0.02, parse = TRUE) +
  scale_fill_gradient2("Emotional IPV Prevalence",high = "#1a0946", 
                       limits= c(0,0.5), 
                       breaks = c(0, 0.1, 0.2,0.3,0.4,0.5),
                       labels = c("0", "0.1", "0.2","0.3", "0.4", "0.5")) +
  theme_void()+
  theme(legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 13),
        legend.key.size = unit(15, "points"),
        legend.position = c(0.95, 0.95),  # Adjust the position as needed
        legend.justification = c(1, 1),
        legend.box.just = "right",
        axis.text = element_text(face = "bold"),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.title.x = element_blank(), # Remove x-axis title
        axis.title.y = element_blank())


# Save the plots -----
ggsave(plot = prevalence_map_plot, filename = "prevalence_map_plot.png", path = save_path, width = 15, height = 8) # Save the plot file by copying it






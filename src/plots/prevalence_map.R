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
prevalence_state <- as.data.frame(e_vpar_lr_con)
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





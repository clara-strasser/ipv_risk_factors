######################### IPV Observations Mexico Map ##########################


# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(survey)
library(stringr)

## Set path ----
path <- "/Users/clara/Desktop/master_thesis/data/"
path <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data ----
load(paste0(path,"bd_endireh_2021.RData"))
load(paste0(path, "endireh_2021.RData")) # main data set

## Set variables -----
P14_3 <- paste0("P14_3_", c(10:22, "23AB", "24AB"))
P14_3<-paste0("P14_3_",1:38); P14_3[c(23,24,35:38)]<-paste0(P14_3[c(23,24,35:38)],"AB") 
variables <- c("ID_PER","UPM_DIS","EST_DIS","FAC_MUJ","CVE_ENT","T_INSTRUM", "P13_C_1", P14_3) 

muj<-TB_SEC_IVaVD[,variables] 

muj <- muj %>%
  mutate(vio_emo_año = ifelse(rowSums(select(., any_of(P14_3)) <= "3", na.rm = TRUE) > 0, "2", "1")) %>%
  mutate(vio_emo_año = factor(vio_emo_año, levels = c(1, 2), labels = c("no", "yes")))
muj$vio_emo_año_numeric <- as.numeric(muj$vio_emo_año)
muj <- muj %>%
  mutate(vio_emo_año_numeric = ifelse(vio_emo_año_numeric == 2, 1,0))

muj$vpar_12m_con <- ifelse(
  apply(muj[, paste0("P14_3_", c(1:22, 25:34))], 1, function(row) any(row %in% c('1', '2', '3'))) |
    muj$P14_3_23AB %in% c('1', '2', '3') | muj$P14_3_24AB %in% c('1', '2', '3') |
    muj$P14_3_35AB %in% c('1', '2', '3') | muj$P14_3_36AB %in% c('1', '2', '3') |
    muj$P14_3_37AB %in% c('1', '2', '3') | muj$P14_3_38AB %in% c('1', '2', '3'), 1, 0)


muj$par_12m <- ifelse(((muj$T_INSTRUM%in%c('A1','A2','B1','B2')) | (!muj$P13_C_1%in%'3')), 1, 0) 
muj$par_lv <- ifelse(muj$T_INSTRUM %in% c('A1','A2','B1','B2'),1,0) 


# Women with >= 15, with at least one child and with a male partner
# Use data set: endireh_2021 (subset of whole data set)
id_per <- unique(endireh_2021$ID_PER)
muj <- muj %>%
  mutate(par_lv = ifelse(ID_PER %in% id_per,1,0))

disenio <- svydesign(id=~UPM_DIS, strata=~EST_DIS, data=muj, weights=~FAC_MUJ, nest=TRUE) 


n_vpar_lr_con <- svyratio(~vpar_12m_con, denominator=~par_12m, disenio, na.rm = TRUE)  
e_vpar_lr_con <- svyby(~vio_emo_año_numeric, denominator=~par_lv, by=~CVE_ENT, disenio,  svyratio, na.rm = TRUE)  

e_vpar_lr_con





## Set path -----
path_data <- "/Users/clara/Desktop/master_thesis/data/marco_geoestadistico/889463770541_s/mg2022_integrado/conjunto_de_datos/"

## Set save directory ----
save_directory <- "~/Desktop/master_thesis/r_projects/ipv_risk_factors/results/plots/"

## Load data -------
load("~/Desktop/master_thesis/r_projects/ipv_risk_factors/data/final_data/data_imp_pmm_m1.RData")
shape_data <- st_read(dsn = paste0(path_data,"00ent.shp"))

## Prep data -----
data <- data_imp_pmm_m1
rm(data_imp_pmm_m1)
  
# Re-project the shapefile to WGS84 (longitude and latitude in degrees)
shapefile_df <- st_transform(shape_data, crs = st_crs(4326))

# Modify data
counts_state <- data %>%
  group_by(CVE_ENT) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  mutate(total = sum(count))

# Combine data sets
merged_data <- merge(shapefile_df, counts_state, by = "CVE_ENT", all.x = TRUE)


# Plot -----

# Map with number of observations
observations_map <- ggplot() +
  geom_sf(data = merged_data, aes(fill = count), color = "black") +
  scale_fill_viridis(name = "Obervations", limits= c(0,max(merged_data$count)), 
                     breaks = c(0, 500, 1000, 1500, 2000, max(merged_data$count)),
                     labels = c("0", "500","1000","1500", "2000", ""),
                     option = "viridis", direction = -1) +
  theme_void() +
  guides(fill=guide_colorbar(title.vjust=2.5))


# Map with prevalence of emotional IPV
ggsave(plot = observations_map, filename = "observations_map.png", path = save_directory, width = 15, height = 8) # Save the plot file by copying it



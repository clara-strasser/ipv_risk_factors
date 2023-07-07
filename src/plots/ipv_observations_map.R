######################### IPV Observations Mexico Map ##########################


# Initiate -----

## Load packages ------
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(survey)
library(stringr)

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



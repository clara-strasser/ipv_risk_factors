###########################  Emotional IPV Outcome ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"

## Load data -------
load(paste0(path, "TB_SEC_IVaVD.RData"))


# Emotional IPV --------

# Outcome definition:
# Prevalence of total intimate partner violence (IPV)
# against married or cohabiting women aged 15 years and older, with at least one child,
# in the last 12 months
# Variable name: "vio_emo_año"

# Questions:
# P14_3_10 - P14_3_22, P14_3_23AB and P14_3_24AB
# De octubre de 2020 a la fecha, ¿esto ha ocurrido…
# la ha avergonzado, ofendido, menospreciado o humillado (le ha dicho que es fea o la ha comparado con otras mujeres)
# la ha ignorado, no la toma en cuenta o no le brinda cariño
# le ha dicho que usted lo engaña
# le ha hecho sentir miedo
# la ha amenazado con dejarla/abandonarla, dañarla, quitarle a los(as) hijos(as) o correrla de la casa
# la ha encerrado, le ha prohibido salir o que la visiten
# la ha vigilado, espiado, la ha seguido cuandosale de su casa o se le aparece de manera sorpresiva
# la llama o le manda mensajes por teléfono todo el tiempo, para saber dónde y con quién está y qué está haciendo
# la ha amenazado con algún arma (cuchillo, navaja, pistola o rifle) o con quemarla
# la ha amenazado con matarla, matarse él o matar a los niños(as)
# le ha destruido, tirado o escondido cosas de usted o del hogar
# le ha dejado de hablar
# le revisa su correo o celular y le exige que le dé las contraseñas
# ha hecho que los hijos(as) o parientes se pongan en su contra
# se ha enojado mucho porque no está listo el quehacer, porque la comida no está como él quiere o cree que usted no cumplió con sus obligaciones
# Outcomes:
#             1 - muchas veces (yes)
#             2 - pocas veces (yes)
#             3 - una vez (yes)
#             4 - no ocurrio (no)
#             9 - not specified (NA)
#             b - blank (NA)
# Level:
# no (1), yes (2)
# Aim: create variable "vio_emo_año"

# Set columns
vio_emo <- paste0("P14_3_", c(10:22, "23AB", "24AB"))

# Reduce data frame
vio_emo_df <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", vio_emo))

# Summary stat:
colSums(is.na(vio_emo_df)) # max of 101708 NAs
sapply(vio_emo_df, unique) # NA, 1, 2, 3, 4, 9
head(vio_emo_df[, c(vio_emo)], n = 35)

# Create variable "vio_emo_año"
vio_emo_df <- vio_emo_df %>%
  mutate(vio_emo_año = ifelse(rowSums(select(., any_of(vio_emo)) <= "3", na.rm = TRUE) > 0, "1", 
                              ifelse(rowSums(select(., any_of(vio_emo)) == "4", na.rm = TRUE) > 0, "2", NA_character_))) %>%
  mutate(vio_emo_año = factor(vio_emo_año, levels = c("2", "1"), labels = c("no", "yes")))

# Create variable "vio_emo_año_alt"
# Remark: NAs are converted to 1 ("no)
vio_emo_df <- vio_emo_df %>%
  mutate(vio_emo_año_alt = ifelse(rowSums(select(., any_of(vio_emo)) <= "3", na.rm = TRUE) > 0, "1", "2")) %>%
  mutate(vio_emo_año_alt = factor(vio_emo_año_alt, levels = c("2", "1"), labels = c("no", "yes")))

# Summary stat "vio_emo_año":
head(vio_emo_df[, c(vio_emo, "vio_emo_año")], n = 65)
sum(is.na(vio_emo_df$vio_emo_año)) # 73395 NAs
sum(is.na(vio_emo_df$vio_emo_año) & (vio_emo_df$T_INSTRUM == "A1" | vio_emo_df$T_INSTRUM == "A2")) # 46755 NAs
sum((vio_emo_df$T_INSTRUM == "A1" | vio_emo_df$T_INSTRUM == "A2")) # 68540 Number (for P3_1 it is 68574 Number)
# Remark: only 21.785 obervations for "vio_emo_año" not NA and married/cohabiting women


# Summary stat "vio_emo_año_alt":
head(vio_emo_df[, c(vio_emo, "vio_emo_año_alt")], n = 65)
sum(is.na(vio_emo_df$vio_emo_año_alt)) # 0 NAs
sum(is.na(vio_emo_df$vio_emo_año_alt) & (vio_emo_df$T_INSTRUM == "A1" | vio_emo_df$T_INSTRUM == "A2")) # 0 NAs
sum((vio_emo_df$T_INSTRUM == "A1" | vio_emo_df$T_INSTRUM == "A2")) # 68540 Number (for P3_1 it is 68574 Number)
# Remark: 0 NAs as NAs were converted to "no"

# Subset data set
vio_emo_df <- vio_emo_df %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "vio_emo_año", "vio_emo_año_alt"))

# Finalize ------

## Set path -----
path_outcome <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"


## Save data -----
emotional_ipv <- vio_emo_df
save(emotional_ipv, file = paste0(path_outcome,"emotional_ipv.RData"))









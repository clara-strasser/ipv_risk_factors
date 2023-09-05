###########################  Factor Analysis ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)
library(corrplot)
library(psych)
library(GPArotation)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load data -------
load(paste0(path, "TB_SEC_IVaVD.RData")) # all women
load(paste0(path_data, "endireh_2021.RData")) # subset women (married/cohabiting, male partner, >= 1 child)

## Keep relevant observations -----
ids <- endireh_2021$ID_PER

## Subset -----

# Set columns
vio_emo <- paste0("P14_3_", c(10:22, "23AB", "24AB"))

# Reduce data frame
data <- TB_SEC_IVaVD %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", vio_emo))

# Subset
data <- data[data$ID_PER %in% ids,]

# Remove df
rm(TB_SEC_IVaVD)
rm(endireh_2021)
rm(ids)

# Manipulate -----

# Convert NAs to 0
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), ~ifelse(is.na(.), 4, .))

# Convert character variables to factors
# 1 = many times
# 2 = a few times
# 3 = once
# 4 = no
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), ~factor(.x, levels = c("1", "2", "3", "4")))

# Correlation Plot ------

# Convert columns to numeric
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), as.numeric)

# Plot
corr <- cor(na.omit(data[, vio_emo]))
corrplot(corr)


# Factor Analysis ------

# Remove columns
factor_data <- data %>%
  select(-c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

## Set optimal number of factors -----

# 1) Parallel Analysis
# Results: 3
fa.parallel(factor_data, fa = "pc") # use principal component

# 2) Eigenvalues, Criterium of Kaiser
# Results: 3
ev <- eigen(cor(na.omit(factor_data)))
ev$values

# 3) Scree Plot
# Results: 3
scree(factor_data, pc = FALSE)

# Overall result: three factors seem reasonable

# Factor analysis with 2 factors, oblique rotation, estimation with maximum-likelihood
f <- fa(factor_data, nfactors = 3, rotate = "oblimin")
fa.organize(f)
fa.diagram(f, main = "Factor analysis with three factors and ML estimation")












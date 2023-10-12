###########################  Factor Analysis ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(Hmisc)
library(purrr)
library(lavaan)
library(corrplot)
library(psych)
library(GPArotation)
library(xtable)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"

## Load function ----
source("/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/functions/fa_table.R")

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

# Convert NAs to 4
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), ~ifelse(is.na(.), 4, .))

# Convert character variables to factors
# 1 = many times
# 2 = a few times
# 3 = once
# 4 = no
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), ~factor(.x, levels = c("1", "2", "3", "4")))

# Convert columns to numeric
data <- data %>%
  mutate_at(vars(all_of(vio_emo)), as.numeric)

# Remove rows with NAs
data <- na.omit(data)

# Correlation Plot ------

# Plot
corr <- cor(na.omit(data[, vio_emo]))
corrplot(corr)


# Exploratory Factor Analysis (EFA) ------

# Remove columns
factor_data <- data %>%
  select(-c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM"))

# Randomly split data in 80:20
set.seed(123)
factor_data$id <- 1:nrow(factor_data)
train <- factor_data %>% 
  sample_frac(0.80)
test  <- dplyr::anti_join(factor_data, train, by = "id") %>%
  select(-c("id"))
train <- train %>%
  select(-c("id"))

## Set optimal number of factors -----

# 1) Parallel Analysis
# Results: 3
fa.parallel(train, fa = "fa", fm ="ml", ylabel = "Factor Analysis Eigenvalues", main = "Parallel Analysis Scree Plot") # use principal component

# 2) Eigenvalues, Criterium of Kaiser
# Results: 3
ev <- eigen(cor(na.omit(train)))
ev$values

# 3) Scree Plot
# Results: 3
scree(train, pc = FALSE)

# Overall result: three factors seem reasonable

# Factor analysis with 2 factors, oblique rotation, estimation with maximum-likelihood
f_train_1 <- fa(train, nfactors = 1, rotate = "oblimin", fm = "ml")
fa.organize(f_train_1)
fa.diagram(f_train_1, main = "Factor analysis with one factor and ML estimation")

# Factor analysis with 2 factors, oblique rotation, estimation with maximum-likelihood
f_train_2 <- fa(train, nfactors = 2, rotate = "oblimin", fm = "ml")
fa.organize(f_train_2)
fa.diagram(f_train_2, main = "Factor analysis with three factors and ML estimation")

# Factor analysis with 2 factors, oblique rotation, estimation with maximum-likelihood
f_train_3 <- fa(train, nfactors = 3, rotate = "oblimin", fm = "ml")
fa.organize(f_train_3)
fa.diagram(f_train_3, labels = TRUE, main = "Factor analysis with three factors and ML estimation")

# Confirmatory Factor Analysis (CFA) ------

## One-Factor Model ------

# Model
one_fac_model <- "psych_vio =~ P14_3_10 + P14_3_11 + P14_3_12 +
P14_3_13 + P14_3_14 + P14_3_15 + P14_3_16 + P14_3_17 + P14_3_18 + 
P14_3_19 + P14_3_20 + P14_3_21 + P14_3_22 + P14_3_23AB + P14_3_24AB"

# Fit
fit_one <- cfa(one_fac_model, data = test)

# Results
summary(fit_one, fit.measures = TRUE)

## Two-Factor Model ------
two_fac_model <- "emo_abuse =~ P14_3_10 + P14_3_11 + P14_3_13 + P14_3_14 + P14_3_23AB + P14_3_24AB + P14_3_12 + P14_3_16 + P14_3_17 + P14_3_22
                 threats =~ P14_3_15 + P14_3_18 + P14_3_19 + P14_3_20"

# Fit
fit_two <- cfa(two_fac_model, data = test)

# Results
summary(fit_two, fit.measures = TRUE)

## Three-Factor Model ------
three_fac_model <- "emo_abuse =~ P14_3_10 + P14_3_11 + P14_3_13 + P14_3_14 + P14_3_23AB + P14_3_24AB
                 threats =~ P14_3_15 + P14_3_18 + P14_3_19 + P14_3_20
                  surv =~ P14_3_12 + P14_3_16 + P14_3_17 + P14_3_22"

# Fit
fit_three <- cfa(three_fac_model, estimator = "ML", std.lv=TRUE, data = test)

# Results
summary(fit_three, fit.measures = TRUE)

# Plots and Tables ------

## Factor Loading table for three-factor model -----
tables <- fa_table(f_train_3, 0.3)
tables_latex <- xtable(tables, include.rownames=FALSE)
tables_latex





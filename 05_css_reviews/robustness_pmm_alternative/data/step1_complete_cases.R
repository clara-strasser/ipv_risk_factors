########################### Step 1: Complete Cases ###########################

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(mice) # relevant for step 1
library(VIM) # relevant for step 1
library(naniar)

## Load functions --------------------------------------------------------------
source("src/propplot.R")

## Set path --------------------------------------------------------------------
path <- "/Users/clarastrasser/ipv_data/data/"
path_save <- "/Users/clarastrasser/ipv_data/data/final_data/rob_pmm_alt/"


## Load data -------------------------------------------------------------------
load(paste0(path, "endireh_2021.RData")) # main data set

# Data Preparation Process -----------------------------------------------------

## Prepare data -----

# Convert characters to numeric
endireh_2021 <- endireh_2021 %>%
  mutate(pres_2020_f = as.numeric(pres_2020_f),
         pres_2020_m = as.numeric(pres_2020_m),
         gini20 = as.numeric(gini20),
         idh2020 = as.numeric(idh2020),
         pea_f = as.numeric(pea_f),
         pea_m = as.numeric(pea_m),
         MasPrev = as.numeric(MasPrev),
         FemPrev = as.numeric(FemPrev))

# Remove unnecessary risk factors 
# edu_parlow
# edu_parmedium
# edu_parhigh        
# ind_par 
# sep_ex 
# vio_fis_ex 
# vio_emo_ex 
# vio_sex_ex  
# vio_eco_ex  
# ing_par
# ing_muj
endireh_2021 <- endireh_2021 %>%
  select(-c("edu_parlow", "edu_parmedium", "edu_parhigh", "ind_par", 
            "sep_ex", "vio_fis_ex", "vio_emo_ex", "vio_sex_ex", "vio_eco_ex", "ing_par", "ing_muj"))

## Initial Descriptives -----
# Initial data set: 63.152

# Calculate NAs of all variables
print(data.frame(Variable = names(endireh_2021), 
                 Count = sapply(endireh_2021, function(x) sum(is.na(x))), 
                 Percentage = sapply(endireh_2021, function(x) sum(is.na(x)) / length(x) * 100)))

# Keep only columns with missing data
endireh_missing <- endireh_2021 %>%
  select(where(~ any(is.na(.))))

# See percent of missings
gg_miss_var(endireh_missing)

# Pattern of missing data
missing_pattern <- md.pattern(endireh_missing)

# Plot missing data combinations
gg_miss_upset(endireh_missing, nsets =30, nintersects = 60, 
              sets.x.label = "Number of Missings by Variable", mainbar.y.label = "Number of Missings by Combination",
              mb.ratio = c(0.4, 0.6), shade.alpha = 0.5, matrix.color = "#002b58", sets.bar.color = "#002b58",
              main.bar.color = "#002b58")


## Missing Data Imputation ------
# Remark: Conduct iterative constrained imputation (sometimes called rejection sampling within PMM)
# 1. Impute everything with PMM
# 2. Find which imputed values are implausible
# 3. Set those back to NA in the data (leaving the plausibly-imputed values in place)
# 4. Re-impute only the remaining NAs
# 5. Repeat until zero implausible imputed values remain

# Remove multi-collinear columns
raw_df <- endireh_2021 %>%
  select(-c("ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM")) %>%
  select(-ends_with("high")) %>%
  select(-ends_with("both")) %>%
  select(-ends_with("comhigh_urban"))

# Precompute once: original NA positions (logical matrix, fixed across iterations)
orig_na       <- is.na(raw_df)
constr_vars   <- c("EDAD", "eda_sex", "eda_mat", "eda_hij", "ingm_muj")

# Per-variable original-NA masks for the constraint checks
was_na <- setNames(lapply(constr_vars, function(v) orig_na[, v]), constr_vars)

current_df    <- raw_df
max_iter      <- 50
iter          <- 0
n_implausible <- Inf

while (n_implausible > 0 && iter < max_iter) {
  iter <- iter + 1

  completed_iter <- complete(
    mice(current_df, m = 1, maxit = 5, meth = "pmm",
         seed = 800 + iter, printFlag = FALSE),
    1L
  )

  # Lock in plausible imputed values: fill all originally-NA positions in current_df
  # so that plausible draws become "observed" in the next iteration
  for (v in names(current_df)[colSums(orig_na) > 0]) {
    current_df[[v]][orig_na[, v]] <- completed_iter[[v]][orig_na[, v]]
  }

  # Detect violations (trigger if at least one variable in the pair was originally NA)
  viol_sex_edad <- (was_na$eda_sex | was_na$EDAD) & completed_iter$eda_sex > completed_iter$EDAD
  viol_mat_edad <- (was_na$eda_mat | was_na$EDAD) & completed_iter$eda_mat > completed_iter$EDAD
  viol_hij_edad <- (was_na$eda_hij | was_na$EDAD) & completed_iter$eda_hij > completed_iter$EDAD
  viol_sex_hij  <- (was_na$eda_sex | was_na$eda_hij) & completed_iter$eda_sex > completed_iter$eda_hij
  viol_ingm     <- was_na$ingm_muj & completed_iter$ingm_muj > 0 & completed_iter$empleo_vida == "no"

  # Reset only the originally-missing variable in each violating pair back to NA
  current_df$EDAD[viol_sex_edad    & was_na$EDAD]    <- NA
  current_df$eda_sex[viol_sex_edad & was_na$eda_sex] <- NA
  current_df$EDAD[viol_mat_edad    & was_na$EDAD]    <- NA
  current_df$eda_mat[viol_mat_edad & was_na$eda_mat] <- NA
  current_df$EDAD[viol_hij_edad    & was_na$EDAD]    <- NA
  current_df$eda_hij[viol_hij_edad & was_na$eda_hij] <- NA
  current_df$eda_sex[viol_sex_hij  & was_na$eda_sex] <- NA
  current_df$eda_hij[viol_sex_hij  & was_na$eda_hij] <- NA
  current_df$ingm_muj[viol_ingm]                     <- NA

  # Count remaining NAs in constrained variables — decreases monotonically
  n_implausible <- sum(is.na(current_df[, constr_vars]))
  cat(sprintf("Iteration %d: %d constrained value(s) still to re-impute\n",
              iter, n_implausible))
}

if (iter == max_iter && n_implausible > 0)
  warning(sprintf("Max iterations (%d) reached; %d implausible value(s) remain.",
                  max_iter, n_implausible))

cat(sprintf("\nDone in %d iteration(s).\n", iter))

# Merge imputed values back into the full endireh_2021 (preserves all columns)
step1_endireh <- endireh_2021 %>%
  mutate(across(everything(), ~ coalesce(., completed_iter[[cur_column()]])))

# Derive *gradhigh and act_distboth from imputed gradmedium / gradlow columns
step1_endireh <- step1_endireh %>%
  mutate(lib_sex_gradhigh = factor(ifelse(lib_sex_gradmedium == "no" & lib_sex_gradlow == "no", 2, 1),
                                   levels = c(1, 2), labels = c("no", "yes"))) %>%
  mutate(lib_eco_gradhigh = factor(ifelse(lib_eco_gradmedium == "no" & lib_eco_gradlow == "no", 2, 1),
                                   levels = c(1, 2), labels = c("no", "yes"))) %>%
  mutate(lib_soc_gradhigh = factor(ifelse(lib_soc_gradmedium == "no" & lib_soc_gradlow == "no", 2, 1),
                                   levels = c(1, 2), labels = c("no", "yes"))) %>%
  mutate(act_distboth = factor(ifelse(act_distfemales == "no" & act_distmales == "no", 2, 1),
                               levels = c(1, 2), labels = c("no", "yes")))

# REMARK: RUN ON CLUSTER
# step1_endireh_pmm_alt


save(step1_endireh_pmm_alt, file = paste0(path_save,"step1_endireh.RData"))

## Compare step1_endireh (standard PMM) vs step1_endireh_pmm_alt (iterative) ---

# Variables that had missing values in the original data
imp_vars_all  <- names(endireh_2021)[colSums(is.na(endireh_2021)) > 0]
imp_vars_num  <- imp_vars_all[sapply(endireh_2021[, imp_vars_all], is.numeric)]
imp_vars_fac  <- imp_vars_all[sapply(endireh_2021[, imp_vars_all], is.factor)]

### 1. Plausibility violations ---------------------------------------------------

check_plaus <- function(df) {
  c(
    eda_sex_gt_EDAD  = sum(!is.na(df$eda_sex)  & !is.na(df$EDAD)    & df$eda_sex  > df$EDAD,    na.rm = TRUE),
    eda_mat_gt_EDAD  = sum(!is.na(df$eda_mat)  & !is.na(df$EDAD)    & df$eda_mat  > df$EDAD,    na.rm = TRUE),
    eda_hij_gt_EDAD  = sum(!is.na(df$eda_hij)  & !is.na(df$EDAD)    & df$eda_hij  > df$EDAD,    na.rm = TRUE),
    eda_sex_gt_hij   = sum(!is.na(df$eda_sex)  & !is.na(df$eda_hij) & df$eda_sex  > df$eda_hij, na.rm = TRUE),
    ingm_no_work     = sum(!is.na(df$ingm_muj) & !is.na(df$empleo_vida) &
                             df$ingm_muj > 0   & df$empleo_vida == "no", na.rm = TRUE)
  )
}

cat("\n--- Plausibility violations ---\n")
print(data.frame(
  Check    = c("eda_sex > EDAD", "eda_mat > EDAD", "eda_hij > EDAD",
               "eda_sex > eda_hij", "ingm_muj > 0 & never worked"),
  PMM_std  = check_plaus(step1_endireh),
  PMM_iter = check_plaus(step1_endireh_pmm_alt),
  row.names = NULL
))

### 2. Distribution of imputed values vs observed (numeric) ----------------------

orig_na_full <- is.na(endireh_2021)  # NA mask over the full dataset

compare_df <- do.call(rbind, lapply(imp_vars_num, function(v) {
  if (!v %in% names(step1_endireh) || !v %in% names(step1_endireh_pmm_alt)) return(NULL)
  was_missing <- orig_na_full[, v]
  if (!any(was_missing)) return(NULL)
  rbind(
    data.frame(variable = v, method = "PMM standard",
               value = step1_endireh[[v]][was_missing]),
    data.frame(variable = v, method = "PMM iterative",
               value = step1_endireh_pmm_alt[[v]][was_missing]),
    data.frame(variable = v, method = "Observed (non-NA)",
               value = endireh_2021[[v]][!was_missing])
  )
}))

ggplot(compare_df, aes(x = value, colour = method, fill = method)) +
  geom_density(alpha = 0.15) +
  facet_wrap(~ variable, scales = "free") +
  scale_colour_manual(values = c("PMM standard"     = "#0072B2",
                                 "PMM iterative"    = "#D55E00",
                                 "Observed (non-NA)"= "#009E73")) +
  scale_fill_manual(values   = c("PMM standard"     = "#0072B2",
                                 "PMM iterative"    = "#D55E00",
                                 "Observed (non-NA)"= "#009E73")) +
  labs(title = "Distribution of imputed vs observed values",
       x = "Value", y = "Density", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10),
        axis.text  = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.position = "bottom")

### 3. Constrained variables: side-by-side summary stats -------------------------

cat("\n--- Summary of constrained variables: observed (non-NA) ---\n")
print(summary(endireh_2021[, intersect(constr_vars, names(endireh_2021))]))

cat("\n--- Summary of constrained variables: standard PMM ---\n")
print(summary(step1_endireh[, constr_vars]))

cat("\n--- Summary of constrained variables: iterative PMM ---\n")
print(summary(step1_endireh_pmm_alt[, constr_vars]))

### 4. Correlation structure (numeric imputed variables) -------------------------

frob_diff <- function(df1, df2, vars) {
  c1 <- cor(df1[, vars], use = "pairwise.complete.obs")
  c2 <- cor(df2[, vars], use = "pairwise.complete.obs")
  norm(c1 - c2, type = "F")
}

num_imp_in_both <- imp_vars_num[imp_vars_num %in% names(step1_endireh) &
                                  imp_vars_num %in% names(step1_endireh_pmm_alt)]

# Correlation matrix of observed data (complete cases only, as reference)
cor_obs <- cor(endireh_2021[, num_imp_in_both], use = "pairwise.complete.obs")
frob_vs_obs <- function(df) norm(cor_obs - cor(df[, num_imp_in_both], use = "pairwise.complete.obs"), type = "F")

cat("\n--- Frobenius norm vs observed correlation matrix ---\n")
print(data.frame(
  PMM_std_vs_obs  = round(frob_vs_obs(step1_endireh),        4),
  PMM_iter_vs_obs = round(frob_vs_obs(step1_endireh_pmm_alt), 4),
  PMM_std_vs_iter = round(frob_diff(step1_endireh, step1_endireh_pmm_alt, num_imp_in_both), 4)
))

### 5. Factor variable proportions (imputed rows vs observed) --------------------

cat("\n--- Factor proportions: observed (non-NA) vs standard PMM vs iterative PMM ---\n")
for (v in imp_vars_fac) {
  if (!v %in% names(step1_endireh) || !v %in% names(step1_endireh_pmm_alt)) next
  was_missing <- orig_na_full[, v]
  if (!any(was_missing)) next
  cat(sprintf("\n%s  (n imputed = %d, n observed = %d):\n",
              v, sum(was_missing), sum(!was_missing)))
  tbl <- rbind(
    Observed = prop.table(table(endireh_2021[[v]][!was_missing])),
    PMM_std  = prop.table(table(step1_endireh[[v]][was_missing])),
    PMM_iter = prop.table(table(step1_endireh_pmm_alt[[v]][was_missing]))
  )
  print(round(tbl, 3))
}










# ### Variant 1: Predictive Mean Matching -----
# 
# # Remove multi-collinear columns
# step1_df <- endireh_2021 %>%
#   select(-c("ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM")) %>% # remove as constant
#   select(-ends_with("high")) %>%  # remove
#   select(-ends_with("both")) %>% # remove
#   select(-ends_with("comhigh_urban")) # remove
# 
# # Use "predictive mean matching" (pmm) to impute missing values
# step1_df <- mice(step1_df, m=1, maxit = 5, meth = "pmm", seed=800)
# 
# # Create complete data set
# step1_completed <- complete(step1_df, 1)
# 
# # Test Tabular
# summary(endireh_2021)
# summary(step1_completed)
# 
# # Test Graph and save
# densityplot(step1_df) # for continuous
# propplot(step1_df) # for factors
# 
# 
# # Create complete data set
# step1_endireh <- endireh_2021 %>%
#   mutate(across(everything(), ~ coalesce(., step1_completed[[cur_column()]])))
# 
# # Replace NAs in factor variables
# step1_endireh <- step1_endireh %>%
#   mutate(lib_sex_gradhigh = factor(ifelse(lib_sex_gradmedium == "no" & lib_sex_gradlow == "no", 2, 1),
#                                    levels = c(1, 2),
#                                    labels = c("no", "yes"))) %>%
#   mutate(lib_eco_gradhigh = factor(ifelse(lib_eco_gradmedium == "no" & lib_eco_gradlow == "no", 2, 1),
#                                    levels = c(1, 2),
#                                    labels = c("no", "yes"))) %>%
#   mutate(lib_soc_gradhigh = factor(ifelse(lib_soc_gradmedium == "no" & lib_soc_gradlow == "no", 2, 1),
#                                    levels = c(1, 2),
#                                    labels = c("no", "yes"))) %>%
#   mutate(act_distboth = factor(ifelse(act_distfemales == "no" & act_distmales == "no", 2, 1),
#                                levels = c(1, 2),
#                                labels = c("no", "yes")))

# Examples:
# 0206256.01.1.02
# 2360033.02.1.02
# 3202611.06.1.02

# Save data
#save(step1_endireh, file = paste0(path_save,"step1_endireh.RData"))

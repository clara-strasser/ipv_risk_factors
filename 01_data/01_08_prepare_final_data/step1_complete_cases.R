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
path_save <- "/Users/clarastrasser/ipv_data/data/prep_data/"


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

### Variant 1: Predictive Mean Matching -----

# Remove multi-collinear columns
step1_df <- endireh_2021 %>%
  select(-c("ID_VIV", "CVE_ENT", "CVE_MUN", "T_INSTRUM")) %>% # remove as constant
  select(-ends_with("high")) %>%  # remove
  select(-ends_with("both")) %>% # remove
  select(-ends_with("comhigh_urban")) # remove

# Use "predictive mean matching" (pmm) to impute missing values
step1_df <- mice(step1_df, m=1, maxit = 5, meth = "pmm", seed=800)

# Create complete data set
step1_completed <- complete(step1_df, 1)

# Test Tabular
summary(endireh_2021)
summary(step1_completed)

# Test Graph and save
densityplot(step1_df) # for continuous
propplot(step1_df) # for factors


# Create complete data set
step1_endireh <- endireh_2021 %>%
  mutate(across(everything(), ~ coalesce(., step1_completed[[cur_column()]])))

# Replace NAs in factor variables
step1_endireh <- step1_endireh %>%
  mutate(lib_sex_gradhigh = factor(ifelse(lib_sex_gradmedium == "no" & lib_sex_gradlow == "no", 2, 1),
                                   levels = c(1, 2),
                                   labels = c("no", "yes"))) %>%
  mutate(lib_eco_gradhigh = factor(ifelse(lib_eco_gradmedium == "no" & lib_eco_gradlow == "no", 2, 1),
                                   levels = c(1, 2),
                                   labels = c("no", "yes"))) %>%
  mutate(lib_soc_gradhigh = factor(ifelse(lib_soc_gradmedium == "no" & lib_soc_gradlow == "no", 2, 1),
                                   levels = c(1, 2),
                                   labels = c("no", "yes"))) %>%
  mutate(act_distboth = factor(ifelse(act_distfemales == "no" & act_distmales == "no", 2, 1),
                                 levels = c(1, 2),
                                 labels = c("no", "yes")))

# Examples:
# 0206256.01.1.02
# 2360033.02.1.02
# 3202611.06.1.02

# Save data
#save(step1_endireh, file = paste0(path_save,"step1_endireh.RData"))


## Alternative Imputation Methods -----------------------------------------------

# Original pre-imputation data (stored inside the mids object)
raw_df <- step1_df$data

### Variant 2: CART (Classification and Regression Trees) -----
step1_cart <- mice(raw_df, m = 1, maxit = 5, meth = "cart", seed = 800)
step1_cart_completed <- complete(step1_cart, 1)

### Variant 3: Random Forest -----
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
step1_rf <- mice(raw_df, m = 1, maxit = 5, meth = "rf", seed = 800)
step1_rf_completed <- complete(step1_rf, 1)

## Compare imputed distributions ------------------------------------------------

# Variables flagged in plausibility analysis
focus_vars <- c("eda_sex", "eda_mat", "eda_hij", "ingm_muj")

pmm_completed <- complete(step1_df, 1)

compare_df <- do.call(rbind, lapply(focus_vars, function(v) {
  if (!v %in% names(raw_df)) return(NULL)
  was_missing <- is.na(raw_df[[v]])
  if (!any(was_missing)) return(NULL)
  data.frame(
    variable = v,
    method   = rep(c("PMM", "CART", "RF"), each = sum(was_missing)),
    value    = c(
      pmm_completed[[v]][was_missing],
      step1_cart_completed[[v]][was_missing],
      step1_rf_completed[[v]][was_missing]
    )
  )
}))

ggplot(compare_df, aes(x = value, colour = method, fill = method)) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ variable, scales = "free") +
  scale_colour_manual(values = c("PMM" = "#0072B2", "CART" = "#D55E00", "RF" = "#009E73")) +
  scale_fill_manual(values   = c("PMM" = "#0072B2", "CART" = "#D55E00", "RF" = "#009E73")) +
  labs(title = "Distribution of Imputed Values by Method",
       x = "Imputed Value", y = "Density", colour = "Method", fill = "Method") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 10),
        axis.text  = element_text(size = 10),
        axis.title = element_text(size = 10))

## Plausibility violations per method ------------------------------------------

check_plausibility <- function(df) {
  c(
    eda_sex_gt_EDAD  = if (all(c("eda_sex","EDAD") %in% names(df)))
      sum(!is.na(df$eda_sex) & !is.na(df$EDAD) & df$eda_sex > df$EDAD) else NA,
    eda_mat_gt_EDAD  = if (all(c("eda_mat","EDAD") %in% names(df)))
      sum(!is.na(df$eda_mat) & !is.na(df$EDAD) & df$eda_mat > df$EDAD) else NA,
    eda_hij_gt_EDAD  = if (all(c("eda_hij","EDAD") %in% names(df)))
      sum(!is.na(df$eda_hij) & !is.na(df$EDAD) & df$eda_hij > df$EDAD) else NA,
    eda_sex_gt_hij   = if (all(c("eda_sex","eda_hij") %in% names(df)))
      sum(!is.na(df$eda_sex) & !is.na(df$eda_hij) & df$eda_sex > df$eda_hij) else NA,
    ingm_no_work     = if (all(c("ingm_muj","empleo_vida") %in% names(df)))
      sum(df$ingm_muj > 0 & df$empleo_vida == "no", na.rm = TRUE) else NA
  )
}

plaus_table <- data.frame(
  Check = c("eda_sex > EDAD", "eda_mat > EDAD", "eda_hij > EDAD",
            "eda_sex > eda_hij", "ingm_muj > 0 & never worked"),
  PMM   = check_plausibility(pmm_completed),
  CART  = check_plausibility(step1_cart_completed),
  RF    = check_plausibility(step1_rf_completed),
  row.names = NULL
)
print(plaus_table)

## Iterative PMM: re-impute until no implausible values remain -----------------
# Approach: after each imputation, implausible *imputed* values are set back to
# NA so mice re-draws them in the next iteration. Originally observed values are
# never touched. Terminates when zero implausible imputed values remain.

# Which rows had each variable originally missing (fixed across iterations)
was_na <- list(
  eda_sex  = is.na(raw_df$eda_sex),
  eda_mat  = is.na(raw_df$eda_mat),
  eda_hij  = is.na(raw_df$eda_hij),
  ingm_muj = is.na(raw_df$ingm_muj)
)

current_df   <- raw_df
max_iter     <- 20
iter         <- 0
n_implausible <- Inf

while (n_implausible > 0 && iter < max_iter) {
  iter <- iter + 1

  mids_iter      <- mice(current_df, m = 1, maxit = 5, meth = "pmm",
                         seed = 800 + iter, printFlag = FALSE)
  completed_iter <- complete(mids_iter, 1)

  # Violations that were introduced by imputation (originally NA)
  viol_sex_edad <- was_na$eda_sex  & completed_iter$eda_sex > completed_iter$EDAD
  viol_mat_edad <- was_na$eda_mat  & completed_iter$eda_mat > completed_iter$EDAD
  viol_hij_edad <- was_na$eda_hij  & completed_iter$eda_hij > completed_iter$EDAD
  viol_sex_hij  <- (was_na$eda_sex | was_na$eda_hij) &
                     completed_iter$eda_sex > completed_iter$eda_hij
  viol_ingm     <- was_na$ingm_muj &
                     completed_iter$ingm_muj > 0 & completed_iter$empleo_vida == "no"

  # Reset only the originally-missing variable(s) in each violation
  current_df$eda_sex[viol_sex_edad]                    <- NA
  current_df$eda_mat[viol_mat_edad]                    <- NA
  current_df$eda_hij[viol_hij_edad]                    <- NA
  current_df$eda_sex[viol_sex_hij & was_na$eda_sex]   <- NA
  current_df$eda_hij[viol_sex_hij & was_na$eda_hij]   <- NA
  current_df$ingm_muj[viol_ingm]                      <- NA

  n_implausible <- sum(viol_sex_edad, viol_mat_edad, viol_hij_edad,
                       viol_sex_hij,  viol_ingm,  na.rm = TRUE)
  cat(sprintf("Iteration %d: %d implausible imputed value(s) reset to NA\n",
              iter, n_implausible))
}

if (iter == max_iter && n_implausible > 0)
  warning(sprintf("Max iterations (%d) reached; %d implausible value(s) remain.",
                  max_iter, n_implausible))

step1_pmm_constrained <- completed_iter
cat(sprintf("\nIterative PMM done in %d iteration(s). Stored as step1_pmm_constrained.\n", iter))

# Sanity check: should print all zeros
print(check_plausibility(step1_pmm_constrained))

## Overall method comparison across all imputed variables -----------------------

imputed_vars <- names(raw_df)[colSums(is.na(raw_df)) > 0]
methods      <- list(PMM = pmm_completed, CART = step1_cart_completed, RF = step1_rf_completed)

### Distribution preservation --------------------------------------------------

# Numeric variables: KS statistic between observed (non-missing) and imputed values
# Factor variables:  total variation distance between observed and imputed proportions
# Lower = better for both metrics

eval_dist <- function(completed, var) {
  was_missing <- is.na(raw_df[[var]])
  obs         <- raw_df[[var]][!was_missing]
  imp         <- completed[[var]][was_missing]

  if (is.numeric(obs)) {
    list(type = "numeric", stat = ks.test(imp, obs)$statistic)
  } else {
    obs_prop <- prop.table(table(obs))
    imp_prop <- prop.table(table(imp))
    all_lvls <- union(names(obs_prop), names(imp_prop))
    p <- obs_prop[all_lvls]; p[is.na(p)] <- 0
    q <- imp_prop[all_lvls]; q[is.na(q)] <- 0
    list(type = "factor", stat = 0.5 * sum(abs(p - q)))  # total variation distance
  }
}

dist_rows <- do.call(rbind, lapply(imputed_vars, function(v) {
  res <- lapply(methods, eval_dist, var = v)
  data.frame(
    variable = v,
    type     = res[[1]]$type,
    PMM      = round(res$PMM$stat,  4),
    CART     = round(res$CART$stat, 4),
    RF       = round(res$RF$stat,   4),
    row.names = NULL
  )
}))

cat("\n--- Distribution preservation (lower = better) ---\n")
print(dist_rows)

# Summary: mean statistic per method and variable type
cat("\n--- Mean score by variable type ---\n")
print(
  dist_rows %>%
    group_by(type) %>%
    summarise(PMM = mean(PMM), CART = mean(CART), RF = mean(RF), .groups = "drop")
)

### Correlation structure preservation -----------------------------------------

# Frobenius norm of difference between correlation matrices (numeric vars only)
# Lower = better

num_vars <- names(raw_df)[sapply(raw_df, is.numeric)]
cor_obs  <- cor(raw_df[, num_vars], use = "pairwise.complete.obs")

frob <- function(completed) {
  cor_imp <- cor(completed[, num_vars], use = "pairwise.complete.obs")
  norm(cor_obs - cor_imp, type = "F")
}

cat("\n--- Frobenius norm vs observed correlation matrix (lower = better) ---\n")
print(data.frame(
  PMM  = round(frob(pmm_completed),        4),
  CART = round(frob(step1_cart_completed), 4),
  RF   = round(frob(step1_rf_completed),   4)
))

### Summary scoreboard ---------------------------------------------------------

# Rank each method per variable (1 = best), then average ranks
rank_df <- dist_rows %>%
  mutate(
    rank_PMM  = rank(c(PMM,  CART, RF))[1],
    rank_CART = rank(c(PMM,  CART, RF))[2],
    rank_RF   = rank(c(PMM,  CART, RF))[3]
  )

# Simpler: row-wise rank
ranks <- t(apply(dist_rows[, c("PMM","CART","RF")], 1, rank))
colnames(ranks) <- c("PMM","CART","RF")

cat("\n--- Mean rank across all imputed variables (lower = better) ---\n")
print(round(colMeans(ranks), 3))

cat("\n--- Proportion of variables where each method ranks best ---\n")
print(round(colMeans(ranks == 1), 3))

## Implausible values in the original data (before imputation) -----------------

# Uses endireh_2021 — the raw data before any imputation
# NA comparisons evaluate to FALSE, so cases with missing values in either
# variable are correctly excluded from the violation counts.

orig <- endireh_2021

plaus_orig <- data.frame(
  Check             = c("eda_sex > EDAD", "eda_mat > EDAD", "eda_hij > EDAD",
                        "eda_sex > eda_hij", "ingm_muj > 0 & never worked"),
  N_original        = c(
    sum(!is.na(orig$eda_sex)  & !is.na(orig$EDAD)    & orig$eda_sex  > orig$EDAD,    na.rm = TRUE),
    sum(!is.na(orig$eda_mat)  & !is.na(orig$EDAD)    & orig$eda_mat  > orig$EDAD,    na.rm = TRUE),
    sum(!is.na(orig$eda_hij)  & !is.na(orig$EDAD)    & orig$eda_hij  > orig$EDAD,    na.rm = TRUE),
    sum(!is.na(orig$eda_sex)  & !is.na(orig$eda_hij) & orig$eda_sex  > orig$eda_hij, na.rm = TRUE),
    sum(!is.na(orig$ingm_muj) & !is.na(orig$empleo_vida) &
          orig$ingm_muj > 0   & orig$empleo_vida == "no", na.rm = TRUE)
  ),
  N_missing_either  = c(
    sum(is.na(orig$eda_sex)  | is.na(orig$EDAD)),
    sum(is.na(orig$eda_mat)  | is.na(orig$EDAD)),
    sum(is.na(orig$eda_hij)  | is.na(orig$EDAD)),
    sum(is.na(orig$eda_sex)  | is.na(orig$eda_hij)),
    sum(is.na(orig$ingm_muj) | is.na(orig$empleo_vida))
  ),
  row.names = NULL
)

cat("\n--- Implausible values in original data (before imputation) ---\n")
print(plaus_orig)

## How many imputed violations were already present in the original data? -------

# mice only modifies missing values, so rows where both variables were
# non-missing in the original carry through unchanged into every imputed dataset.
# N_already = violations present in original (non-missing + constraint broken)
# N_new     = additional violations introduced by imputation (was missing before)

# Logical vectors for "already implausible" rows in the original
already <- list(
  eda_sex_gt_EDAD  = !is.na(orig$eda_sex)  & !is.na(orig$EDAD)    & orig$eda_sex  > orig$EDAD,
  eda_mat_gt_EDAD  = !is.na(orig$eda_mat)  & !is.na(orig$EDAD)    & orig$eda_mat  > orig$EDAD,
  eda_hij_gt_EDAD  = !is.na(orig$eda_hij)  & !is.na(orig$EDAD)    & orig$eda_hij  > orig$EDAD,
  eda_sex_gt_hij   = !is.na(orig$eda_sex)  & !is.na(orig$eda_hij) & orig$eda_sex  > orig$eda_hij,
  ingm_no_work     = !is.na(orig$ingm_muj) & !is.na(orig$empleo_vida) &
                       orig$ingm_muj > 0   & orig$empleo_vida == "no"
)

# Logical vectors for violations in each imputed dataset
violated_in <- function(df) list(
  eda_sex_gt_EDAD  = !is.na(df$eda_sex)  & !is.na(df$EDAD)    & df$eda_sex  > df$EDAD,
  eda_mat_gt_EDAD  = !is.na(df$eda_mat)  & !is.na(df$EDAD)    & df$eda_mat  > df$EDAD,
  eda_hij_gt_EDAD  = !is.na(df$eda_hij)  & !is.na(df$EDAD)    & df$eda_hij  > df$EDAD,
  eda_sex_gt_hij   = !is.na(df$eda_sex)  & !is.na(df$eda_hij) & df$eda_sex  > df$eda_hij,
  ingm_no_work     = !is.na(df$ingm_muj) & !is.na(df$empleo_vida) &
                       df$ingm_muj > 0   & df$empleo_vida == "no"
)

viol_pmm  <- violated_in(pmm_completed)
viol_cart <- violated_in(step1_cart_completed)
viol_rf   <- violated_in(step1_rf_completed)

origin_table <- data.frame(
  Check        = c("eda_sex > EDAD", "eda_mat > EDAD", "eda_hij > EDAD",
                   "eda_sex > eda_hij", "ingm_muj > 0 & never worked"),
  N_already    = sapply(already, sum),
  PMM_total    = sapply(viol_pmm,  sum),
  PMM_new      = mapply(function(v, a) sum(v & !a), viol_pmm,  already),
  CART_total   = sapply(viol_cart, sum),
  CART_new     = mapply(function(v, a) sum(v & !a), viol_cart, already),
  RF_total     = sapply(viol_rf,   sum),
  RF_new       = mapply(function(v, a) sum(v & !a), viol_rf,   already),
  row.names    = NULL
)

cat("\n--- Violations by origin: already in original vs newly introduced ---\n")
cat("(N_already is the same for all methods; *_new = introduced by imputation)\n")
print(origin_table)

## Income implausibility: women with ingm_muj > 0 but empleo_vida == "no" -------

# Exchange rate: 2021 annual average (ENDIREH survey year)
# Source: Banco de México / ECB: 1 EUR ≈ 24.08 MXN
eur_mxn <- 24.08

# Mexican monthly minimum wage 2021 (general zone): 141.70 MXN/day × 30
min_wage_mxn <- 141.70 * 30   # 4,251 MXN/month
min_wage_eur <- min_wage_mxn / eur_mxn

implaus_income <- orig %>%
  filter(!is.na(ingm_muj) & !is.na(empleo_vida) &
           ingm_muj > 0   & empleo_vida == "no") %>%
  mutate(
    ingm_muj_eur    = round(ingm_muj / eur_mxn, 2),
    pct_of_min_wage = round(ingm_muj / min_wage_mxn * 100, 1)
  ) %>%
  select(ID_PER, empleo_vida, ingm_muj, ingm_muj_eur, pct_of_min_wage)

cat("\n--- Implausible income observations (ingm_muj > 0 & empleo_vida == 'no') ---\n")
cat(sprintf("N = %d observations\n", nrow(implaus_income)))
cat(sprintf("Exchange rate used: 1 EUR = %.2f MXN (2021 annual average)\n", eur_mxn))
cat(sprintf("Reference: 2021 monthly minimum wage = %.0f MXN = %.1f EUR\n\n",
            min_wage_mxn, min_wage_eur))

print(summary(implaus_income[, c("ingm_muj", "ingm_muj_eur", "pct_of_min_wage")]))

# Why the values are non-reasonable ----
cat("\n--- Breakdown by income level relative to minimum wage ---\n")
print(
  implaus_income %>%
    mutate(income_band = case_when(
      ingm_muj_eur <  10                           ~ "< 10 EUR  (noise / rounding artefact)",
      ingm_muj_eur >= 10  & ingm_muj_eur < min_wage_eur ~ "10 – min wage (implausibly low)",
      ingm_muj_eur >= min_wage_eur                 ~ ">= min wage (contradicts never worked)"
    )) %>%
    count(income_band) %>%
    mutate(share = scales::percent(n / sum(n), accuracy = 0.1))
)

# Distribution plot
ggplot(implaus_income, aes(x = ingm_muj_eur)) +
  geom_histogram(bins = 40, fill = "#0072B2", colour = "white", alpha = 0.8) +
  geom_vline(xintercept = min_wage_eur, linetype = "dashed", colour = "#D55E00", linewidth = 0.8) +
  annotate("text", x = min_wage_eur + 5, y = Inf, vjust = 1.5,
           label = "Min. wage (2021)", colour = "#D55E00", size = 3.5, hjust = 0) +
  labs(
    title = "Income distribution of women who report never having worked",
    x     = "Monthly income (EUR, 2021)",
    y     = "Count"
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        axis.text  = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold"))

#### Exkurs

implaus_income_raw <- implaus_income %>%                                                                                                                                                            
left_join( TB_SEC_IVaVD %>% select(ID_PER, P4_1,P8_1, P4_2, ing_muj, P4_2_1, P4_2_1_new, ingm_muj),                                                                                                             
by = "ID_PER"                                                                                                                                                                                   
 )                                                                                                                                                                                                 




# Save data
#save(step1_endireh, file = paste0(path_save,"step1_endireh.RData"))
# save(step1_cart_completed, file = paste0(path_save,"step1_cart_completed.RData"))
# save(step1_rf_completed, file = paste0(path_save,"step1_rf_completed.RData"))

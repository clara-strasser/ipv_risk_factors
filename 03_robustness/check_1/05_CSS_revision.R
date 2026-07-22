############################ Main Analysis #############################
# See also: Torres Munguía & Martínez-Zarzoso (2022)


## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)
library(stabs)
library(ggplot2)

## Set path --------------------------------------------------------------------
path_data <- "/dss/dsshome1/0B/ru23kek2/data/robustness/"
#path_save <- path where estimation results should be saved

## Load data -------------------------------------------------------------------
load(paste0(path_data, "model2.RData"))
load(paste0(path_data, "stabsel2.RData"))
load(paste0(path_data, "confintemoipv2.RData"))
source("src/conf_table.R")

## Stability selection results table -------------------------------------------

# stabselemoipv$selected is a named integer vector:
#   names  = full base-learner spec, e.g. "bols(vio_inf, intercept = FALSE, df = 1)"
#   values = position index
# names() gives the strings that match names(coef(modelemoipv)).
selected_bl_names <- names(stabselemoipv$selected)
sel_probs         <- stabselemoipv$max[selected_bl_names]

model_coefs <- coef(modelemoipv)

estimates <- sapply(selected_bl_names, function(bl) {
  if (bl %in% names(model_coefs)) {
    cf <- model_coefs[[bl]]
    if (length(cf) == 1L) round(cf, 4L) else NA_real_
  } else {
    NA_real_
  }
})

effect_type <- dplyr::case_when(
  grepl("^bols",     selected_bl_names) ~ "linear",
  grepl("^bbs",      selected_bl_names) ~ "smooth",
  grepl("^bspatial", selected_bl_names) ~ "spatial",
  grepl("^brandom",  selected_bl_names) ~ "random",
  TRUE                                  ~ "other"
)

# AME for linear (bols) terms: mean(phi(linear predictor)) * beta
# margins / marginaleffects do not support gamboost, so compute directly.
# For smooth / spatial / random terms AME is left NA (effect is nonlinear).
lp           <- predict(modelemoipv, type = "link")
scale_factor <- mean(dnorm(lp))

ame <- ifelse(!is.na(estimates), round(scale_factor * estimates, 4), NA_real_)

stabsel_table <- data.frame(
  Variable      = selected_bl_names,
  `Sel. Prob`   = round(sel_probs, 3),
  Estimate      = estimates,
  AME           = ame,
  `Effect type` = effect_type,
  row.names     = NULL,
  check.names   = FALSE,
  stringsAsFactors = FALSE
)

stabsel_table <- stabsel_table[order(-stabsel_table[["Sel. Prob"]]), ]
rownames(stabsel_table) <- NULL

## 95% CI for AME via bootstrap confint ----------------------------------------

linear_bls <- selected_bl_names[effect_type == "linear"]

ci_list <- lapply(linear_bls, function(bl) {
  tryCatch(
    conf_table(confintemoipv, which = bl),
    error = function(e) data.frame(which = NA_integer_, lowerCI = NA_real_, upperCI = NA_real_)
  )
})

ci_df <- data.frame(
  Variable  = linear_bls,
  AME_lower = round(scale_factor * sapply(ci_list, `[[`, "lowerCI"), 4),
  AME_upper = round(scale_factor * sapply(ci_list, `[[`, "upperCI"), 4),
  row.names = NULL,
  stringsAsFactors = FALSE
)

stabsel_table <- stabsel_table %>%
  left_join(ci_df, by = "Variable") %>%
  arrange(desc(`Sel. Prob`))

print(stabsel_table)

## Partial effect plots for selected numerical variables -----------------------
source("src/custom_mboost.R")

# Load original data to restore mean-centred x-axis to original scale
path_data_raw <- "/dss/dsshome1/0B/ru23kek2/data/final_data/"
load(paste0(path_data_raw, "data_no_imp.RData"))
mean_masnoden <- mean(data_no_imp2$MasNoDen, na.rm = TRUE)
mean_eda_sex  <- mean(data_no_imp2$eda_sex,  na.rm = TRUE)
rm(data_no_imp)

# The two numerical variables as in confint.R
num_plots <- list(
  list(
    which  = "bols(MasNoDen, intercept = FALSE)",
    x_col  = "MasNoDen",
    by_col = NULL,
    mean_x = mean_masnoden,
    xlab   = "Share of Non-Reported Common Crimes Against Men"
  ),
  list(
    which  = "bols(eda_sex, by = con_sex, intercept = FALSE)",
    x_col  = "eda_sex",
    by_col = "con_sex",
    mean_x = mean_eda_sex,
    xlab   = "Woman's Age at First Sexual Intercourse (Consent to Sex = Yes)"
  )
)

for (plt in num_plots) {
  
  which_idx <- confintemoipv$model$which(plt$which, usedonly = FALSE)
  
  # CI matrix from bootstrap: 2 x n  (row 1 = lower, row 2 = upper)
  ci_probit <- .ci_mboost(confintemoipv$boot_pred, level = 0.95, which = which_idx)
  
  # x data stored in the confint object (mean-centred)
  x_data <- confintemoipv$data[[which_idx]]
  
  if (is.null(plt$by_col)) {
    # Simple continuous variable: all rows
    x_vals   <- x_data[[plt$x_col]] + plt$mean_x
    y_probit <- rowSums(predict(modelemoipv, which = which_idx,
                                newdata = x_data, type = "link"))
  } else {
    # Interaction term: keep only rows where by_col is at the positive level
    by_vals <- x_data[[plt$by_col]]
    keep    <- if (is.factor(by_vals)) by_vals == levels(by_vals)[2L] else by_vals == 1
    x_data_sub <- x_data[keep, , drop = FALSE]
    x_vals     <- x_data_sub[[plt$x_col]] + plt$mean_x
    ci_probit  <- ci_probit[, keep, drop = FALSE]
    y_probit   <- rowSums(predict(modelemoipv, which = which_idx,
                                  newdata = x_data_sub, type = "link"))
  }
  
  # AME and its CI
  y_ame  <- scale_factor * y_probit
  ci_ame <- scale_factor * ci_probit
  
  ord <- order(x_vals)
  
  df_plot <- data.frame(
    x         = x_vals[ord],
    y_probit  = y_probit[ord],
    lo_probit = ci_probit[1L, ord],
    hi_probit = ci_probit[2L, ord],
    y_ame     = y_ame[ord],
    lo_ame    = ci_ame[1L, ord],
    hi_ame    = ci_ame[2L, ord]
  )
  
  p <- ggplot(df_plot, aes(x = x)) +
    geom_ribbon(aes(ymin = lo_probit, ymax = hi_probit),
                fill = "#0072B2", alpha = 0.15) +
    geom_line(aes(y = y_probit, colour = "Probit"), linewidth = 0.8) +
    geom_ribbon(aes(ymin = lo_ame, ymax = hi_ame),
                fill = "#D55E00", alpha = 0.15) +
    geom_line(aes(y = y_ame, colour = "AME"), linewidth = 0.8) +
    geom_rug(sides = "b", alpha = 0.3) +
    scale_colour_manual(
      name   = "Scale",
      values = c("Probit" = "#0072B2", "AME" = "#D55E00")
    ) +
    labs(x = plt$xlab, y = "f(x)") +
    theme_minimal() +
    theme(
      axis.line       = element_line(color = "black"),
      axis.title      = element_text(size = 10),
      axis.text       = element_text(size = 10),
      axis.ticks.y    = element_line(),
      axis.ticks.x    = element_line(),
      plot.title      = element_text(face = "bold", hjust = 0.5, size = 10, color = "black"),
      legend.title    = element_text(size = 10, face = "bold"),
      legend.text     = element_text(size = 10),
      legend.position = "bottom"
    )
  
  print(p)
}

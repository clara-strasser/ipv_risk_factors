# Model authors



data$mot_mat <- relevel(data$mot_mat, ref = "no")
data$lib_eco_gradmedium <- relevel(data$lib_eco_gradmedium, ref = "no")
data$redsoc_gradmedium <- relevel(data$redsoc_gradmedium, ref = "no")
data$act_distmales <- relevel(data$act_distmales, ref = "no")



data <- vawgdbOutcC
offset_var_authors <- rep(pnorm(weighted.mean(x = as.numeric(as.character(data[, "vio_emo_año"])),
                                      w = data$FAC_MUJ))-0.5, nrow(data))
model <- gam(vio_emo_año ~ EDAD + eda_sex:con_sex + mot_mat:eda_mat + 
               lib_eco_gradmedium + redsoc_gradmedium + act_distmales + s(gini15) +
               pea_f + MasPrev,
             family = binomial(link = "probit"), 
             weights = data$FAC_MUJ,
             offset = offset_var_authors,
             data = data)
summary(model)















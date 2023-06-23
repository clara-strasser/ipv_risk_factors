





### Three-step strategy ###
## Functional gradient descent boosting
modelemoipv <- gamboost(vio_emo_año,
                        data = vawgdbOutcC,
                        control = boost_control(mstop = 2000, nu = 0.5, 
                                                trace = TRUE, 
                                                stopintern = TRUE),
                        weights = vawgdbOutcC$FAC_MUJ,
                        offset = pnorm(weighted.mean(x = as.numeric(as.character(vawgdbOutcC[, "vio_emo_año"])),
                                                     w = vawgdbOutcC$FAC_MUJ))-0.5,
                        family = Binomial(link = "probit"))

# Cross-validation
set.seed(1806)
cvemoipv <- cvrisk(modelemoipv, folds = cv(model.weights(modelemoipv), 
                                           type = "subsampling"), 
                   grid = 1:10000, 
                   papply = mclapply,
                   mc.cores = parallel::detectCores())
stopemoipv <- mstop(cvemoipv)
modelemoipv[stopemoipv]

## Stability selection
p <- length(names(coef(modelemoipv, which = "")))
stabsel_conf <- stabsel_parameters(p = p, 
                                   q = 20, 
                                   cutoff = 0.8)
# Stability selection with unimodality assumption

# Cutoff: 0.8; q: 20; PFER (*):  3.74 
# (*) or expected number of low selection probability variables
# PFER (specified upper bound):  3.743316 
# PFER corresponds to signif. level 0.0425 (without multiplicity adjustment)
stabselemoipv <- stabsel(modelemoipv,
                         q = 20, 
                         cutoff = 0.8,
                         sampling.type = "SS",
                         mc.cores = parallel::detectCores())

## Pointwise bootstrap confidence intervals
confintemoipv <- confint(modelemoipv, B = 1000, 
                         level = 0.95, B.mstop = 0,
                         papply = mclapply, 
                         cvrisk_options = list(mc.cores = 25))

save(confintemoipv, stabselemoipv, modelemoipv, stopemoipv, cvemoipv, file = "estimation_ipv.RData")

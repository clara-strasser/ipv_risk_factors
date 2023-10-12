############################ Confidence Intervals #############################

# Aim: Print confidence intervals for the specified categorical variables, so they can
# be included in the table
# Reason: for gamboost only plots for the confidence intervals can be generated
# Source and credits: https://github.com/boost-R/mboost/blob/master/R/confint.R
# Remark: work in progress!



# First --------
# Function .ci_mboost() is needed for the conf_table() function
.ci_mboost <- function(predictions, level, which = NULL, raw = FALSE) {
  
  preds <- sapply(predictions, function(p) p[[which]])
  if (!raw) {
    quantiles <- c((1 - level)/2, 1 - (1 - level)/2)
    preds <- apply(preds, 1, FUN = quantile, probs = quantiles)
  }
  
  return(preds)
}

# Second --------
# Generate conf_table() function

conf_table <- function(x, which, level = x$level, raw = FALSE, ...) {
  
  if (missing(which)) {
    which <- attr(x, "which")
  } else {
    which <- x$model$which(which, usedonly = FALSE)
    if (!all(which %in% attr(x, "which")))
      stop(sQuote("which"), " is wrongly specified")
  }
  if (length(which) > 1)
    stop("Specify a single base-learner using ", sQuote("which"))
  
  CI <- .ci_mboost(x$boot_pred, level = level, which = which, raw = raw)
  
  varying <- which(sapply(x$data[[which]], function(x) length(unique(x))) > 1)
  
  if (ncol(x$data[[which]]) > 1 && length(varying) > 1) {
    
    if (length(varying) > 2)
      stop("Plots are only implemented for up to 2 variables.")
    
    fm <- as.formula(paste("pr ~ ", paste(names(varying), collapse = "*"), sep = ""))
    pr <- CI[1, ]  ## lower CI
    pr2 <- CI[2, ]  ## upper CI
    
    # Check if CI[1, ] or CI[2, ] is 0, and replace with CI[1, 2] or CI[2, 2]
    if (pr[1] == 0) pr[1] <- pr[2]
    if (pr2[1] == 0) pr2[1] <- pr2[2]
    
    # Create a data frame with "which," "lowerCI," and "upperCI" columns
    result_df <- data.frame(which = which, lowerCI = pr, upperCI = pr2)
    
    return(result_df)
    
  } else {
    
    # Check if CI[1, ] or CI[2, ] is 0, and replace with CI[1, 2] or CI[2, 2]
    if (CI[1] == 0) CI[1] <- CI[1, 2]
    if (CI[2] == 0) CI[2] <- CI[2, 2]
    
    # Create a data frame with "which," "lowerCI," and "upperCI" columns
    result_df <- data.frame(which = which, lowerCI = CI[1], upperCI = CI[2])
    
    
    return(result_df)
  }
}





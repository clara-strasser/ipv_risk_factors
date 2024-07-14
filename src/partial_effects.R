############################ Partial Effects #############################
# Aim: Create partial effect plots for the boosting effects.
# Inspiration source: https://github.com/adamdsmith/NanSound_EcolEvol/blob/master/R/ggplot_effects.R


partial_effects <- function(model, data, variables) {
  
  # generate empty plot
  plots <- list()
  
  for (variable in variables) {
    # set base learner names
    baselearner_names <- extract(modelemoipv, what = "bnames")
    
    # make distinction between linear and non-linear
    baselearner_bbs <- paste0("bbs(", variable, ", knots = 20, df = 1, center = TRUE)")
    baselearner_bols <- paste0("bols(", variable, ", intercept = FALSE)")
    
    # generate df
    x <- as.data.frame(data[, variable])
    names(x) <- variable
    
    # check if both base learners exist for the variable
    if (baselearner_bbs %in% baselearner_names && baselearner_bols %in% baselearner_names) {
      # make predictions on the link scale (quantiles of the standard normal distribution)
      y_bbs <- rowSums(predict(modelemoipv, which = baselearner_bbs, newdata = x, type = "link"))
      y_bols <- rowSums(predict(modelemoipv, which = baselearner_bols, newdata = x, type = "link"))
      
      # save predictions and true
      data_bbs <- data.table(vars = baselearner_bbs, x = x[, variable], y = y_bbs)
      data_bols <- data.table(vars = baselearner_bols, x = x[, variable], y = y_bols)
      
      # plot linear
      plot_bols <- ggplot(data_bols, aes(x = x, y = y)) +
        geom_line(alpha = 0.5, color = "#002b58") +
        geom_rug(aes(y = y), sides = "b", position = "jitter") +
        labs(x = "", y = paste0("f(",variable, ")"), title = variable) +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              axis.title = element_text(size = 14),
              axis.ticks.y = element_line(),
              axis.ticks.x = element_line(),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
              legend.title = element_text(size = 20, face = "bold"),
              legend.text = element_text(size = 13),
              legend.key.size = unit(15, "points"),
              legend.position = c(0.95, 0.95),  # Adjust the position as needed
              legend.justification = c(1, 1),
              legend.box.just = "right")
      
      # plot non-linear
      plot_bbs <- ggplot(data_bbs, aes(x = x, y = y)) +
        geom_line(alpha = 0.5, color = "#002b58") +
        geom_rug(aes(y = y), sides = "b", position = "jitter") +
        labs(x = "", y = paste0("f(",variable, ")"), title = variable) +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              axis.title = element_text(size = 14),
              axis.ticks.y = element_line(),
              axis.ticks.x = element_line(),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
              legend.title = element_text(size = 20, face = "bold"),
              legend.text = element_text(size = 13),
              legend.key.size = unit(15, "points"),
              legend.position = c(0.95, 0.95),  # Adjust the position as needed
              legend.justification = c(1, 1),
              legend.box.just = "right")
      
      # save
      plots[[variable]] <- list(plot_bbs, plot_bols)
      
    } else if (baselearner_bbs %in% baselearner_names) {
      y_bbs <- rowSums(predict(modelemoipv, which = baselearner_bbs, newdata = x, type = "link"))
      
      data_bbs <- data.table(vars = baselearner_bbs, x = x[, variable], y = y_bbs)
      
      # plot non-linear
      plot_bbs <- ggplot(data_bbs, aes(x = x, y = y)) +
        geom_line(alpha = 0.5, color = "#002b58") +
        geom_rug(aes(y = y), sides = "b", position = "jitter") +
        labs(x = "", y = paste0("f(",variable, ")"), title = variable) +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              axis.title = element_text(size = 14),
              axis.ticks.y = element_line(),
              axis.ticks.x = element_line(),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
              legend.title = element_text(size = 20, face = "bold"),
              legend.text = element_text(size = 13),
              legend.key.size = unit(15, "points"),
              legend.position = c(0.95, 0.95),  # Adjust the position as needed
              legend.justification = c(1, 1),
              legend.box.just = "right")
      
      plots[[variable]] <- list(plot_bbs)
      
    } else if (baselearner_bols %in% baselearner_names) {
      y_bols <- rowSums(predict(modelemoipv, which = baselearner_bols, newdata = x, type = "link"))
      
      data_bols <- data.table(vars = baselearner_bols, x = x[, variable], y = y_bols)
      
      # plot linear
      plot_bols <- ggplot(data_bols, aes(x = x, y = y)) +
        geom_line(alpha = 0.5, color = "#002b58") +
        geom_rug(aes(y = y), sides = "b", position = "jitter") +
        labs(x = "", y = paste0("f(",variable, ")"), title = variable) +
        theme_minimal() +
        theme(axis.line = element_line(color = "black"),
              axis.title = element_text(size = 14),
              axis.ticks.y = element_line(),
              axis.ticks.x = element_line(),
              plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
              legend.title = element_text(size = 20, face = "bold"),
              legend.text = element_text(size = 13),
              legend.key.size = unit(15, "points"),
              legend.position = c(0.95, 0.95),  # Adjust the position as needed
              legend.justification = c(1, 1),
              legend.box.just = "right")
      
      plots[[variable]] <- list(plot_bols)
    }
  }
  
  return(plots)
}


partial_effects_categorical <- function(model, data, variables) {
  
  # generate empty plot
  plots <- list()
  
  for (variable in variables) {
  # get effect of categorical variables
  effect_y <- coef(modelemoipv, which = variable)[[1]]
  y <- data.frame(level = c("no", "yes"), coefficient = c(0, effect_y))
  
  # save in data
  data_bols = data.table(vars = variable, y = y)
  
  # plot
  plot_bols <- ggplot() +
    geom_point(data = data_bols, mapping = aes(x = y.level, y = y.coefficient), color = "#620042", size = 3) +
    geom_line(data = data_bols, mapping = aes(x = y.level, y = y.coefficient, group = 1), color = "#002b58") +
    labs(x = "", y = paste0("f(",variable, ")"), title = variable) +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"),
          axis.title = element_text(size = 14),
          axis.ticks.y = element_line(),
          axis.ticks.x = element_line(),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "black"),
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 13),
          legend.key.size = unit(15, "points"),
          legend.position = c(0.95, 0.95),  # Adjust the position as needed
          legend.justification = c(1, 1),
          legend.box.just = "right")
  
  plots[[variable]] <- list(plot_bols)
  }
  return(plots)
}





















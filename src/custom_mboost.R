library(parallel)
library(stabs)

custom_plot_mboost <- function (x, which = NULL, newdata = NULL, type = "b", rug = TRUE, 
          rugcol = "black", ylim = NULL, xlab = NULL, ylab = expression(f[partial]), 
          add = FALSE, mean = 0,...) 
{
  if (inherits(x, "blackboost")) 
    stop("partial dependency plots for ", sQuote("blackboost"), 
         " not yet implemented.", "See ?blackboost.")
  which <- x$which(which, usedonly = is.null(which))
  if (is.null(xlab)) {
    userspec.xlab <- FALSE
    xlab <- variable.names(x)
  }
  else {
    userspec.xlab <- TRUE
    if (length(which) == length(xlab) & length(which) != 
        length(variable.names(x))) {
      foo <- rep(NA, length(variable.names(x)))
      foo[which] <- xlab
      xlab <- foo
    }
  }
  if (missing(ylab)) {
    userspec.ylab <- FALSE
  }
  else {
    userspec.ylab <- TRUE
    if (length(which) == length(ylab) & length(which) != 
        length(variable.names(x))) {
      foo <- rep(NA, length(variable.names(x)))
      foo[which] <- ylab
      ylab <- foo
    }
  }
  stopifnot(length(xlab) %in% c(1, length(variable.names(x))))
  stopifnot(length(ylab) %in% c(1, length(variable.names(x))))
  RET <- NULL
  n_levelplot <- 0
  for (w in which) {
    data <- model.frame(x, which = w)[[1]]
    get_vary <- x$baselearner[[w]]$get_vary
    vary <- ""
    if (!is.null(get_vary)) 
      vary <- get_vary()
    if (!is.null(newdata)) {
      data <- newdata[colnames(data)]
      if (is.list(data)) 
        data <- as.data.frame(data)
    }
    if (vary != "") {
      v <- data[[vary]]
      if (is.factor(v)) 
        v <- factor(levels(v)[-1], levels = levels(v))
      if (is.numeric(v)) 
        v <- 1
    }
    plot_helper <- function(xl, yl) {
      pr <- predict(x, newdata = data, which = w)
      data <- data + mean
      if (is.null(ylim)) 
        ylim <- range(pr, na.rm = TRUE)
      if (vary != "") {
        datavary <- data[, colnames(data) == vary, drop = FALSE]
        data <- data[, colnames(data) != vary, drop = FALSE]
      }
      if (ncol(data) == 1) {
        if (!add) {
          if (is.factor(data[[1]])) {
            xVals <- unique(sort(data[[1]]))
            xValsN <- as.numeric(xVals)
            yVals <- unique(cbind(pr[order(data[[1]], 
                                           na.last = NA)], sort(data[[1]])))[, 1]
            if (length(pr) == 1 && pr == 0) {
              yVals <- rep(0, length(xVals))
            }
            plot(xValsN, yVals, type = "n", xaxt = "n", 
                 xlim = range(as.numeric(xVals)) + c(-0.5, 
                                                     0.5), xlab = xl, ylab = yl, ylim = ylim)
            print.xaxis <- TRUE
            if (length(list(...)) >= 1 && any(xaxis <- c("xaxt", 
                                                         "axes") %in% names(list(...)))) {
              if (xaxis[1] && list(...)$xaxt == "n") 
                print.xaxis <- FALSE
              if (xaxis[2] && list(...)$axes == FALSE) 
                print.xaxis <- FALSE
            }
            if (print.xaxis) 
              axis(1, at = xValsN, labels = levels(xVals))
            for (i in 1:length(xVals)) {
              lines(x = rep(xValsN[i], 2) + c(-0.35, 
                                              0.35), y = rep(yVals[i], 2), ...)
            }
          }
          else {
            plot(sort(data[[1]]), pr[order(data[[1]], 
                                           na.last = NA)], type = type, xlab = xl, 
                 ylab = yl, ylim = ylim, ...)
          }
          if (rug) 
            rug(data[[1]], col = rugcol)
        }
        else {
          if (is.factor(data[[1]])) {
            xVals <- unique(sort(data[[1]]))
            xValsN <- as.numeric(xVals)
            yVals <- unique(pr[order(data[[1]], na.last = NA)])
            if (length(pr) == 1 && pr == 0) {
              yVals <- rep(0, length(xVals))
            }
            for (i in 1:length(xVals)) {
              lines(x = rep(xValsN[i], 2) + c(-0.35, 
                                              0.35), y = rep(yVals[i], 2), ...)
            }
          }
          else {
            lines(sort(data[[1]]), pr[order(data[[1]], 
                                            na.last = NA)], type = type, ...)
            if (rug) {
              rug(data[[1]], col = rugcol)
              warning(sQuote("rug = TRUE"), " should be used with care if ", 
                      sQuote("add = TRUE"))
            }
          }
        }
      }
      n_levelplot <<- n_levelplot + (ncol(data) == 2)
      if (ncol(data) == 2 && n_levelplot == 1) {
        if (is.null(newdata)) {
          tmp <- expand.grid(unique(data[, 1]), unique(data[, 
                                                            2]))
          colnames(tmp) <- colnames(data)
          data <- tmp
          if (vary != "") {
            tmp <- cbind(datavary[1, ], tmp)
            colnames(tmp)[1] <- vary
          }
          pr <- predict(x, newdata = tmp, which = w)
        }
        fm <- as.formula(paste("pr ~ ", paste(colnames(data), 
                                              collapse = "*"), sep = ""))
        if (!userspec.xlab) 
          xl <- colnames(data)[1]
        if (!userspec.ylab) 
          yl <- colnames(data)[2]
        RET <<- levelplot(fm, data = data, xlab = xl, 
                          ylab = yl, ...)
      }
      if (ncol(data) > 2) {
        for (v in colnames(data)) {
          tmp <- data
          pardep <- sapply(data[[v]], function(vv) {
            tmp[[v]] <- vv
            mean(predict(x, newdata = tmp, which = w))
          })
          if (!userspec.xlab) 
            xl <- v
          plot(sort(data[[v]]), pardep[order(data[[v]], 
                                             na.last = NA)], type = type, xlab = xl, 
               ylab = yl, ylim = ylim, ...)
        }
      }
    }
    xl <- ifelse(length(xlab) > 1, xlab[w], xlab[1])
    yl <- ifelse(length(ylab) > 1, ylab[w], ylab[1])
    if (add && length(which) > 1) 
      warning(sQuote("add = TRUE"), " should be used for single plots only")
    if (!is.matrix(data)) {
      if (vary == "") 
        plot_helper(xl, yl)
      if (vary != "") {
        for (i in 1:length(v)) {
          data[[vary]] <- v[rep(i, nrow(data))]
          if (!userspec.xlab) {
            plot_helper(paste(xl, "=", v[i]), yl)
          }
          else {
            plot_helper(xl, yl)
          }
        }
      }
    }
    else {
      warning(paste(extract(x, what = "bnames", which = w), 
                    ": automated plot not reasonable for base-learners", 
                    " of matrices", sep = ""))
    }
  }
  if (n_levelplot > 0) {
    if (n_levelplot == 1) 
      return(RET)
    stop(paste("Cannot plot multiple surfaces via levelplot;", 
               "Plot base-learners seperately via plot(model, which = ...)!"))
  }
}


custom_lines_ci <- function (x, which, level = x$level, col = rgb(170, 170, 170, 
                            alpha = 85, maxColorValue = 255), raw = FALSE, mean=mean ,...) 
{
  if (missing(which)) {
    which <- attr(x, "which")
  }
  else {
    which <- x$model$which(which, usedonly = FALSE)
    if (!all(which %in% attr(x, "which"))) 
      stop(sQuote("which"), " is wrongly specified")
  }
  if (length(which) > 1) 
    stop("Specify a single base-learner using ", sQuote("which"))
  CI <- .ci_mboost(x$boot_pred, level = level, which = which, 
                   raw = raw)
  x.data <- x$data[[which]] + mean
  if (ncol(x.data) > 1 && sum(sapply(x.data, function(x) length(unique(x))) > 
                              1) > 1) {
    stop("Cannot plot lines for more than 1 dimension")
  }
  else {
    x.data <- x.data[, 1]
  }
  if (is.factor(x.data)) {
    if (raw) 
      warning("plotting raw values is currently not implemented", 
              " for factors")
    pData <- cbind(x.data, t(CI))
    pData <- unique(pData)
    for (i in 1:nrow(pData)) {
      polygon(x = pData[i, 1] + c(-0.35, 0.35, 0.35, -0.35), 
              y = rep(pData[i, 2:3], each = 2), col = col, 
              border = col, ...)
    }
  }
  else {
    if (!raw) {
      polygon(c(x.data, rev(x.data)), c(CI[1, ], rev(CI[2, 
      ])), col = col, border = col)
    }
    else {
      matlines(x$data[[which]], CI, col = col, lty = "solid", 
               ...)
    }
  }
}


custom_lines <- function (x, which = NULL, type = "l", rug = FALSE, mean=mean, ...) 
{
  custom_plot_mboost(x, which = which, type = type, add = TRUE, rug = rug, mean=mean,
       ...)
}




custom_plot_mboost_ci <- function (x, which, level = x$level, ylim = NULL, type = "l", 
                                   col = "black", ci.col = rgb(170, 170, 170, alpha = 85, maxColorValue = 255), 
                                   raw = FALSE, print_levelplot = TRUE, mean=0, ...) 
{
  if (missing(which)) {
    which <- attr(x, "which")
  }
  else {
    which <- x$model$which(which, usedonly = FALSE)
    if (!all(which %in% attr(x, "which"))) 
      stop(sQuote("which"), " is wrongly specified")
  }
  if (length(which) > 1) 
    stop("Specify a single base-learner using ", sQuote("which"))
  CI <- .ci_mboost(x$boot_pred, level = level, which = which, 
                   raw = raw)
  varying <- which(sapply(x$data[[which]], function(x) length(unique(x))) > 
                     1)
  if (ncol(x$data[[which]]) > 1 && length(varying) > 1) {
    if (length(varying) > 2) 
      stop("Plots are only implemented for up to 2 variables.")
    p1 <- custom_plot_mboost(x$model, which = which, newdata = x$data[[which]], 
               main = "Mean surface", mean=mean, ...)
    fm <- as.formula(paste("pr ~ ", paste(names(varying), 
                                          collapse = "*"), sep = ""))
    pr <- CI[1, ]
    p2 <- levelplot(fm, data = x$data[[which]] + mean, main = paste(rownames(CI)[1], 
                                                             "CI surface"), ...)
    pr <- CI[2, ]
    p3 <- levelplot(fm, data = x$data[[which]] + mean, main = paste(rownames(CI)[2], 
                                                             "CI surface"), ...)
    if (print_levelplot) {
      print(p1, position = c(0, 0, 0.33, 1), more = TRUE)
      print(p2, position = c(0.33, 0, 0.66, 1), more = TRUE)
      print(p3, position = c(0.66, 0, 1, 1))
      warning("The scale is not the same")
    }
    else {
      return(list(mean = p1, lowerCI = p2, upperCI = p3))
    }
  }
  else {
    print("Hi")
    if (is.null(ylim)) {
      ylim <- range(CI)
    }
    custom_plot_mboost(x$model, which = which, newdata = x$data[[which]], 
         rug = FALSE, type = "n", ylim = ylim, col = col, mean = mean, ...)
    custom_lines_ci(x, which, level, col = ci.col, raw = raw, mean = mean, ...)
    custom_lines(x$model, which = which, newdata = x$data[[which]], 
          rug = FALSE, type = "l", col = col, mean = mean, ...)
  }
}


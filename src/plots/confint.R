############################ Confidence Intervals #############################


# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(mboost)
library(parallel)
library(stabs)

## Set path -----
path_data <- "" # include

## Load data -------
load(paste0(path_data, "confintemoipv.RData")) # include confidence interval info from confint()

# Coefficients -----

## Plot ------

# Individual Level:
# Unemployment - 14
plot(confintemoipv, which =14, col = "#002b58", xlab="Unemployment")
# Vio Inf - 16
plot(confintemoipv, which =16, col = "#002b58", xlab="Violence Witnessed in Childhood")
# Vio Inf Exp - 17
plot(confintemoipv, which =17, col = "#002b58", xlab="Violence Experienced in Childhood")
# Sex Vio Inf Exp - 17
plot(confintemoipv, which =18, col = "#002b58", xlab="Sexual Violence Experienced in Childhood")
# Consent to Sex - 26
plot(confintemoipv, which =26, col = "#002b58", xlab="Consent to Sex at First Sexual Intercourse")
# Eda Sex - 27
plot(confintemoipv, which =27, col = "#002b58", xlab="Women's Age at First Sexual Intercourse by Consent to Sex Yes")
# Con Marr - 32
plot(confintemoipv, which =32, col = "#002b58", xlab="Consent to Current Marriage or Cohabitation")

# Relationship Level:

# Vio inf par - 36
plot(confintemoipv, which =36, col = "#002b58", xlab="Violence Witnessed by Partner in Childhood")
# Vio exp par - 37
plot(confintemoipv, which =37, col = "#002b58", xlab="Violence Experienced by Partner in Childhood")
# Div both - 40
plot(confintemoipv, which =40, col = "#002b58", xlab="Division of Housework Among Both")
# Div men - 41
plot(confintemoipv, which =41, col = "#002b58", xlab="Division of Housework Among Men")
# Aut sex med - 44
plot(confintemoipv, which =44, col = "#002b58", xlab="Woman’s Level Autonomy about Sex Life Medium")
# Aut eco med - 46 
plot(confintemoipv, which =46, col = "#002b58", xlab="Woman’s Level Autonomy about Economic Resources Medium")
# Sup high - 51
plot(confintemoipv, which =51, col = "#002b58", xlab="Women’s Perception About Support from Social Network High")
# Soc med - 52
plot(confintemoipv, which =52, col = "#002b58", xlab="Level of Social Interaction Medium")

# Societal Level

# share non rep - 85 
plot(confintemoipv, which =85, col = "#002b58", xlab="Share of Non Reported Common Crimes Against Men")



# Set the layout for the subplots (5 rows, 3 columns)
par(mfrow = c(6, 3))

# Create and arrange your plots in a grid
plot1 <- plot(confintemoipv, which = 14, col = "#002b58", xlab = "Unemployment")
plot2 <- plot(confintemoipv, which = 16, col = "#002b58", xlab = "Violence Witnessed in Childhood")
plot3 <- plot(confintemoipv, which = 17, col = "#002b58", xlab = "Violence Experienced in Childhood")
plot4 <- plot(confintemoipv, which = 18, col = "#002b58", xlab = "Sexual Violence Experienced in Childhood")
plot5 <- plot(confintemoipv, which = 26, col = "#002b58", xlab = "Consent to Sex at First Sexual Intercourse")
plot6 <- plot(confintemoipv, which = 27, col = "#002b58", xlab = "Women's Age at First Sexual Intercourse by Consent to Sex Yes")
plot7 <- plot(confintemoipv, which = 32, col = "#002b58", xlab = "Consent to Current Marriage or Cohabitation")
plot8 <- plot(confintemoipv, which = 36, col = "#002b58", xlab = "Violence Witnessed by Partner in Childhood")
plot9 <- plot(confintemoipv, which = 37, col = "#002b58", xlab = "Violence Experienced by Partner in Childhood")
plot10 <- plot(confintemoipv, which = 40, col = "#002b58", xlab = "Division of Housework Among Both")
plot11 <- plot(confintemoipv, which = 41, col = "#002b58", xlab = "Division of Housework Among Men")
plot12 <- plot(confintemoipv, which = 44, col = "#002b58", xlab = "Woman’s Level Autonomy about Sex Life Medium")
plot13 <- plot(confintemoipv, which = 46, col = "#002b58", xlab = "Woman’s Level Autonomy about Economic Resources Medium")
plot14 <- plot(confintemoipv, which = 51, col = "#002b58", xlab = "Women’s Perception About Support from Social Network High")
plot15 <- plot(confintemoipv, which = 52, col = "#002b58", xlab = "Level of Social Interaction Medium")
plot16 <- plot(confintemoipv, which = 85, col = "#002b58", xlab = "Share of Non Reported Common Crimes Against Men")

# Arrange the subplots in a grid
plots_grid <- arrangeGrob(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
                          plot10, plot11, plot12, plot13, plot14, plot15, plot16, ncol = 3)

# Save the combined plot using ggsave
ggsave(filename = "ci_plots.png",
       plot = plots_grid,
       dpi = 600,
       width = 200, height = 260,
       units = "mm", device = "png",
       bg = "transparent")











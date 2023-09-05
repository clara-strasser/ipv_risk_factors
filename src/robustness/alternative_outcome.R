###########################  IPV Outcome Alternative ###########################

# Initiate -----

## Load packages ------
library(dplyr)
library(tidyr)
library(purrr)

## Set path -----
path <- "/Users/clara/Desktop/master_thesis/data/"
path_data <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/"
path_save <- "/Users/clara/Desktop/master_thesis/r_projects/ipv_risk_factors/data/ipv/"

## Load data -------
load(paste0(path, "TB_SEC_IVaVD.RData")) # all women
load(paste0(path_data, "endireh_2021.RData")) # subset women (married/cohabiting, male partner, >= 1 child)

## Keep relevant observations -----
ids <- endireh_2021$ID_PER

## Subset ------
data <- TB_SEC_IVaVD[TB_SEC_IVaVD$ID_PER %in% ids,]

# Outcome Determination --------
# Remark: Factor analysis was used to determine the number of outcomes to be generated

## Coercive Control -----
# Questions:
# P14_3_15: Forbidding the woman to leave the house, locking her up, or stopping her from having visits.
# P14_3_18: Threatening the woman with a weapon (knife, blade, gun, or rifle) or with burning her.
# P14_3_19: Threatening to kill the woman, to kill himself or to kill the children.
# P14_3_20: Destroying, throwing away or hiding objects belonging to the woman or the household.
# Outcomes:
#             1 - muchas veces (yes)
#             2 - pocas veces (yes)
#             3 - una vez (yes)
#             4 - no ocurrio (no)
#             9 - not specified (NA)
#             b - blank (NA)
# Level:
# no (1), yes (2)

# Set columns
control <- c("P14_3_15", "P14_3_18", "P14_3_19", "P14_3_20")

## Surveillance -----
# Questions:
# P14_3_12: Accusing the woman of cheating on him.
# P14_3_16: Watching the woman, spying on her, following her, showing up to her unexpectedly.
# P14_3_17: Calling or texting the woman repeatedly to find out where she is, if she is with someone and what she is doing.
# P14_3_22: Monitoring the woman's e-mails or mobile phone and demanding her passwords.
# Outcomes:
#             1 - muchas veces (yes)
#             2 - pocas veces (yes)
#             3 - una vez (yes)
#             4 - no ocurrio (no)
#             9 - not specified (NA)
#             b - blank (NA)
# Level:
# no (1), yes (2)

# Set columns
surv <- c("P14_3_12", "P14_3_16", "P14_3_17", "P14_3_22")

## Emotional Abuse -----
# Questions:
# P14_3_10: Embarrassing, insulting, degrading or humiliating the woman (calling her ugly or comparing her to other women).
# P14_3_11: Ignoring, disregarding or lacking affection towards the woman.
# P14_3_13: Making the woman feel afraid.
# P14_3_14: Threatening to leave/abandon the woman, to harm her, to take away her children or to kick her out of the house.
# P14_3_23AB: Turning the woman's children or relatives against the woman.
# P14_3_24AB: Being angry with the woman because the chores are not done, because the food does not meet his expectations or because he feels that the woman has not fulfilled her obligations.
# Outcomes:
#             1 - muchas veces (yes)
#             2 - pocas veces (yes)
#             3 - una vez (yes)
#             4 - no ocurrio (no)
#             9 - not specified (NA)
#             b - blank (NA)
# Level:
# no (1), yes (2)

# Set columns
emo_abuse <- c("P14_3_10", "P14_3_11", "P14_3_13", "P14_3_14", "P14_3_23AB", "P14_3_24AB")

# Subset data ------
data <- data %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", control, surv, emo_abuse))


# Summary Stat -----
colSums(is.na(data)) 
sapply(data, unique) # NA, 1, 2, 3, 4, 9 
head(data[, c(control)], n = 35)
head(data[, c(surv)], n = 35)
head(data[, c(emo_abuse)], n = 35)

# Create Variables ----
data <- data %>%
  mutate(control_año = ifelse(rowSums(select(., any_of(control)) <= "3", na.rm = TRUE) > 0, "2", "1")) %>%
  mutate(control_año = factor(control_año, levels = c(1, 2), labels = c("no", "yes"))) %>%
  mutate(surv_año = ifelse(rowSums(select(., any_of(surv)) <= "3", na.rm = TRUE) > 0, "2", "1")) %>%
  mutate(surv_año = factor(surv_año, levels = c(1, 2), labels = c("no", "yes"))) %>%
  mutate(emo_abuse_año = ifelse(rowSums(select(., any_of(emo_abuse)) <= "3", na.rm = TRUE) > 0, "2", "1")) %>%
  mutate(emo_abuse_año = factor(emo_abuse_año, levels = c(1, 2), labels = c("no", "yes")))
  
# Test ----

## Control -----
head(data[, c(control, "control_año")], n = 65)
sum(is.na(data$control_año)) # 0 NAs
table(data$control_año)    
#  no    yes 
# 61479  1673 

## Surv -------
head(data[, c(surv, "surv_año")], n = 65)
sum(is.na(data$surv_año)) # 0 NAs
table(data$surv_año)    
#   no   yes 
# 57288  5864 

## Emotional Abuse --------
head(data[, c(emo_abuse, "emo_abuse_año")], n = 65)
sum(is.na(data$emo_abuse_año)) # 0 NAs
table(data$emo_abuse_año)    
#   no   yes 
# 52804 10348

# Keep relevant data ----
data <- data %>%
  select(c("ID_VIV", "ID_PER", "CVE_ENT", "CVE_MUN", "T_INSTRUM", "control_año", "surv_año", "emo_abuse_año"))

# Finalize ------

## Save data -----
emotional_ipv_alternative <- data
save(emotional_ipv_alternative, file = paste0(path_save,"emotional_ipv_alternative.RData"))



########################## TB_SEC_III Risk Factors #############################

# Initiate -----

## Load packages ------
library(dplyr)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------
load(paste0(path,"TB_SEC_III.RData"))

# Risk Factors -----

## RELATIONSHIP STATUS
# Remark: the relationship status was verified with preceding questions
# Variable name: P3_8
# Outcomes: 1 - cohabiting
#           2 - separated
#           3 - divorced
#           4 - widowed
#           5 - married
#           6 - single
#           b - blank
# Levels: cohabiting/married (1), other (2)
TSDem$rel_status <- ifelse(TSDem$P2_16 == "1" | TSDem$P2_16 == "5", 1, 2)
TSDem$rel_status <- factor(TSDem$rel_status, levels = c(1, 2), labels = c("cohabiting/married", "other"))
# Summary Stat:
table(TSDem$rel_status, useNA = "ifany")






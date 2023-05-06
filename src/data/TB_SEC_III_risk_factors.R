########################## TB_SEC_III Risk Factors #############################

# Initiate -----

## Load packages ------
library(dplyr)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------
load(paste0(path,"TB_SEC_III.RData"))

# Risk Factors -----

## RELATIONSHIP STATUS BEFORE VERIFICATION
# Remark: the relationship status was asked before asking verification questions
# Variable name: P3_1
# Outcomes: 1 - cohabiting
#           2 - separated
#           3 - divorced
#           4 - widowed
#           5 - married
#           6 - single
# Levels: cohabiting/married (1), other (2)
TB_SEC_III$rel_status_ver <- factor(ifelse(TB_SEC_III$P3_1 %in% c("1", "5"), "1", "2"),
                                levels = c("1", "2"), labels = c("cohabiting/married", "other"))
# Summary Stat:
table(TB_SEC_III$rel_status_ver, useNA = "ifany")
# cohabiting/married     other 
#     68574              41553       

## RELATIONSHIP STATUS AFTER VERIFICATION
# Remark: the relationship status was verified with preceding questions
# Variable name: P3_8
# Outcomes: A1 - cohabiting or married with partner resident
#           A2 - cohabiting or married with partner non-resident
#           B1 - separated or divorced
#           B2 - widowed
#           C1 - single with partner or ex-partner
#           C2 - single
# Levels: A1_cohabiting/married (1), A2_cohabiting/married (2), other (3)
TB_SEC_III$rel_status <- factor(ifelse(TB_SEC_III$P3_8 %in% c("A1"), "1",
                                       ifelse(TB_SEC_III$P3_8  == "A2", "2", "3")),
                                levels = c("1", "2", "3"), labels = c("A1_cohabiting/married", "A2_cohabiting/married", "other"))
table(TB_SEC_III$rel_status, useNA = "ifany")
# A1_cohabiting/married   A2_cohabiting/married      other 
#          66131                   2409              41587 



# REMARKS:
# Existing difference between P3_1 and P3_8
# Less married and cohabiting women AFTER asking the verification questions
# Difference: 34 women
# Women with A2 were asked additional questions (P4AB_2, P4A_1 and P4A_2) in the questionnaire 






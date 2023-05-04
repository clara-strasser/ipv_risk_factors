######################### Read Data and Save ##################################

# Prepare ----

## Set path ----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"
setwd(path)

## Load data ----
load("bd_endireh_2021.RData")

## Save separated ----
save(TB_SEC_III, file = "TB_SEC_III.RData")
save(TB_SEC_IVaVD, file = "TB_SEC_IVaVD.RData")
save(TSDem, file = "TSDem.RData")
save(TVIV, file = "TVIV.RData")




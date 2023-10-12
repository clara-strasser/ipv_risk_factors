######################### Read Data and Save ##################################

# Requirement:
# Download data set from: https://www.inegi.org.mx/programas/endireh/2021/
# The data set is saved in my path.

# Prepare ----

## Set path ----
path <- "/Users/clara/Desktop/master_thesis/data/" 

## Load data ----
load(paste0(path,"bd_endireh_2021.RData"))

## Save separated ----
save(TB_SEC_III, file = paste0(path,"TB_SEC_III.RData"))
save(TB_SEC_IVaVD, file = paste0(path,"TB_SEC_IVaVD.RData"))
save(TSDem, file = paste0(path,"TSDem.RData"))
save(TVIV, file = paste0(path,"TVIV.RData"))




######################### Read Data and Save ##################################
# Aim: Read main ENDIREH 2021 data set
# Requirement:
# Download data set from: https://www.inegi.org.mx/programas/endireh/2021/

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/ipv_data/raw_data"
path_data_endireh <- file.path(base_path, "main_source/")


# Load data  -------------------------------------------------------------------
load(paste0(path_data_endireh,"bd_endireh_2021.RData"))


# Save data  -------------------------------------------------------------------
save(TB_SEC_III, file = paste0(path_data_endireh,"TB_SEC_III.RData"))
save(TB_SEC_IVaVD, file = paste0(path_data_endireh,"TB_SEC_IVaVD.RData"))
save(TSDem, file = paste0(path_data_endireh,"TSDem.RData"))
save(TVIV, file = paste0(path_data_endireh,"TVIV.RData"))




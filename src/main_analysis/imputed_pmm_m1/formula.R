############################ Formula ################################


# Set Formula -------


## Model 1 ------
# Remarks: 
# Model similar to Munguia and Zarzoso (2022)
# Differences: 
# For: imputed data that has variable "vio_inf_par" and "vio_exp_inf_par"
# 1.) 2021 version of ENDIREH
# 2.) Inclusion of further controls
# 3.) Inclusion of terms in the interaction as parallel effects
# 4.) Exclusion of multicollinear variables

model <- vio_emo_año ~ 
  
  bols(intercept, intercept = FALSE) +  
  
  bols(edad_dif, intercept = FALSE) +                                                   
  bbs(edad_dif, knots = 20, df = 1, center = TRUE) +                                    
  
  bols(indigena, intercept = FALSE, df = 1) +   
  
  bols(edad_dif, by = indigena, intercept = FALSE) +                                    
  bbs(edad_dif, by = indigena, knots = 20, df = 1, center = TRUE) +                     
  
  bols(niv_edmedium, intercept = FALSE, df = 1) + 
  
  bols(niv_edhigh, intercept = FALSE, df = 1) + 
  
  bols(edad_dif, by = niv_edmedium, intercept = FALSE) +                                
  bbs(edad_dif, by = niv_edmedium, knots = 20, df = 1, center = TRUE) +   
  
  bols(edad_dif, by = niv_edhigh, intercept = FALSE) +                                  
  bbs(edad_dif, by = niv_edhigh, knots = 20, df = 1, center = TRUE) +                   
  
  bols(cct_rec, intercept = FALSE, df = 1) +
  
  bols(desempleo, intercept = FALSE, df = 1) + 
  
  bols(pareja_prev, intercept = FALSE, df = 1) + 
  
  bols(vio_inf, intercept = FALSE, df = 1) + 
  
  bols(vio_exp_inf, intercept = FALSE, df = 1) + 
  
  bols(vio_sex_inf, intercept = FALSE, df = 1) + 
  
  bols(num_hij, intercept = FALSE) +                                                
  bbs(num_hij, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(eda_hij, intercept = FALSE) +                                                
  bbs(eda_hij, knots = 20, df = 1, center = TRUE) +                                 
  
  bspatial(eda_hij, edad_dif, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_sex, intercept = FALSE) +                                                
  bbs(eda_sex, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(con_sex, intercept = FALSE, df = 1) + 
  
  bols(eda_sex, by = con_sex, intercept = FALSE) +                                  
  bbs(eda_sex, by = con_sex, knots = 20, df = 1, center = TRUE) +                   
  
  bspatial(eda_sex, edad_dif, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_mat, intercept = FALSE) +                                                
  bbs(eda_mat, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(mot_mat, intercept = FALSE, df = 1) +
  
  bols(eda_mat, by = mot_mat, intercept = FALSE) +                                  
  bbs(eda_mat, by = mot_mat, knots = 20, df = 1, center = TRUE) +                   
  
  bspatial(eda_mat, edad_dif, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(vio_inf_par, intercept = FALSE, df = 1) +
  
  bols(vio_exp_inf_par, intercept = FALSE, df = 1) +
  
  bols(hacin, intercept = FALSE) +                                                  
  bbs(hacin, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(act_distboth, intercept = FALSE, df = 1) +
  
  bols(act_distmales, intercept = FALSE, df = 1) +                                  
  
  bols(feminist_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(feminist_gradhigh, intercept = FALSE, df = 1) +                              
  
  bols(lib_sex_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(lib_sex_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(lib_eco_gradmedium, intercept = FALSE, df = 1) +  
  
  bols(lib_eco_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(lib_soc_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(lib_soc_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(redsoc_gradmedium, intercept = FALSE, df = 1) +     
  
  bols(redsoc_gradhigh, intercept = FALSE, df = 1) +                                
  
  bols(rout_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(rout_gradhigh, intercept = FALSE, df = 1) +                                  
  
  bols(log_ingm_muj, intercept = FALSE) +                                                
  bbs(log_ingm_muj, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(log_ingm_par, intercept = FALSE) +                                                
  bbs(log_ingm_par, knots = 20, df = 1, center = TRUE) +                                 
  
  bspatial(log_ingm_muj, log_ingm_par, center = TRUE, differences = 1, knots = 20, df = 1) +
  
  brandom(cvegeo, df = 1) +                                                         
  
  bols(mhr20, intercept = FALSE) +                                                  
  bbs(mhr20, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(fhr20, intercept = FALSE) +                                                  
  bbs(fhr20, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(phogjef_f, intercept = FALSE) +                                              
  bbs(phogjef_f, knots = 20, df = 1, center = TRUE) +                               
  
  bols(pres_2020_f, intercept = FALSE) +                                             
  bbs(pres_2020_f, knots = 20, df = 1, center = TRUE) +                              
  
  bols(gini20, intercept = FALSE) +                                                 
  bbs(gini20, knots = 20, df = 1, center = TRUE) +                                  
  
  bols(idh2020, intercept = FALSE) +                                                
  bbs(idh2020, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(pea_f, intercept = FALSE) +                                                  
  bbs(pea_f, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(Marg20low, intercept = FALSE, df = 1) +    
  
  bols(Marg20medium, intercept = FALSE, df = 1) +   
  
  bols(Marg20high, intercept = FALSE, df = 1) +      
  
  bols(Marg20very_high, intercept = FALSE, df = 1) +                                
  
  bols(Type_comlow_urban, intercept = FALSE, df = 1) +    
  
  bols(Type_commedium_urban, intercept = FALSE, df = 1) +    
  
  bols(Type_comhigh_urban, intercept = FALSE, df = 1) +                             
  
  bspatial(lon, lat, center = TRUE, differences = 1, knots = 20, df = 1) +              
  
  brandom(cveent, df = 1) +                                                         
  
  bols(FemPrev, intercept = FALSE) +                                                
  bbs(FemPrev, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(MasNoDen, intercept = FALSE) +                                               
  bbs(MasNoDen, knots = 20, df = 1, center = TRUE) +                                
  
  bols(FemNoDen, intercept = FALSE) +                                               
  bbs(FemNoDen, knots = 20, df = 1, center = TRUE) +                                
  
  bols(cor19, intercept = FALSE) +                                                  
  bbs(cor19, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(satis19, intercept = FALSE) +                                                
  bbs(satis19, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(ParPolF, intercept = FALSE) +                                                
  bbs(ParPolF, knots = 20, df = 1, center = TRUE) 

## Model 2 -----
# Remarks: 
# Model similar to Munguia and Zarzoso (2022)
# Differences: 
# For: non-imputed data without "vio_inf_par" and "vio_exp_inf_par"
# 1.) 2021 version of ENDIREH
# 2.) Inclusion of further controls
# 3.) Inclusion of terms in the interaction as parallel effects
# 4.) Exclusion of multicollinear variables
model <- vio_emo_año ~ 
  
  bols(intercept, intercept = FALSE) +  
  
  bols(edad_dif, intercept = FALSE) +                                                   
  bbs(edad_dif, knots = 20, df = 1, center = TRUE) +                                    
  
  bols(indigena, intercept = FALSE, df = 1) +   
  
  bols(edad_dif, by = indigena, intercept = FALSE) +                                    
  bbs(edad_dif, by = indigena, knots = 20, df = 1, center = TRUE) +                     
  
  bols(niv_edmedium, intercept = FALSE, df = 1) + 
  
  bols(niv_edhigh, intercept = FALSE, df = 1) + 
  
  bols(edad_dif, by = niv_edmedium, intercept = FALSE) +                                
  bbs(edad_dif, by = niv_edmedium, knots = 20, df = 1, center = TRUE) +   
  
  bols(edad_dif, by = niv_edhigh, intercept = FALSE) +                                  
  bbs(edad_dif, by = niv_edhigh, knots = 20, df = 1, center = TRUE) +                   
  
  bols(cct_rec, intercept = FALSE, df = 1) +
  
  bols(desempleo, intercept = FALSE, df = 1) + 
  
  bols(pareja_prev, intercept = FALSE, df = 1) + 
  
  bols(vio_inf, intercept = FALSE, df = 1) + 
  
  bols(vio_exp_inf, intercept = FALSE, df = 1) + 
  
  bols(vio_sex_inf, intercept = FALSE, df = 1) + 
  
  bols(num_hij, intercept = FALSE) +                                                
  bbs(num_hij, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(eda_hij, intercept = FALSE) +                                                
  bbs(eda_hij, knots = 20, df = 1, center = TRUE) +                                 
  
  bspatial(eda_hij, edad_dif, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_sex, intercept = FALSE) +                                                
  bbs(eda_sex, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(con_sex, intercept = FALSE, df = 1) + 
  
  bols(eda_sex, by = con_sex, intercept = FALSE) +                                  
  bbs(eda_sex, by = con_sex, knots = 20, df = 1, center = TRUE) +                   
  
  bspatial(eda_sex, edad_dif, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(eda_mat, intercept = FALSE) +                                                
  bbs(eda_mat, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(mot_mat, intercept = FALSE, df = 1) +
  
  bols(eda_mat, by = mot_mat, intercept = FALSE) +                                  
  bbs(eda_mat, by = mot_mat, knots = 20, df = 1, center = TRUE) +                   
  
  bspatial(eda_mat, edad_dif, center = TRUE, differences = 1, knots = 20, df = 1) +     
  
  bols(hacin, intercept = FALSE) +                                                  
  bbs(hacin, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(act_distboth, intercept = FALSE, df = 1) +
  
  bols(act_distmales, intercept = FALSE, df = 1) +                                  
  
  bols(feminist_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(feminist_gradhigh, intercept = FALSE, df = 1) +                              
  
  bols(lib_sex_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(lib_sex_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(lib_eco_gradmedium, intercept = FALSE, df = 1) +  
  
  bols(lib_eco_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(lib_soc_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(lib_soc_gradhigh, intercept = FALSE, df = 1) +                               
  
  bols(redsoc_gradmedium, intercept = FALSE, df = 1) +     
  
  bols(redsoc_gradhigh, intercept = FALSE, df = 1) +                                
  
  bols(rout_gradmedium, intercept = FALSE, df = 1) +    
  
  bols(rout_gradhigh, intercept = FALSE, df = 1) +                                  
  
  bols(log_ingm_muj, intercept = FALSE) +                                                
  bbs(log_ingm_muj, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(log_ingm_par, intercept = FALSE) +                                                
  bbs(log_ingm_par, knots = 20, df = 1, center = TRUE) +                                 
  
  bspatial(log_ingm_muj, log_ingm_par, center = TRUE, differences = 1, knots = 20, df = 1) +
  
  brandom(cvegeo, df = 1) +                                                         
  
  bols(mhr20, intercept = FALSE) +                                                  
  bbs(mhr20, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(fhr20, intercept = FALSE) +                                                  
  bbs(fhr20, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(phogjef_f, intercept = FALSE) +                                              
  bbs(phogjef_f, knots = 20, df = 1, center = TRUE) +                               
  
  bols(pres_2020_f, intercept = FALSE) +                                             
  bbs(pres_2020_f, knots = 20, df = 1, center = TRUE) +                              
  
  bols(gini20, intercept = FALSE) +                                                 
  bbs(gini20, knots = 20, df = 1, center = TRUE) +                                  
  
  bols(idh2020, intercept = FALSE) +                                                
  bbs(idh2020, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(pea_f, intercept = FALSE) +                                                  
  bbs(pea_f, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(Marg20low, intercept = FALSE, df = 1) +    
  
  bols(Marg20medium, intercept = FALSE, df = 1) +   
  
  bols(Marg20high, intercept = FALSE, df = 1) +      
  
  bols(Marg20very_high, intercept = FALSE, df = 1) +                                
  
  bols(Type_comlow_urban, intercept = FALSE, df = 1) +    
  
  bols(Type_commedium_urban, intercept = FALSE, df = 1) +    
  
  bols(Type_comhigh_urban, intercept = FALSE, df = 1) +                             
  
  bspatial(lon, lat, center = TRUE, differences = 1, knots = 20, df = 1) +              
  
  brandom(cveent, df = 1) +                                                         
  
  bols(FemPrev, intercept = FALSE) +                                                
  bbs(FemPrev, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(MasNoDen, intercept = FALSE) +                                               
  bbs(MasNoDen, knots = 20, df = 1, center = TRUE) +                                
  
  bols(FemNoDen, intercept = FALSE) +                                               
  bbs(FemNoDen, knots = 20, df = 1, center = TRUE) +                                
  
  bols(cor19, intercept = FALSE) +                                                  
  bbs(cor19, knots = 20, df = 1, center = TRUE) +                                   
  
  bols(satis19, intercept = FALSE) +                                                
  bbs(satis19, knots = 20, df = 1, center = TRUE) +                                 
  
  bols(ParPolF, intercept = FALSE) +                                                
  bbs(ParPolF, knots = 20, df = 1, center = TRUE) 


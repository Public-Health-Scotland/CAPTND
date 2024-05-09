
##################################.
### Label impossible combis NA ###
##################################.

# Author: Charlie Smith
# Date: 2024-05-08


label_impossible_combis_na <- function(df){
  
  df_corrected <- df
  
  vec_cols <- c("count", "total", "proportion") # columns to alter
  
  for(i in 1:length(vec_cols)){ # loop through each specified column and make value NA
    
    df_corrected <- df_corrected %>% 
      mutate(!!sym(vec_cols[i]) := case_when(
        dataset_type == "CAMHS" & hb_name == "NHS 24" ~ NA_integer_, 
        dataset_type == "CAMHS" & variable %in% c("preg_perinatal_ref", "preg_perinatal_app", "vet", "act_code_sent_date", 
                                             "treat_group_or_ind_1","treat_group_or_ind_2","treat_group_or_ind_3") ~ NA_integer_,
        dataset_type == "PT" & variable == "looked_after_c" ~ NA_integer_,
        TRUE ~ !!sym(vec_cols[i])))
    
  } 
  
  df_corrected <- df_corrected %>%
    mutate(proportion = case_when( # set NaN proportions (0/0) to either 100 or 0
      value == "missing" & is.nan(proportion) ~ 100, 
      value %in% vec_value & is.nan(proportion) ~ 0,
      #Variable %in% c("Measures1", "Measures2", "Measures3") & 
      #  HEADER_REF_MONTH >= "2023-10-01" ~ NA_real_,
      TRUE ~ proportion)
    )
  
  
  return(df_corrected)
  
}
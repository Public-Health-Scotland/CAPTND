
########################################.
### Add HB Patient Management System ###
########################################.

# Author: Charlie Smith
# Date: 2024-05-08

append_patient_management_system <- function(df){
  
  pms_lookup <- import('../../../data/PMS_HB_latest.xlsx') |> 
    rename(dataset_type = DATASET,
           hb_name = HB,
           pms = PMS) |> 
    correct_hb_names_simple() |> 
    select(1:3)
  
  df_pms <- df |> 
    left_join(pms_lookup, by = c("dataset_type", "hb_name")) |> 
    select(header_date_month, submission_status, dataset_type, hb_name, pms, everything())
  
  return(df_pms)
  
}


  
  
  


########################################.
### Assess Activation Code Sent Date ###
########################################.

# Author: Charlie Smith
# Date: 2024-04-12


assess_act_code_sent_date <- function(df){
  
  df_act_code_sent_date <- df |> 
    mutate(check_act_code_sent_date = 
             if_else(!!sym(dataset_type_o) == "PT", case_when(
               is.na(!!sym(act_code_sent_date_o)) ~ "missing",
               (!is.na(!!sym(ref_rec_date_o)) & !!sym(act_code_sent_date_o) < !!sym(ref_rec_date_o)) ~ "invalid",
               !!sym(act_code_sent_date_o) >= "2015-01-01" & !!sym(act_code_sent_date_o) <= Sys.Date() ~ "valid",
               TRUE ~ "invalid"), NA_character_))
  
  return(df_act_code_sent_date)
  
}

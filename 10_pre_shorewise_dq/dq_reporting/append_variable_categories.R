
##################################.
### Update variable categories ###
##################################.

# Author: Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2024-08-22


append_variable_categories <- function(df){
  
  df_var_cat <- df |> 
    mutate(var_cat = case_when(
      
      variable %in% c("ucpn", "chi", "upi") ~ "id_vars",
      
      variable %in% c("sex", "dob", "ethnicity", "looked_after_c", "protection", "vet", 
                      "preg_perinatal_ref", "postcode", 
                      "ref_date", "ref_rec_date", "ref_reason", "ref_acc",  "ref_rej_act", 
                      "ref_rej_date", "ref_source", "rej_reason", "act_code_sent_date") ~ "ref_vars",
      
      variable %in% c("app_date", "app_purpose", "att_cat", "att_status", "location", 
                      "prof_group", "preg_perinatal_app") ~ "app_vars",
      
      variable %in% c("unav_date_end", "unav_date_start", "unav_days_no", "unav_reason") ~ "una_vars",
      
      variable %in% c("diag_1", "diag_2", "diag_3", 
                      "treat_1", "treat_2", "treat_3", 
                      "treat_group_or_ind_1", "treat_group_or_ind_2", "treat_group_or_ind_3", 
                      "cgi_i", "pgi_i", "cgi_s",
                      "treat_start_date") ~ "diag_vars",
      
      variable %in% c("case_closed_date") ~ "dis_vars",
      
      TRUE ~ NA_character_),
      
      var_cat = factor(var_cat, levels = c("id_vars", "ref_vars", "app_vars",
                                           "una_vars", "diag_vars", "dis_vars", NA_character_)))
  
  return(df_var_cat)
  
}


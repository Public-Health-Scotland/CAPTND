
#adds column started treatment

add_started_treat_status <- function(df){
  df_eval <- df %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(!!had_first_treat_appt_o = case_when(any(!!sym(att_status_o)==1 & !!sym(app_purpose_o) %in% c(2,3,5)) ~ TRUE, #patient attended treat appt
                                            TRUE ~ FALSE), 
           .after=!!rtt_eval_o
    ) %>% 
    ungroup()
  
  return(df_eval)
}

############################################.
### Assess Appointment Attendance Status ###
############################################.

# Author: Bex Madden
# Date: 2024-04-11

assess_att_status_pub <- function(df){
  
  # load lookup tables
  list_att_status <- readxl::read_xlsx(captnd_code_lookup, sheet = "Attend_Status") %>% 
    select(STATUS) %>% 
    mutate(STATUS = str_pad(STATUS, 2, pad = "0")) %>% 
    filter(STATUS != "99") %>% 
    pull() |> as.numeric()
  
  # assess values
  df_att_status <- df |> 
    mutate(!!sym(att_status_o) := str_pad(!!sym(att_status_o), 2, pad = "0"),
           check_att_status = case_when(
             is.na(!!sym(att_status_o)) ~ "missing",
             !!sym(att_status_o) %in% list_att_status ~ "valid", 
             !!sym(att_status_o) == 99 ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_att_status)
  
}



##################################.
### Assess Attendance Category ###
##################################.

# Author: Bex Madden
# Date: 2024-04-11

assess_att_cat <- function(df){
  
  # load lookup tables
  list_att_cat <- readxl::read_xlsx(captnd_code_lookup, sheet = "Attend_Cat") %>% 
    select(Codes) %>% 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) %>% 
    pull()
  
  # assess values
  df_att_cat <- df |> 
    mutate(!!sym(att_cat_o) := str_pad(!!sym(att_cat_o), 2, pad = "0"),
           check_att_cat = case_when(
             is.na(!!sym(att_cat_o)) ~ "missing",
             !!sym(att_cat_o) %in% list_att_cat ~ "known", 
             TRUE ~ "invalid"))
  
  return(df_att_cat)
  
}

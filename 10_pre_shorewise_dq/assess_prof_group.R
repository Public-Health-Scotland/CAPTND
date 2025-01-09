
#################################.
### Assess Professional Group ###
#################################.

# Author: Bex Madden
# Date: 2024-04-11

assess_prof_group <- function(df){
  
  # dates prior to 2020 used previous version (v 9.3) of professional codes
  # not recoded here as old records are effectively defunct
  
  # load lookup tables
  list_progroup <- readxl::read_xlsx(captnd_code_lookup, sheet = "Pro_Group") %>% 
    select(Code) %>% 
    mutate(Code = str_pad(Code, 2, pad = "0")) %>% 
    filter(! Code %in% c("98" , "99")) %>% 
    pull()
  
  # assess values
  df_prof_group <- df |> 
    mutate(!!sym(prof_group_o) := str_pad(!!sym(prof_group_o), 2, pad = "0"),
           check_prof_group = case_when(
             is.na(!!sym(prof_group_o)) ~ "missing",
             !!sym(prof_group_o) %in% list_progroup ~ "valid", 
             !!sym(prof_group_o)%in% c("98", "99") ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_prof_group)
  
}

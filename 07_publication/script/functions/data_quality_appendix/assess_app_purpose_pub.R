

##################################.
### Assess Appointment Purpose Pub ###
##################################.

# Author: Bex Madden
# Date: 2024-04-11

assess_app_purpose_pub <- function(df){
  
  # lookup purpose codes
  list_app_purpose <- readxl::read_xlsx(captnd_code_lookup, sheet = "App_Purpose") %>% 
    select(PURPOSE) %>% 
    mutate(PURPOSE = str_pad(PURPOSE, 2, pad = "0")) %>% 
    filter(PURPOSE != "99") %>% 
    pull() |> as.numeric()
  
  # assess values
  df_app_purpose <- df |> 
    mutate(!!sym(app_purpose_o) := str_pad(!!sym(app_purpose_o), 2, pad = "0"),
           check_app_purpose = case_when(
             is.na(!!sym(app_purpose_o)) ~ "missing",
             !!sym(app_purpose_o) %in% list_app_purpose ~ "valid",
             !!sym(app_purpose_o) == 99 ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_app_purpose)
  
}


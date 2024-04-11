
###################################.
### Assess Appointment Location ###
###################################.

# Author: Bex Madden
# Date: 2024-04-11

assess_app_location <- function(df){
  
  # load lookup tables
  list_location <- readxl::read_xlsx(captnd_code_lookup, sheet = "Location") |>
    select(Code) |>
    mutate(Code = str_pad(Code, 2, pad = "0")) |>
    filter(! Code %in% c("98","99")) |>
    pull()
  
  # assess values
  df_app_location <- df |> 
    mutate(!!sym(location_o) := str_pad(!!sym(location_o), 2, pad = "0"),
           check_location = case_when(
             is.na(!!sym(location_o)) ~ "missing",
             !!sym(location_o) %in% list_location ~ "valid", 
             !!sym(location_o) %in% c("98", "99") ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_app_location)
  
}

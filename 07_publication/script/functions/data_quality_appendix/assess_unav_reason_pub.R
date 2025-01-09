

####################################.
### Assess Unavailability Reason Pub ###
####################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_unav_reason_pub <- function(df){
  
  # load lookup tables
  list_una_reason <- readxl::read_xlsx(captnd_code_lookup, sheet = "Una_Reason") |>
    select(Codes) |>
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |>
    filter(Codes != "99") |>
    pull() |> as.numeric()
  
  # assess values
  df_unav_reason <- df |> 
    mutate(!!sym(unav_reason_o) := str_pad(!!sym(unav_reason_o), 2, pad = "0"),
           check_unav_reason = case_when(
             is.na(!!sym(unav_reason_o)) ~ "missing",
             !!sym(unav_reason_o) %in% list_una_reason ~ "known", 
             !!sym(unav_reason_o) == 99 ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_unav_reason)
  
}

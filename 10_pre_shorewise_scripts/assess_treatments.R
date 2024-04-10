############################.
### Assess Treatment 1-3 ###
############################.

# Author: Bex Madden
# Date: 2024-04-10

assess_treatments <- function(df){
  
  # lookup treatment codes
  list_treatment <- readxl::read_xlsx(captnd_code_lookup, sheet = "Treatment") |> 
    select(Codes) |>  
    filter(! Codes %in% c("99")) |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |>  
    pull()
  
  list_treatment_updated <- readxl::read_xlsx(captnd_code_lookup, sheet = "Treatment") |> 
    filter(! Codes %in% c("99", "98")) |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |> 
    pull()
  
  # assess values
  df_treatment <- df |> 
    mutate(!!sym(treat_1_o) := str_pad(!!sym(treat_1_o), 2, pad = "0"),
           check_treat_1 = case_when(
             !!sym(header_date_o) < "2022-06-01" & !!sym(treat_1_o) %in% list_treatment ~ "valid",
             !!sym(header_date_o) >= "2022-06-01" & !!sym(treat_1_o) %in% list_treatment_updated ~ "valid",
             !!sym(treat_1_o) %in% c("99", "98") ~ "not known",
             is.na(!!sym(treat_1_o)) ~ "missing",
             TRUE ~ "invalid"),
           
           !!sym(treat_2_o) := str_pad(!!sym(treat_2_o), 2, pad = "0"),
           check_treat_2 = case_when(
             !!sym(header_date_o) < "2022-06-01" & !!sym(treat_2_o) %in% list_treatment ~ "valid",
             !!sym(header_date_o) >= "2022-06-01" & !!sym(treat_2_o) %in% list_treatment_updated ~ "valid",
             !!sym(treat_2_o) %in% c("99", "98") ~ "not known",
             is.na(!!sym(treat_2_o)) ~ "missing",
             TRUE ~ "invalid"),
           
           !!sym(treat_3_o) := str_pad(!!sym(treat_3_o), 2, pad = "0"),
           check_treat_3 = case_when(
             !!sym(header_date_o) < "2022-06-01" & !!sym(treat_3_o) %in% list_treatment ~ "valid",
             !!sym(header_date_o) >= "2022-06-01" & !!sym(treat_3_o) %in% list_treatment_updated ~ "valid",
             !!sym(treat_3_o) %in% c("99", "98") ~ "not known",
             is.na(!!sym(treat_3_o)) ~ "missing",
             TRUE ~ "invalid"))
  
  return(df_treatment)
  
}


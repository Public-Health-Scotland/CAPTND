#################################################.
### Assess Treatment: Group or Individual 1-3 Pub ###
#################################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_group_or_inds_pub <- function(df){
  
  #lookup codes
  list_group_ind <- readxl::read_xlsx(captnd_code_lookup, sheet = "Group_Ind") |>   ## ERROR - object 'lookup' not found
    select(Codes) |>
    filter(Codes != c("99")) |>
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |>
    pull() |> as.numeric()
  
  # assess values
  df_group_ind <- df |> 
    mutate(!!sym(treat_group_or_ind_1_o) := str_pad(!!sym(treat_group_or_ind_1_o), 2, pad = "0"),
           check_treat_group_or_ind_1 = case_when(
             is.na(!!sym(treat_group_or_ind_1_o)) ~ "missing",
             !!sym(treat_group_or_ind_1_o) %in% list_group_ind ~ "valid",
             !!sym(treat_group_or_ind_1_o) == 99 ~ "not known",
             TRUE ~ "invalid"),
           
           !!sym(treat_group_or_ind_2_o) := str_pad(!!sym(treat_group_or_ind_2_o), 2, pad = "0"),
           check_treat_group_or_ind_2 = case_when(
             is.na(!!sym(treat_group_or_ind_2_o)) ~ "missing",
             !!sym(treat_group_or_ind_2_o) %in% list_group_ind ~ "valid",
             !!sym(treat_group_or_ind_2_o) == 99 ~ "not known",
             TRUE ~ "invalid"),
           
           !!sym(treat_group_or_ind_3_o) := str_pad(!!sym(treat_group_or_ind_3_o), 2, pad = "0"),
           check_treat_group_or_ind_3 = case_when(
             is.na(!!sym(treat_group_or_ind_3_o)) ~ "missing",
             !!sym(treat_group_or_ind_3_o) %in% list_group_ind ~ "valid",
             !!sym(treat_group_or_ind_3_o) == 99 ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_group_ind)
  
}

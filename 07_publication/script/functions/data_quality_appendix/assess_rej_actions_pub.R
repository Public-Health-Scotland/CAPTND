
################################.
### Assess Rejection Actions Pub ###
################################.

# Author: Charlie Smith
# Date: 2024-04-12

assess_rej_actions_pub <- function(df){
  
  vec_rej_actions <- read_xlsx(captnd_code_lookup, sheet = "Rej_Action") |>  
    select(REJ_ACTIONS) |>
    filter(! REJ_ACTIONS %in% c("7")) |>
    mutate(REJ_ACTIONS = str_pad(REJ_ACTIONS, 2, pad = "0")) |>
    pull() |> as.numeric()
  
  df_rej_actions <- df |> 
    mutate(check_ref_rej_act = case_when(
      !!sym(ref_acc_o) == 1 & is.na(!!sym(ref_rej_act_o))  ~ "missing but valid",
      !!sym(ref_acc_o) == 2 & is.na(!!sym(ref_rej_act_o))  ~ "missing",
      !!sym(ref_acc_o) == 2 & !!sym(ref_rej_act_o) %in% vec_rej_actions ~ "valid",
      !!sym(ref_acc_o) == 3 & is.na(!!sym(ref_rej_act_o)) ~ "missing but valid",
      is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_rej_act_o)) ~ "missing",
      !!sym(ref_rej_act_o) == 7 ~ "not known",
      TRUE ~ "invalid"))  
  
  # testing only
  # mutate(month = floor_date(header_date, unit = "month")) |> 
  # filter(month == "2024-08-01" & ucpn == "27870998") |> 
  # select(dataset_type, hb_name, chi, upi, ucpn, ref_acc, ref_rej_act, check_ref_rej_act)
  
  
  return(df_rej_actions)
  
}

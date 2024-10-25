
################################.
### Assess Rejection Actions ###
################################.

# Author: Charlie Smith
# Date: 2024-04-12

assess_rej_actions <- function(df){
  
  vec_rej_actions <- read_xlsx(captnd_code_lookup, sheet = "Rej_Action") |>  
    select(REJ_ACTIONS) |>
    filter(! REJ_ACTIONS %in% c("7")) |>
    mutate(REJ_ACTIONS = str_pad(REJ_ACTIONS, 2, pad = "0")) |>
    pull()
  
  df_rej_actions <- df |> 
    mutate(check_ref_rej_act = case_when(
      !!sym(ref_acc_o) == '01' & is.na(!!sym(ref_rej_act_o))  ~ "missing but valid",
      !!sym(ref_acc_o) == '02' & is.na(!!sym(ref_rej_act_o))  ~ "missing",
      !!sym(ref_acc_o) == '02' & !!sym(ref_rej_act_o) %in% vec_rej_actions ~ "valid",
      !!sym(ref_acc_o) == '03' & is.na(!!sym(ref_rej_act_o)) ~ "missing but valid",
      is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_rej_act_o)) ~ "missing",
      !!sym(ref_acc_o) == "07" ~ "not known",
      TRUE ~ "invalid"))
  
  return(df_rej_actions)
  
}

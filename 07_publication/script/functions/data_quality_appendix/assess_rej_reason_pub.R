
###############################.
### Assess Rejection Reason Pub ###
###############################.

# Author: Charlie Smith
# Date: 2024-04-11

assess_rej_reason_pub <- function(df){
  
  vec_rej_reason <- read_xlsx(captnd_code_lookup, sheet = "Rej_Reason") %>% 
    select(REJ_REASON) %>% 
    filter(! REJ_REASON %in% c("6", "7")) %>% 
    mutate(REJ_REASON = str_pad(REJ_REASON, 2, pad = "0")) %>% 
    pull() |> as.numeric()
  
  df_rej_reason <- df |> 
    mutate(check_rej_reason = case_when(
      !!sym(ref_acc_o) == 1 & is.na(!!sym(ref_rej_reason_o)) ~ "missing but valid", # accepted and no rejection reason = valid
      !!sym(ref_acc_o) == 2 & is.na(!!sym(ref_rej_reason_o)) ~ "missing", # rejected & rej reason is missing = missing
      !!sym(ref_acc_o) == 2 & !!sym(ref_rej_reason_o) %in% vec_rej_reason ~ "valid",# rej reason is included in list of valid reasons = valid
      !!sym(ref_acc_o) == 3 & is.na(!!sym(ref_rej_reason_o)) ~ "missing but valid", # pending and rej reason is missing = valid
      is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_rej_reason_o)) ~ "missing",
      !!sym(ref_rej_reason_o) %in% c(6, 7) ~ "not known",
      TRUE ~ "invalid"))
  
  return(df_rej_reason)
  
}
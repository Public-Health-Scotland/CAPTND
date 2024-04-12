
###############################.
### Assess Rejection Reason ###
###############################.

# Author: Charlie Smith
# Date: 2024-04-11

assess_rej_reason <- function(df){
  
  vec_rej_reason <- read_xlsx(captnd_code_lookup, sheet = "Rej_Reason") %>% 
    select(REJ_REASON) %>% 
    filter(! REJ_REASON %in% c("6", "7")) %>% 
    mutate(REJ_REASON = str_pad(REJ_REASON, 2, pad = "0")) %>% 
    pull()
  
  df_rej_reason <- df |> 
    mutate(check_rej_reason = case_when(
      !!sym(ref_acc_o) == '01' & is.na(!!sym(ref_rej_reason_o)) ~ "missing but valid", # accepted and no rejection reason = valid
      !!sym(ref_acc_o) == '02' & is.na(!!sym(ref_rej_reason_o)) ~ "missing", # rejected & rej reason is missing = missing
      !!sym(ref_acc_o) == '02' & !!sym(ref_rej_reason_o) %in% vec_rej_reason ~ "valid",# rej reason is included in list of valid reasons = valid
      !!sym(ref_acc_o) == '03' & is.na(!!sym(ref_rej_reason_o)) ~ "missing but valid", # pending and rej reason is missing = valid
      is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_rej_reason_o)) ~ "missing",
      !!sym(ref_rej_reason_o) %in% c("06", "07") ~ "not known",
      TRUE ~ "Invalid"))
    
  return(df_rej_reason)
  
}

################################.
### Assess Referral Accepted ###
################################.

# Author: Charlie Smith
# Date: 2024-04-11

assess_ref_acc <- function(df){
  
  vec_accepted <- read_xlsx(captnd_code_lookup, sheet = "Ref_Accepted") %>% 
    select(Code) %>% 
    mutate(Code = str_pad(Code, 2, pad = "0")) %>% 
    pull()
  
  df_ref_acc <- df %>% 
    mutate(check_ref_acc = case_when(
             is.na(!!sym(ref_acc_o)) ~ "missing",
             !!sym(ref_acc_o) %in% vec_accepted ~ "valid",
             TRUE ~ "invalid"))
  
  return(df_ref_acc)
  
}




##############################.
### Assess Referral Reason Pub ###
##############################.

# Author: Charlie Smith
# Date: 2024-04-11


assess_ref_reason_pub <- function(df){
  
  vec_ref_reason <- read_xlsx(captnd_code_lookup, sheet = "Ref_Reason") %>% 
    select(Code) %>% 
    mutate(Code = str_pad(Code, 2, pad = "0")) %>% 
    filter(! Code %in% c("98", "99")) %>% 
    pull() |> as.numeric()
  
  df_ref_reason <- df %>% 
    mutate(check_ref_reason = case_when(
      is.na(!!sym(ref_reason_o)) ~ "missing",
      !!sym(ref_reason_o) %in% c(98, 99) ~ "not known",  
      !!sym(ref_reason_o) %in% vec_ref_reason ~ "valid",
      TRUE ~ "invalid"))
  
  return(df_ref_reason)
  
}


#############################.
### Assess Rejection Reason ###
#############################.

# Author: Charlie Smith
# Date: 2024-04-11

assess_rej_reason <- function(df){
  
  vec_rej_reason <- read_xlsx(captnd_code_lookup, sheet = "Rej_Reason") %>% 
    select(REJ_REASON) %>% 
    filter(! REJ_REASON %in% c("6", "7")) %>% 
    mutate(REJ_REASON = str_pad(REJ_REASON, 2, pad = "0")) %>% 
    pull()
  
  df_rej_reason <- df
    mutate(check_rej_reason = case_when(
      
      ))
    
  return(df_rej_reason)
  
}
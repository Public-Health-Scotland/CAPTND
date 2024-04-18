
##############################.
### Assess Referral Source ###
##############################.

# Author: Charlie Smith
# Date: 2024-04-11


assess_ref_source <- function(df){
  
  vec_ref_source <- read_xlsx(captnd_code_lookup, sheet = "Ref_Source") %>% 
    select(Code) %>% 
    mutate(Code = str_pad(Code, 2, pad = "0")) %>% 
    filter(! Code %in% c("98", "99")) %>% 
    pull()
  
  df_ref_source <- df |> 
    mutate(check_ref_source = case_when(
             is.na(!!sym(ref_source_o)) ~ "missing",
             !!sym(ref_source_o) %in% c("98", "99") ~ "not known",  
             !!sym(ref_source_o) %in% vec_ref_source ~ "valid",
             TRUE ~ "invalid"))
  
  return(df_ref_source)
  
}

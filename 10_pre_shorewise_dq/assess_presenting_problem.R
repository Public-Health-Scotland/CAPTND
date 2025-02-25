
#################################.
### Assess presenting problem ###
#################################.

# Author: Charlie Smith
# Date: 2025-02-20


assess_presenting_prob <- function(df){
  
  lookup_values <- readxl::read_xlsx(captnd_code_lookup, sheet = "Presenting_Problem") %>% 
    select(Codes) %>% 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) %>% 
    filter(! Codes %in% c("96", "98", "99")) %>% 
    pull()
  
  df_presenting_prob <- df |> 
    mutate(check_presenting_prob_1 = case_when(
      !!sym(presenting_prob_1_o) %in% lookup_values ~ "valid",
      !!sym(presenting_prob_1_o) %in% c("96", "98", "99") ~ "not known",
      is.na(!!sym(presenting_prob_1_o)) ~ "missing",
      TRUE ~ "invalid"),
      
      check_presenting_prob_2 = case_when(
        !!sym(presenting_prob_2_o) %in% lookup_values ~ "valid",
        !!sym(presenting_prob_2_o) %in% c("96", "98", "99") ~ "not known",
        is.na(!!sym(presenting_prob_2_o)) ~ "missing",
        TRUE ~ "invalid"),
      
      check_presenting_prob_3 = case_when(
        !!sym(presenting_prob_3_o) %in% lookup_values ~ "valid",
        !!sym(presenting_prob_3_o) %in% c("96", "98", "99") ~ "not known",
        is.na(!!sym(presenting_prob_3_o)) ~ "missing",
        TRUE ~ "invalid")
      
      )
  
  return(df_presenting_prob)
  
}


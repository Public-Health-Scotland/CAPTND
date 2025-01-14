
#################################.
### Assess global impressions ###
#################################.

# Author: Charlie Smith
# Date: 2024-12-19

assess_global_impressions <- function(df){
  
  vec_glob_imp <- c("01", "02", "03", "04", "05", "06", "07")
  
  df_assessed <- df |> 
    mutate(
      check_cgi_i = case_when(
        !!sym(cgi_i_o) %in% vec_glob_imp ~ "known", 
        !!sym(cgi_i_o) == "00" ~ "not known",
        is.na(!!sym(cgi_i_o)) ~ "missing",
        TRUE ~ "invalid"),
      check_pgi_i = case_when(
        !!sym(pgi_i_o) %in% vec_glob_imp ~ "known", 
        !!sym(pgi_i_o) == "00" ~ "not known",
        is.na(!!sym(pgi_i_o)) ~ "missing",
        TRUE ~ "invalid"),
      check_cgi_s = case_when(
        !!sym(cgi_s_o) %in% vec_glob_imp ~ "known", 
        !!sym(cgi_s_o) == "00" ~ "not known",
        is.na(!!sym(cgi_s_o)) ~ "missing",
        TRUE ~ "invalid"))
  
  return(df_assessed)
  
  }
  


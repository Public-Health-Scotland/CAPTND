
##################.
### Assess Sex Pub ###
##################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_sex_pub <- function(df){
  
  # valid sex codes
  vec_sex <- readxl::read_xlsx(captnd_code_lookup, sheet = "Sex") %>% 
    select(sex = Sex) %>% 
    mutate(sex = str_pad(sex, 2, pad = "0")) %>% 
    filter(sex != "09") %>% # remove "not specified" as == "not known"
    pull(.) |> as.numeric()
  
  
  df_sex <- df |> 
    mutate(check_sex = case_when(
      !!sym(sex_o) %in% vec_sex ~ "valid",
      !!sym(sex_o) == 9 ~ "not known",
      is.na(!!sym(sex_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_sex)
  
}


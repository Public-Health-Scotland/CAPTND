
###########################.
### Assess Sex Reported ###
###########################.

# Author: CJS
# Date: 2024-12-03


assess_sex_reported <- function(df){
  
  vec_sex <- readxl::read_xlsx(captnd_code_lookup, sheet = "Sex") %>% 
      select(sex = Sex) %>% 
      mutate(sex = str_pad(sex, 2, pad = "0")) %>% 
      filter(sex != "09") %>% # remove "not specified" as == "not known"
      pull() |> as.numeric()
  
  df_sex_reported <- df |> 
    mutate(check_sex_reported = case_when(
      !!sym(sex_reported_o) %in% vec_sex ~ "valid",
      !!sym(sex_reported_o) == 09 ~ "not known",
      is.na(!!sym(sex_reported_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_sex_reported)
  
}
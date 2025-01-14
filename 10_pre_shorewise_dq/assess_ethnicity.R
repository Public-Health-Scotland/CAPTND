
########################.
### Assess Ethnicity ###
########################. 

# Author: Charlie Smith
# Date: 2024-04-05


assess_ethnicity <- function(df){
  
  vec_ethnicity <- read_xlsx(captnd_code_lookup, sheet = "Ethnicity") %>% 
  select(ETHNICITY) %>% 
  filter(! ETHNICITY %in% c("98", "99", "6Z")) %>% 
  pull()

  # ethnicity codes updated after 24th of May 2023 (applies to data subs after 1st April 2023)
  vec_ethnicity_updated <- read_xlsx(captnd_code_lookup, sheet = "Ethnicity_code_update") %>% 
    select(ETHNICITY) %>% 
    filter(! ETHNICITY %in% c("98", "99", "6Z")) %>% 
    pull()
  
  # check variable
  df_ethnicity <- df %>% 
    mutate(check_ethnicity = case_when(
             is.na(!!sym(ethnicity_o)) ~ "missing",
             !!sym(header_date_o) < "2023-04-01" &  !!sym(ethnicity_o) %in% vec_ethnicity ~ "valid", # BEFORE 1st April 2023, use old codes
             !!sym(header_date_o) >= "2023-04-01" &  !!sym(ethnicity_o) %in% vec_ethnicity_updated ~ "valid", # AFTER 1st April 2023, use new codes
             !!sym(ethnicity_o) %in% c("98", "99", "6Z") ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_ethnicity)
  
}










#################################################.
### Assess Pregnant/Perinatal in Appointments ###
#################################################.

# Author: Bex Madden
# Date: 2024-04-11

assess_preg_perinatal_app <- function(df){
  
  # load lookup tables
  list_preg_perinatal <- readxl::read_xlsx(captnd_code_lookup, sheet = "Pregnancy") %>%
    select(Code) %>%
    filter(Code != "99") %>% 
    mutate(Code = str_pad(Code, 2, pad = "0")) %>%
    pull()
  
    # assess values
  df_preg_perinatal_app <- df |> 
    mutate(!!sym(preg_perinatal_app_o) := str_pad(!!sym(preg_perinatal_app_o), 2, pad = "0"),
           check_preg_perinatal_app = ifelse(!!sym(dataset_type_o) == "PT",
                                             case_when(
                                               is.na(!!sym(preg_perinatal_app_o)) ~ "missing",
                                               !!sym(preg_perinatal_app_o) %in% list_preg_perinatal ~ "valid",
                                               !!sym(preg_perinatal_app_o) == "99" ~ "not known",
                                               TRUE ~ "invalid"),
                                             NA_character_))
  return(df_preg_perinatal_app)
  
}

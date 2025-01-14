
####################################.
### Assess Veteran Status Edited ###
####################################. 

# Author: Charlie Smith
# Date: 2024-12-03

assess_vet_status_edited <- function(df){
  
  vec_veteran <- read_xlsx(captnd_code_lookup, sheet = "Vets") |> 
    select(Codes) |> 
    filter(Codes != "99") |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |> 
    pull()
  
  df_vet <- df %>% 
    mutate(check_vet_edited = if_else(!!sym(dataset_type_o) == "PT", case_when(
      is.na(!!sym(vet_edited_o)) ~ "missing",
      !!sym(vet_edited_o) %in% vec_veteran ~ "valid",
      !!sym(vet_edited_o) == "99" ~ "not known",
      TRUE ~ "invalid"), NA_character_))
  
  return(df_vet)
  
}



#############################.
### Assess Veteran Status Pub ###
#############################. 

# Author: Charlie Smith
# Date: 2024-04-10

assess_vet_status_pub <- function(df){
  
  vec_veteran <- read_xlsx(captnd_code_lookup, sheet = "Vets") |> 
    select(Codes) |> 
    filter(Codes != "99") |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |> 
    pull() |> as.numeric()
  
  df_vet <- df %>% 
    mutate(check_vet = if_else(!!sym(dataset_type_o) == "PT", case_when(
      is.na(!!sym(vet_o)) ~ "missing",
      !!sym(vet_o) %in% vec_veteran ~ "valid",
      !!sym(vet_o) == 99 ~ "not known",
      TRUE ~ "invalid"), NA_character_))
  
  return(df_vet)
  
}


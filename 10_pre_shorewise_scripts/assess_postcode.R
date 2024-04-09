
#######################.
### Assess Postcode ###
#######################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_postcode <- function(df){
  
  # load postcode lookup
  lookup_postcode <- import("../../../data/postcode_simd_lookup.csv") |> 
    select(postcode) |> 
    pull()

  # assess values
  df_postcode <- df |> 
    mutate(!!postcode_o := gsub(" ", "", !!sym(postcode_o)),
      check_postcode = case_when(
      !!sym(postcode_o) %in% lookup_postcode ~ "valid", 
      is.na(!!sym(postcode_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_postcode)
  
}


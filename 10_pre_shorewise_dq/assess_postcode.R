
#######################.
### Assess Postcode ###
#######################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_postcode <- function(df){
  
  # load postcode lookup
  lookup_postcode <- import("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.csv") |> 
    select(2) |> 
    mutate(pc8 = gsub(" ", "", pc8)) |> 
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


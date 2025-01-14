
#####################################.
### Assess Postcode Last Reported ###
#####################################.

# Author: Charlie Smith
# Date: 2024-12-03

assess_postcode_last_reported <- function(df){
  
  # load postcode lookup
  lookup_postcode <- import("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.csv") |> 
    select(2) |> 
    mutate(pc8 = gsub(" ", "", pc8)) |> 
    pull()

  # assess values
  df_postcode_last <- df |> 
    mutate(check_postcode_last_reported = case_when(
      !!sym(postcode_last_reported_o) %in% lookup_postcode ~ "valid", 
      is.na(!!sym(postcode_last_reported_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_postcode_last)
  
}


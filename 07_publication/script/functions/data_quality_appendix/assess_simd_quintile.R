
############################.
### Assess SIMD Quintile ###
############################.

# Author: CJS 
# Date: 2024-12-03

assess_simd_quintile <- function(df){
  
  df_simd <- df |> 
    mutate(check_simd2020_quintile = case_when(
      is.na(!!sym(simd_quintile_o)) ~ "missing",
      !!sym(simd_quintile_o) %in% c(1:5) ~ "valid",
      TRUE ~ "invalid"))
  
  return(df_simd)
  
}
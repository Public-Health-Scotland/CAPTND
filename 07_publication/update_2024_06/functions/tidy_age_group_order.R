
############################.
### Tidy age group order ###
############################.

# Author: Charlie Smith
# Date: 2024-07-16


tidy_age_group_order <- function(df){
  
  vec_age_group <- age_group(c(1:100)) |> unique()
  
  df_age <- df |>
    mutate(age_group = factor(age_group, levels = vec_age_group))
    
  return(df_age)
  
}

############################.
### Tidy age group order ###
############################.

# Author: Charlie Smith
# Date: 2024-07-16


tidy_age_group_order <- function(df){
  
  vec_age_group <- create_age_groups(c(1:100)) |> unique()
  
  df_age <- df |>
    mutate(!!sym(age_group_o) := factor(!!sym(age_group_o), levels = vec_age_group))
    
  return(df_age)
  
}
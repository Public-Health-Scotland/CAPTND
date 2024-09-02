
############################################################.
### Publication - replace sex codes with sex descriptors ###
############################################################.

# Author: Charlie Smith
# Date: 2024-05-30

add_sex_description <- function(df){
  
  df_sex <- df |> 
    mutate(sex_reported = fcase(
      !!sym(sex_reported_o) == 0, "Not known",
      !!sym(sex_reported_o) == 1, "Male",
      !!sym(sex_reported_o) == 2, "Female",
      !!sym(sex_reported_o) == 9, NA_character_,
      default = NA_character_))
  
  return(df_sex)
  
}

############################################################.
### Publication - replace sex codes with sex descriptors ###
############################################################.

# Author: Charlie Smith
# Date: 2024-05-30

add_sex_description <- function(df){
  
  df_sex <- df |> 
    mutate(sex_reported = case_when(
      sex_reported == 0 ~ "Not known",
      sex_reported == 1 ~ "Male",
      sex_reported == 2 ~ "Female",
      sex_reported == 9 ~ NA_character_,
      TRUE ~ NA_character_))
  
  return(df_sex)
  
}
#####################.
###   NULL to NA  ###
#####################.

#Converts NULL strings into NAs


null_to_na <- function(df){
  
  df_noNull=df %>%
    mutate(across(where(is.character), \(x) na_if(x,"NULL")))
  
  return(df_noNull)
}


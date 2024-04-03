
#############################.
### Format dates as dates ###
#############################.

# Author: Charlie Smith
# Date: 2024-04-01


format_dates <- function(df){
  
  df_dates <- df |> 
    mutate_at(vars(
    contains("date", ignore.case = TRUE), !!dob_o), ymd)
  
  return(df_dates)
  
}


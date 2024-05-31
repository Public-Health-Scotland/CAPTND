
#############################.
### Append Quarters to df ###
#############################.

# Author: Charlie Smith
# Date: 2024-05-30


append_quarter_ending <- function(df, date_col){
  
  df_quarters <- df |> 
    mutate(quarter = ceiling_date(!!sym(date_col), unit = "quarter") - 1,
           quarter_ending = floor_date(quarter, unit = "month")) |> 
    select(-quarter)
  
  return(df_quarters)
  
}


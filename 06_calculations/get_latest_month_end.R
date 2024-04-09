
################################.
### Get latest month in data ###
################################.

# Author: Charlie Smith
# Date: 2024-04-09

get_lastest_month_end = function(df){
  
  month <- df |> 
    select(!!header_date_o) |> 
    distinct() |> 
    pull() |> 
    max() |> 
    ceiling_date(unit = 'month')-1
  
  return(month)

}
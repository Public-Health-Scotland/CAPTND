#################################################.
### Get df of time frames for completing data ###
#################################################.

# Author: Bex Madden
# date: 1/10/2024


get_time_series <- function(months_lag, 
                            time_frame = c('monthly', 'quarterly')){
  
# make date range according to desired number of lagging months
month_end <- floor_date(most_recent_month_in_data, "month")
month_start <- ymd(month_end) - months(months_lag)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# add timeframe column according to desired interval
if(time_frame == 'monthly'){
  df_time <- data.frame(month = date_range)
  
} else {
  
  if(time_frame == 'quarterly'){
    df_time <- data.frame(month = date_range) |> 
      append_quarter_ending(date_col = "month") |> 
      select(quarter_ending) |> 
      distinct()
  }}


return(df_time)

}
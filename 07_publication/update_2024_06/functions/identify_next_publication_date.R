
######################################.
### Identify next publication date ###
######################################.

# Author: Charlie Smith
# Date: 2024-07-25

id_next_pub <- function(latest_month){ # = month_end (last data month)
  
  date_first <- latest_month + months(6) # six months after last data month
  date_end <- ceiling_date(date_first, "months")-1 # last day of +6 months month
  
  date_seq <- seq.Date(from = date_first, # get sequence of dates in month
                       to = date_end,
                       by = "day")
  
  next_pub_date <- data.frame(dates = date_seq) %>% # set as data.frame
    mutate(dow = weekdays(dates)) %>% # add day of week for each date
    filter(dow == "Tuesday") %>% # keep only Tuesdays
    slice(1) %>% # get first Tuesday
    mutate(day = as.numeric(format(dates, format = "%d")), # get day number
           ord_suffix = case_when( # create ordinal suffix based on day number
             day == 1 ~ "st",
             day == 2 ~ "nd",
             day == 3 ~ "rd",
             TRUE ~ "th"), # pretty lazy, but okay for first Tuesday
           month = format(dates, format = "%B"), # get month name
           year = format(dates, format = "%Y"), # get year
           desc = paste0(day, ord_suffix, " of ", month, " ", year)) %>% # combine for date description
    select(desc) %>% 
    pull() %>% 
    return()
  
} 
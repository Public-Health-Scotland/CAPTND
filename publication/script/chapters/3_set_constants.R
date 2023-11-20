
#####################.
### Set constants ###
#####################.

# Author: Charlie Smith
# Date: 2023-11-14




# 1 - Establish data time frame -------------------------------------------

month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")



# 2 - Set safe place to save working data ---------------------------------

data_working_safe <- "../../../data/publication_files/"



# 3 - Production data -----------------------------------------------------
publication_month <- month_end + months(3)


production_date <- Sys.Date()
production_month <- format(as.Date(production_date), "%B %Y")



# 4 - Reference -----------------------------------------------------------

# create HB vector
hb_vector <- c("NHS Scotland",
               "NHS Ayrshire and Arran", 
               "NHS Borders", 
               "NHS Dumfries and Galloway",
               "NHS Fife",
               "NHS Forth Valley", 
               "NHS Grampian",
               "NHS Greater Glasgow and Clyde",
               "NHS Highland",
               "NHS Lanarkshire", 
               "NHS Lothian", 
               "NHS Orkney",
               "NHS Shetland", 
               "NHS Tayside", 
               "NHS Western Isles",
               "NHS 24")


# chart dimensions
chart_width <- 24
chart_height <- 16


# 6 - Get next pub date ---------------------------------------------------
# (6 months after month_end, 1st Tuesday)
# next_pub_date <- id_next_pub(data_last = month_end)

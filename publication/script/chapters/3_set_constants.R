
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






# 6 - Get next pub date ---------------------------------------------------
# (6 months after month_end, 1st Tuesday)
next_pub_date <- id_next_pub(data_last = month_end)

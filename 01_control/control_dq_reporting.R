
########################################.
### Control pre-shorewise DQ scripts ###
########################################.

# Author: Charlie Smith
# Date: 2024-10-29

# Requirements:
# 2 GB RAM
# 15 GB memory (perhaps less)

suppressWarnings(source('./10_pre_shorewise_dq/control_pre_shorewise_detail.R'))


# NB if the number of variables in CAPTND increases, the concatenation formulas 
# in the trend data tabs in excel template will stop working and so will need manually updated 
# (drag the formula to down to cover the max number of rows in each - trend_row 
# and trend_row_alt provide these values). I tried to automate this using a loop 
# but it was prohibitively slow. This could be improved using features of the purrr() 
# package, but I cannot figure out a solution in the time available.


########################################.
### Function: Make a date into words ###
########################################.

# Author: Charlie Smith
# Date: 2024-05-07

date_to_month_year_words <- function(date){
  
  m <- as.numeric(format(date, "%m"))
  y <- format(date, "%Y")
  
  month_year_word <- paste(month.name[m], y, sep = " ")
  
  return(month_year_word)
  
}

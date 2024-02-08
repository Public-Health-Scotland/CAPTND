#####################################################.
###  remove unnecessary characters from postcode  ###
#####################################################.

#limitation: doesn't fix cases in which there are extra elements to the first half of the postcode
#ex: G1a 1AB can't be fixed, but G1 1ABC can

# 1 housekeeping ----------------------------------------------------------

# library(stringr)
# library(dplyr)


# 2 function --------------------------------------------------------------


clean_postcode <- function(df){
  
  df_edited <- df %>%
    mutate(!!postcode_o := str_replace_all(!!sym(postcode_o), '[:space:]*[:punct:]*', ''),
           !!postcode_o := str_replace_all(!!sym(postcode_o), '(?<=[:digit:]{1}[:alpha:]{2}).$*', ''))
  
  return(df_edited)
  
}

p=c('EH2b 8FFa', 'AB12  - 8WH.', 'A14 6RR!', 'G1 6HHq')
df=data.frame('postcode'=p)

df_2=clean_postcode(df)


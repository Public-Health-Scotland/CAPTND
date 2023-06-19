################################.
###  Complete veteran status ###
################################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('setup/new_column_names_swift.R')
library(dplyr)
library(tidyr)
library(lubridate)


# 2 Function --------------------------------------------------------------

complete_veteran_status <- function(df){
  #test
  # df=read_csv('../../../data/testDataset_vet.csv')
  # vet_edited_o='vet_edited'
  # vet_edited_counts_o='vet_edited_counts'
  # first_vet_o='first_vet'
  # last_vet_o='last_vet'
  # 
  # df$vet=as.numeric(df$vet)

  df_completed <- df %>%
    group_by(!!sym(chi_o)) %>% 
    mutate(!!vet_edited_o := case_when(
      (is.na(!!sym(vet_o)) | !!sym(vet_o) %in% c(98, 99)) & 
        lead(!!sym(vet_o), order_by=!!sym(header_date_o)) == 1 ~ 1,
      (is.na(!!sym(vet_o)) | !!sym(vet_o) %in% c(98, 99)) & 
        lag(!!sym(vet_o),order_by=!!sym(header_date_o)) == 2 ~ 2,
                                       TRUE ~ !!sym(vet_o)),
      .after=!!vet_o)
     

  return(df_completed)
}




# mutate(!!vet_edited_o := case_when(!!sym(vet_o) %in% c(98,99)~ NA_integer_,
#                                     TRUE ~ !!sym(vet_o)),
#        .after=!!vet_o) %>% 

# !!vet_edited_counts_o := (n_distinct(!!sym(vet_edited_o))),
#        !!first_vet_o := first(!!sym(vet_edited_o), order_by = !!sym(header_date_o)),
#        !!last_vet_o := last(!!sym(vet_edited_o), order_by = !!sym(header_date_o)),
# firstwith=na.locf(!!sym(vet_edited_o), fromLast = TRUE, na.rm = FALSE)
# 
# 
# 
# 
# if next non na value is 2 and previous non na value is 2  ~ 2
# if next non na value is 2 and there is no previous non na value ~ na
# if next non na value is 2 and previous non na value is 1 ~ na
# if next non value is 1 ~ 1
# 
# 
# !!first_vet_yes := first(!!sym(vet_edited_o)[!!sym(vet_edited_o)==1], order_by = !!sym(header_date_o)),










# !!sym(vet_edited_counts_o)==2 &
#                             is.na(!!sym(first_vet_o)) &
#                             !!sym(last_vet_o)==1 ~ 1,
#                           !!sym(vet_edited_counts_o)==2 &
#                             !!sym(first_vet_o)==2 &
#                             is.na(!!sym(last_vet_o)) ~ 2,
#                           !!sym(vet_edited_counts_o)==2 &
#                             !!sym(last_vet_o)==!!sym(first_vet_o) ~ !!sym(first_vet_o),
# 
# 
#                           !!sym(vet_edited_counts_o)==3&
#                             is.na(!!sym(first_vet_o)) &


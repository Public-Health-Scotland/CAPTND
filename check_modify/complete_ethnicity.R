###########################.
###  Complete ethnicity ###
###########################.


# 1 Housekeeping ----------------------------------------------------------

#Compares DOB recorded to DOB from CHI
source('config/new_colnames.R')
library(dplyr)
library(tidyr)
library(readr)
library(phsmethods)
library(lubridate)


# 2 Function --------------------------------------------------------------

complete_ethnicity <- function(df){
  
  df_completed <- df %>%
    mutate(!!ethnicity_edited_o := case_when(!!sym(ethnicity_o) %in% c(98,99)~ NA_character_,
                                        TRUE ~ !!sym(ethnicity_o)),
           .after=!!ethnicity_o) %>% 
    group_by(!!sym(patient_id_o)) %>% 
    mutate(!!ethnicity_edited_counts_o := (n_distinct(!!sym(ethnicity_edited_o))),
           !!ethnicity_edited_o := case_when(!!sym(ethnicity_edited_counts_o) == 2 & 
                                                    NA %in% !!sym(ethnicity_edited_o) ~ first(!!sym(ethnicity_edited_o), na_rm = TRUE),
                                                  TRUE ~ !!sym(ethnicity_edited_o)),
           !!ethnicity_edited_counts_o := (n_distinct(!!sym(ethnicity_edited_o))),
           !!ethnicity_evaluation_o := if_else(!!sym(ethnicity_edited_counts_o) == 1, 'ok','multiple ethnicities'),
           !!ethnicity_last_reported_o := last(!!sym(ethnicity_edited_o),order_by=!!sym(header_date_o), na_rm = TRUE),
           .after = !!ethnicity_edited_o) %>% 
    ungroup()
  
  report_multiple_ethnicities(df_completed)

  return(df_completed)
}

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
  
  df_completed <- df %>%
    mutate(!!vet_edited_o := case_when(!!sym(vet_o) %in% c(98,99)~ NA_integer_,
                                        TRUE ~ !!sym(vet_o)),
           .after=!!vet_o) %>% 
    group_by(!!sym(chi_o)) %>% 
    mutate(!!vet_edited_counts_o := (n_distinct(!!sym(vet_edited_o))),
           !!first_vet_o := first(!!sym(vet_edited_o), order_by = !!sym(header_date_o)),
           !!last_vet_o := last(!!sym(vet_edited_o), order_by = !!sym(header_date_o)),
           !!vet_edited_o := case_when(!!sym(vet_edited_counts_o)==2 &
                                         is.na(!!sym(first_vet_o)) &
                                         !!sym(last_vet_o)==1 ~ 1,
                                       !!sym(vet_edited_counts_o)==2 &
                                         !!sym(first_vet_o)==2 &
                                         is.na(!!sym(last_vet_o)) ~ 2,
                                       TRUE ~ !!sym(vet_edited_o)
                                       ),
           .after=!!vet_edited_o)
           

  return(df_completed)
}


#just a test
df<-  read_csv('../../../data/testDataset_lowercase.csv') %>% 
  mutate(chi = as.character(chi)) %>% 
  filter(issue=='one_CHI_3_vets_status')

vet_edited_o='vet_edited'
first_vet_o='first_vet'
last_vet_o='last_vet'
vet_edited_counts_o='vet_edited_counts'

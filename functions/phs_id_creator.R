#######################.
#### PHS ID creator ###
#######################.

#Creates PHS ID, which is a unique identifier of the patient record (not the journey)
#It is a concatenation of CHI and referral received date
#In case referral received date is not present, referral date is used (this will
#be the case for some records from Globalscape)


# 1 Load libraries --------------------------------------------------------
library(dplyr)
library(lubridate)


# 2 PHS id creator --------------------------------------------------------

phs_id_creator <- function(df){

  #phs id should be NA if CHI is NA or if rec date is NA and ref date is NA 
  df_phs_id=df %>% mutate(across(where(is.character), ~na_if(., "NULL")),
                          phs_id = case_when(!is.na(chi) & !is.na(rec_date) ~ 
                                               paste(chi, as.character(rec_date), sep = '_'),
                                             !is.na(chi) & is.na(rec_date) & !is.na(ref_date) ~
                                               paste(chi, as.character(ref_date), sep = '_')
                                             )
                          )
  return(df_phs_id)
}
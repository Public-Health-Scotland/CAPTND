
##################################.
### Globalscape column renamer ###
##################################.

# Function to rename globalscape columns prior to joining stages


# 1 - Load packages -------------------------------------------------------
library(dplyr)
library(magrittr)

# 2 - Load column names ---------------------------------------------------

source("./config/new_colnames.R")


# 3 - Load function -------------------------------------------------------

globalscape_column_renamer <- function(captnd_all){
  
  stage_names <- names(captnd_all) # get names of list elements to work through
  
  list_bucket <- list() # empty list to store renamed dataframes
  
  for(i in 1:length(stage_names)){
    
    if (stage_names[i] == "referral") { # if stage name matches, use these col names (etc.)
      
     df_renamed <- captnd_all[[i]] %>%  
       rename(!!header_date_o := "LOADDATE",
              !!file_id_o := "FILENAME",
              !!ucpn_o := "UCPN",
              !!upi_o := "UPI",
              !!chi_o := "CHI",
              !!hb_name_o := "HB",
              !!postcode_o := "POSTCODE",
              !!sex_o := "SEX",
              !!dob_o := "DOB",
              !!ethnicity_o := "ETHNICITY",
              !!ref_date_o := "REF_DATE",
              !!ref_rec_date_o := "RECEIVED_DATE",
              !!ref_source_o := "REF_SOURCE",
              !!ref_reason_o := "REF_REASON",
              !!ref_acc_o := "ACCEPTED",
              !!ref_rej_date_o := "REJ_DATE",
              !!ref_rej_reason_o := "REJ_REASON",
              !!ref_rej_act_o := "REJ_ACTIONS",
              !!protection_o := "PROTECTION",
              !!looked_after_c_o := "LAC",
              !!vet_o := "VETERAN",  
              !!preg_perinatal_o := "PPMH",
              !!act_code_sent_date_o := "CODE_SENT_DATE") 

    } else if (stage_names[i] == "apps") {
      
      df_renamed <- captnd_all[[i]] %>%  
        rename(!!header_date_o := "LOADDATE",
               !!file_id_o := "FILENAME",
               !!ucpn_o := "UCPN",
               !!upi_o := "UPI",
               !!chi_o := "CHI",
               !!hb_name_o := "HB",
               !!app_date_o := "APP_DATE",
               !!app_purpose_o := "PURPOSE",
               !!att_status_o := "STATUS",
               !!unav_date_start_o := "START_DATE",
               !!unav_date_end_o := "END_DATE",
               !!unav_days_no_o := "NUM_DAYS",
               !!unav_reason_o := "REASON",
               !!att_cat_o := "CATEGORY",
               !!prof_group_o := "PROF_GROUP",
               !!location_o := "LOCATION",
               !!preg_perinatal_o := "PPMH") #%>% 
        #select(-c(PPMH, loaddate, filename, record_type)) # drop unneeded cols?
      
    } else if (stage_names[i] == "diagnosis") {
    
      df_renamed <- captnd_all[[i]] %>%  
        rename(!!header_date_o := "LOADDATE",
               !!file_id_o := "FILENAME",
               !!ucpn_o := "UCPN",
               !!upi_o := "UPI",
               !!chi_o := "CHI",
               !!hb_name_o := "HB",
               !!diag_1_o := "DIAG1",
               !!diag_2_o := "DIAG2",
               !!diag_3_o := "DIAG3", 
               !!treat_1_o := "TREAT1",
               !!treat_2_o := "TREAT2",
               !!treat_3_o := "TREAT3",
               !!treat_group_or_ind_1_o := "GRPIND1",
               !!treat_group_or_ind_2_o := "GRPIND2",
               !!treat_group_or_ind_3_o := "GRPIND3",
               !!treat_start_date_o := "START_DATE") #%>% 
        #select(-c(loaddate, filename, record_type))
    
    } else if (stage_names[i] == "outcomes") {
      
      df_renamed <- captnd_all[[i]] %>%  
        rename(!!header_date_o := "LOADDATE",
               !!file_id_o := "FILENAME",
               !!ucpn_o := "UCPN",
               !!upi_o := "UPI",
               !!chi_o := "CHI",
               !!hb_name_o := "HB",
               !!measure_1_o := "MEASURES1",
               !!measure_2_o := "MEASURES2",
               !!measure_3_o := "MEASURES3") #%>% 
        #select(-c(loaddate, filename, record_type))
      
    } else if (stage_names[i] == "discharge") {
    
      df_renamed <- captnd_all[[i]] %>%  
        rename(!!header_date_o := "LOADDATE",
               !!file_id_o := "FILENAME",
               !!ucpn_o := "UCPN",
               !!upi_o := "UPI",
               !!chi_o := "CHI",
               !!hb_name_o := "HB",
               !!case_closed_date_o := "END_DATE") #%>% 
        #select(-c(loaddate, filename, record_type)) 
      
     } else {
      
      stop("Error: something's not right (unexpected number of stages)")
      
    }
    
      list_bucket[[i]] <- df_renamed # add df to empty list
    
  }
  
  names(list_bucket) <- stage_names # reapply stage names

  return(list_bucket)
  
}







#############################.
### Set Column Data Types ###
#############################.

# Author: Charlie Smith
# Date: 2023-05-03


# 1 - Housekeeping --------------------------------------------------------

# 1.1 - Load packages -----------------------------------------------------
# library(dplyr)
# library(lubridate)
# library(magrittr)
# library(rio)


# 1.2 - Get column name objects -------------------------------------------
source("config/new_colnames.R")



# 2 - Function ------------------------------------------------------------

set_col_data_types <- function(df){
  
  x <- df %>% 
    
    # set date columns
    mutate_at(vars(
      contains("date", ignore.case = TRUE), !!dob_o), ymd) %>% 

    # set numeric columns
    mutate(
      !!file_id_o := as.character(!!sym(file_id_o)),
      !!sex_o := as.numeric(!!sym(sex_o)),
      !!ref_source_o := as.numeric(!!sym(ref_source_o)),
      !!ref_reason_o := as.numeric(!!sym(ref_reason_o)),
      
      !!ref_acc_o := as.numeric(!!sym(ref_acc_o)),
      !!ref_rej_reason_o := as.numeric(!!sym(ref_rej_reason_o)),
      !!ref_rej_act_o := as.numeric(!!sym(ref_rej_act_o)), 
      !!app_purpose_o := as.numeric(!!sym(app_purpose_o)),
      !!att_status_o := as.numeric(!!sym(att_status_o)),
      
      !!unav_days_no_o := as.numeric(!!sym(unav_days_no_o)),
      !!unav_reason_o := as.numeric(!!sym(unav_reason_o)),
      !!att_cat_o := as.numeric(!!sym(att_cat_o)),
      !!prof_group_o := as.numeric(!!sym(prof_group_o)),
      !!location_o := as.numeric(!!sym(location_o)),
      
      !!treat_group_or_ind_1_o := as.numeric(!!sym(treat_group_or_ind_1_o)),
      !!treat_group_or_ind_2_o := as.numeric(!!sym(treat_group_or_ind_2_o)),
      !!treat_group_or_ind_3_o := as.numeric(!!sym(treat_group_or_ind_3_o)),
      !!measure_1_o := as.numeric(!!sym(measure_1_o)),
      !!measure_2_o := as.numeric(!!sym(measure_2_o)),
      
      !!measure_3_o := as.numeric(!!sym(measure_3_o)),
      !!protection_o := as.numeric(!!sym(protection_o)),
      !!looked_after_c_o := as.numeric(!!sym(looked_after_c_o)),
      !!vet_o := as.numeric(!!sym(vet_o)),
      !!preg_perinatal_o := as.numeric(!!sym(preg_perinatal_o))
      
      # the rest stay as characters
      )
      
  if(line_no_o %in% colnames(x)) {
    x=x %>% 
      mutate(
        !!line_no_o := as.numeric(!!sym(line_no_o))
      )
  
  } 
  
    
  
  return(x)
  
}

# Better to name and set all cols explicitly? Add try catch for unexpected cols?


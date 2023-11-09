#############################################
###     Directory structure creator       ###
#############################################


#Creates folders according to direcory plan on described on 
#file_structure_proposal_nov_2023.txt located in shorewise main folder


# 1 Load packages ---------------------------------------------------------

library(lubridate)
library(purrr)



# Function ----------------------------------------------------------------



create_captnd_directory_structure <- function(date_of_today) {
  create_pathway_names(date_of_today)
  
  dir_vector <- c(root_dir,
                  data_export_dir,
                  patients_waiting_dir,
                  patients_waiting_dir_by_board,
                  patients_seen_dir,
                  patients_seen_dir_by_board,
                  open_cases_dir,
                  open_cases_dir_by_board,
                  referrals_dir,
                  referrals_dir_by_board,
                  data_quality_dir,
                  data_removed_dir,
                  stats_removed_dir,
                  removed_data_export_dir,
                  removed_data_export_by_board_dir,
                  data_checked_dir,
                  stats_checked_dir,
                  dob_conflicting_dir,
                  dob_conflicting_by_board_dir,
                  ethniticies_dir,
                  rtt_dir,
                  rtt_by_board_dir,
                  excess_time_since_last_appt_dir,
                  external_reports_dir,
                  product1_dir,
                  product2_dir,
                  product3_dir)
  
  map(dir_vector, dir.create)
  
}






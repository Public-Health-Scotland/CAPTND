#############################################
###     Directory structure creator       ###
#############################################


#Creates folders according to direcory plan on described on 
#file_structure_proposal_nov_2023.txt located in shorewise main folder

# NB set in '02_setup/set_dir_structure.R'


# 1 Load packages ---------------------------------------------------------

# library(lubridate)
# library(purrr)



# Function ----------------------------------------------------------------



create_captnd_directory_structure <- function() {
  
  dir_vector <- c(root_dir,
                  data_export_dir,
                  patients_waiting_dir,
                  patients_waiting_dir_by_board,
                  patients_seen_dir,
                  patients_seen_dir_by_board,
                  open_cases_dir,
                  open_cases_dir_by_board,
                  first_contact_dir, 
                  first_contact_dir_by_board,
                  referrals_dir,
                  referrals_dir_by_board,
                  appointments_dir,
                  appointments_dir_by_board, 
                  dna_dir,
                  dna_dir_by_board,
                  patient_turnover_dir,
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
                  product3_dir,
                  
                  shorewise_pub_dir,
                  shorewise_pub_data_dir,
                  shorewise_pub_measure_summaries_dir,
                  shorewise_pub_report_dir,
                  
                  pre_shorewise_output_dir,
                  data_prep_dir,
                  data_quality_report_dir,
                  publication_dir,
                  
                  comp_report_dir,
                  comp_report_dir_patient_data)
  
  map(dir_vector, dir.create)
  
}


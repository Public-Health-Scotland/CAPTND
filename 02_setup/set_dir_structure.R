#####################################.
###     Directory structure       ###
#####################################.


#Creates folders names according to direcory plan on described on 
#file_structure_proposal_nov_2023.txt located in shorewise main folder


# 1 Load packages ---------------------------------------------------------

# library(lubridate)



# Function ----------------------------------------------------------------



  
  root_dir <<- paste0("../../../output/analysis_", data_analysis_latest_date)
  data_export_dir <<- paste0(root_dir, '/data_export')
  
  patients_waiting_dir <<- paste0(data_export_dir,'/patients_waiting')
  patients_waiting_dir_by_board <<- paste0(patients_waiting_dir, "/by_board")
  #patients_waiting_dir_by_board <<- paste0(patients_waiting_dir, "/by_month")
  
  patients_seen_dir <<- paste0(data_export_dir,'/patients_seen')
  patients_seen_dir_by_board <<- paste0(patients_seen_dir, "/by_board")
  
  open_cases_dir <<- paste0(data_export_dir,'/open_cases')
  open_cases_dir_by_board <<- paste0(open_cases_dir, "/by_board")
  
  first_contact_dir <<- paste0(data_export_dir,'/first_contact')
  first_contact_dir_by_board <<- paste0(first_contact_dir, "/by_board")
  
  referrals_dir <<- paste0(data_export_dir,'/referrals')
  referrals_dir_by_board <<- paste0(referrals_dir, "/by_board")
  
  appointments_dir <<- paste0(data_export_dir,'/appointments')
  appointments_dir_by_board <<- paste0(appointments_dir, "/by_board")
  
  dna_dir <<- paste0(data_export_dir,'/dna')
  dna_dir_by_board <<- paste0(dna_dir, "/by_board")
  
  data_quality_dir <<- paste0(root_dir, '/data_quality')
  data_removed_dir <<- paste0(data_quality_dir, '/data_removed')
  
  stats_removed_dir <<- paste0(data_removed_dir, '/stats_removed')
  removed_data_export_dir <<- paste0(data_removed_dir, '/removed_data_export')
  removed_data_export_by_board_dir <<- paste0(removed_data_export_dir, '/by_board')
  
  
  data_checked_dir <<- paste0(data_quality_dir, '/data_checked')
  stats_checked_dir <<- paste0(data_checked_dir, '/stats_checked')
  

  dob_conflicting_dir <<- paste0(stats_checked_dir, '/dob_conflicting')
  dob_conflicting_by_board_dir <<- paste0(dob_conflicting_dir, '/by_board')

  
  ethniticies_dir <<- paste0(stats_checked_dir, '/ethnicities')
  rtt_dir <<- paste0(stats_checked_dir, '/rtt')
  rtt_by_board_dir <<- paste0(rtt_dir,'/by_board')
  
  excess_time_since_last_appt_dir <<- paste0(stats_checked_dir, '/excess_time_since_last_appt')
  
  external_reports_dir <<- paste0(root_dir, '/external_reports')
  product1_dir <<- paste0(external_reports_dir, '/product1')
  product2_dir <<- paste0(external_reports_dir, '/product2')
  product3_dir <<- paste0(external_reports_dir, '/product3')
  
  









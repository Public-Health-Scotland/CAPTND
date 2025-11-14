##################################################.
## Create CAPTND Data Error Reports for each HB ##
##################################################.

#Author: Luke Taylor
#Written: 13/10/2025

month_end <- as.Date('2025-09-30')

#source scripts
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")
source("05_data_quality/report_multi_ref_records.R")
source("05_data_quality/report_multi_chi.R")
source("05_data_quality/report_appts_missing_refs.R")
source("05_data_quality/report_missing_appt_details.R")
source("05_data_quality/report_invalid_accept_rej_status.R")
source("05_data_quality/check_impossible_dates.R")
source("05_data_quality/report_appts_after_ref_rej.R")
source("05_data_quality/report_cancellation_date_issues.R")
source("05_data_quality/report_invalid_unav_periods.R")
source("05_data_quality/report_missing_ethnicity.R")
source("05_data_quality/report_wl.R")
source("05_data_quality/report_inactive_patient_pathways.R")

month_start <- as.Date('2025-09-01')
month_name <- format(month_start, "%b-%Y")


df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#create parquet files
multi_ref_pathways()
multi_chi_pathways()
assess_appts_missing_refs()
treat_appts_missing_refs()
missing_appt_purpose()
missing_att_status()
invalid_accept_status()
impossible_app_dates()
impossible_case_closed_dates()
#multi_ucpn_sub()
appts_after_rej_ref()
missing_cancel_dates()
cancel_date_error()
invalid_unav_period() 
missing_ethnicity()
write_wl_extract()
inactive_pathways(inactive_period = 12)

create_error_report <- function(dataset_choice = c("CAMHS", "PT")){
  
  for (hb in hb_vector){
    
    wb <- loadWorkbook("../../../report_templates/data_quality/error_report_template.xlsx")
    
    #write data into tabs
    date_style <- createStyle(numFmt = "yyyy/mm/dd")
    
    #Tab1
    multi_ref_records_df <- read_parquet(paste0(stats_checked_dir, "/multi_ref_records_", month_start, ".parquet")) |>
      mutate(ref_rec_date = as.Date(ref_rec_date),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 1", 
              x = multi_ref_records_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    #set row range based on df size
    row_range <- if (nrow(multi_ref_records_df) > 0) {
      11:(nrow(multi_ref_records_df) + 11)
    } else {
      integer(0)
    }
    
    #add style to format date columns
    addStyle(wb, sheet = "Tab 1", style = date_style, cols = 6:7, rows = row_range, gridExpand = T)
    
    #Tab2
    multi_chi_pathways_df <- read_parquet(paste0(stats_checked_dir, "/multi_chi_pathways_", month_start, ".parquet")) |>
      mutate(header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 2", 
              x = multi_chi_pathways_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(multi_chi_pathways_df) > 0) {
      11:(nrow(multi_chi_pathways_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 2", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    
    #Tab3
    assess_appts_missing_refs_df <- read_parquet(paste0(stats_checked_dir, "/assess_appts_missing_ref_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             header_date = as.Date(header_date),
             first_con_app_date = as.Date(first_con_app_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 3", 
              x = assess_appts_missing_refs_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(assess_appts_missing_refs_df) > 0) {
      11:(nrow(assess_appts_missing_refs_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 3", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 3", style = date_style, cols = 8:9, rows = row_range, gridExpand = T)
    
    #Tab4
    treat_appts_missing_refs_df <- read_parquet(paste0(stats_checked_dir, "/treat_appts_missing_ref_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             header_date = as.Date(header_date),
             first_con_app_date = as.Date(first_con_app_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 4", 
              x = treat_appts_missing_refs_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(treat_appts_missing_refs_df) > 0) {
      11:(nrow(treat_appts_missing_refs_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 4", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 4", style = date_style, cols = 8:9, rows = row_range, gridExpand = T)
    
    #Tab5
    missing_appt_purpose_df <- read_parquet(paste0(stats_checked_dir, "/missing_appt_purpose_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 5", 
              x = missing_appt_purpose_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(missing_appt_purpose_df) > 0) {
      11:(nrow(missing_appt_purpose_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 5", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 5", style = date_style, cols = 9, rows = row_range, gridExpand = T)
    
    #Tab6
    missing_att_status_df <- read_parquet(paste0(stats_checked_dir, "/missing_att_status_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 6", 
              x = missing_att_status_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(missing_att_status_df) > 0) {
      11:(nrow(missing_att_status_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 6", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 6", style = date_style, cols = 9, rows = row_range, gridExpand = T)
  
    #Tab7
    rej_with_no_date_df <- read_parquet(paste0(stats_checked_dir, "/rej_with_no_date_", month_start, ".parquet")) |>
      mutate(ref_date = as.Date(ref_date),
             ref_rec_date = as.Date(ref_rec_date),
             ref_rej_date = as.Date(ref_rej_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice) 
    
    writeData(wb, sheet = "Tab 7", 
              x = rej_with_no_date_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(rej_with_no_date_df) > 0) {
      11:(nrow(rej_with_no_date_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 7", style = date_style, cols = 2, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 7", style = date_style, cols = 7:8, rows = row_range, gridExpand = T)
    
    #Tab8
    accept_with_rej_date_df <- read_parquet(paste0(stats_checked_dir, "/accept_with_rej_date_", month_start, ".parquet")) |>
      mutate(ref_date = as.Date(ref_date),
             ref_rec_date = as.Date(ref_rec_date),
             ref_rej_date = as.Date(ref_rej_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 8", 
              x = accept_with_rej_date_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(accept_with_rej_date_df) > 0) {
      11:(nrow(accept_with_rej_date_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 8", style = date_style, cols = 2, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 8", style = date_style, cols = 7:8, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 8", style = date_style, cols = 10, rows = row_range, gridExpand = T)
    
    #Tab9
    impossible_app_dates_df <- read_parquet(paste0(stats_checked_dir, "/impossible_appts_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             ref_rec_date_opti = as.Date(ref_rec_date_opti),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice) 
    
    writeData(wb, sheet = "Tab 9", 
              x = impossible_app_dates_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(impossible_app_dates_df) > 0) {
      11:(nrow(impossible_app_dates_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 9", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)
    
    #Tab10
    impossible_cc_dates_df <- read_parquet(paste0(stats_checked_dir, "/impossible_cc_ref_", month_start, ".parquet")) |>
      mutate(ref_rec_date_opti = as.Date(ref_rec_date_opti),
             case_closed_date = as.Date(case_closed_date),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice) 
    
    writeData(wb, sheet = "Tab 10", 
              x = impossible_cc_dates_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(impossible_cc_dates_df) > 0) {
      11:(nrow(impossible_cc_dates_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 10", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)
    
    
    #Tab11
    appts_after_rej_ref_df <- read_parquet(paste0(stats_checked_dir, "/appts_after_rej_ref_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             ref_rec_date = as.Date(ref_rec_date),
             ref_rej_date = as.Date(ref_rej_date),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 11", 
              x = appts_after_rej_ref_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(appts_after_rej_ref_df) > 0) {
      11:(nrow(appts_after_rej_ref_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 11", style = date_style, cols = 6:9, rows = row_range, gridExpand = T)
    
    #Tab12
    missing_cancel_date_df <- read_parquet(paste0(stats_checked_dir, "/no_cancel_date_", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             cancellation_date = as.Date(cancellation_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 12", 
              x = missing_cancel_date_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(missing_cancel_date_df) > 0) {
      11:(nrow(missing_cancel_date_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 12", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    
    #Tab13
    cancel_date_error_df <- read_parquet(paste0(stats_checked_dir, "/app_purp_not_can", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             cancellation_date = as.Date(cancellation_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 13", 
              x = cancel_date_error_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(cancel_date_error_df) > 0) {
      11:(nrow(cancel_date_error_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 13", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 13", style = date_style, cols = 8, rows = row_range, gridExpand = T)
    
    #Tab14
    unav_validity_df <- read_parquet(paste0(stats_checked_dir, "/invalid_unav", month_start, ".parquet")) |>
      mutate(app_date = as.Date(app_date),
             unav_date_start = as.Date(unav_date_start),
             unav_date_end = as.Date(unav_date_end),
             header_date = as.Date(header_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice) |>
      mutate()
    
    writeData(wb, sheet = "Tab 14", 
              x = unav_validity_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(unav_validity_df) > 0) {
      11:(nrow(unav_validity_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 14", style = date_style, cols = 6, rows = row_range, gridExpand = T)
    addStyle(wb, sheet = "Tab 14", style = date_style, cols = 10, rows = row_range, gridExpand = T)
    
    #Tab15
    missing_ethnicity_df <- read_parquet(paste0(stats_checked_dir, "/missing_ethnicity_", month_start, ".parquet")) |>
      mutate(ref_date = as.Date(ref_date),
             ref_rec_date = as.Date(ref_rec_date)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 15", 
              x = missing_ethnicity_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(missing_ethnicity_df) > 0) {
      11:(nrow(missing_ethnicity_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 15", style = date_style, cols = 6:7, rows = row_range, gridExpand = T)
    
    #Tab16
    wl_extract_df <- read_parquet(paste0(stats_checked_dir, "/wl_extract_", month_end, ".parquet")) |>
      mutate(ref_rec_date = as.Date(ref_rec_date),
             sub_month_end = as.Date(sub_month_end)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 16", 
              x = wl_extract_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(wl_extract_df) > 0) {
      11:(nrow(wl_extract_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 16", style = date_style, cols = 6:7, rows = row_range, gridExpand = T)
    
    #Tab17
    inactive_patients_df <- read_parquet(paste0(stats_checked_dir, "/inactive_patients_", month_start, ".parquet")) |>
      mutate(ref_rec_date_opti = as.Date(ref_rec_date_opti),
             last_act = as.Date(last_act)) |>
      filter(hb_name == hb,
             dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 17", 
              x = inactive_patients_df, 
              startCol = 2, startRow = 11, colNames = FALSE)
    
    row_range <- if (nrow(inactive_patients_df) > 0) {
      11:(nrow(inactive_patients_df) + 11)
    } else {
      integer(0)
    }
    
    addStyle(wb, sheet = "Tab 17", style = date_style, cols = 6:7, rows = row_range, gridExpand = T)
    
    
    
    title <- paste0("CAPTND: ", dataset_choice, " Data Error/Extract Report ", month_name)
    
    writeData(wb, sheet = "Contents", 
              x = title,  
              startCol = 2, startRow = 2)
    addStyle(wb, sheet = "Contents", style = createStyle(fontName = 'Arial', 
                                                         fontSize = 14,
                                                         textDecoration = c("bold")),rows = 2, cols = 2)
    
    
    vec_tabs <- c("Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7", 
                  "Tab 8", "Tab 9", "Tab 10", "Tab 11", "Tab 12", "Tab 13", 
                  "Tab 14", "Tab 15", "Tab 16", "Tab 17")
    paras <- c(paste0("Tab 1: Individual pathways with multiple referral records, ", month_name),
               paste0("Tab 2: Individual pathways with multiple CHI numbers, ", month_name),
               paste0("Tab 3: Assessment appointments received in ", month_name, " with no referral record"),
               paste0("Tab 4: Treatment appointments received in ", month_name, " with no referral record"),
               paste0("Tab 5: Appointment records received in ", month_name, " with no appointment purpose"),
               paste0("Tab 6: Appointment records recieved in ", month_name, " with no attendance status"),
               paste0("Tab 7: Referral rejection received in ", month_name, " with no rejection date"),
               paste0("Tab 8: Referrals received in ", month_name, " with a status of accepted, but have a rejection date"),
               paste0("Tab 9: Impossible appointment records received in ", month_name),
               paste0("Tab 10: Impossible case closed dates received in ", month_name),
               paste0("Tab 11: Appointment records received in ", month_name, " for a rejected referral"),
               paste0("Tab 12: Cancelled appointments received in ", month_name, " with no cancellation date"),
               paste0("Tab 13: Appointment records received in ", month_name, " that have not been cancelled, but have a cancellation date"),
               paste0("Tab 14: Unusable unavailability records received in ", month_name),
               paste0("Tab 15: Referral records received in ", month_name, " with missing or unknown ethnicity status"),
               paste0("Tab 16: Patients on treatment waiting list who have been waiting more than 18 weeks, as at the end of ", month_name),
               paste0("Tab 17: Patient pathways with no activity for 12 months or more, as of the start of ", month_name))
    
    
    
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], 
                x = paras[i], 
                startCol = 2, startRow = 3)
      addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11,
                                                    textDecoration = "bold"), rows = 2, cols = 4)
    }
    
    
    #save workbook
    saveWorkbook(wb,paste0("/PHI_conf/MentalHealth5/CAPTND/data_quality_report/", "captnd_data_extract_report_", 
                           dataset_choice, "_", hb, "_", month_end, ".xlsx"), 
                 overwrite = TRUE)
    
  }
  
}

create_error_report(dataset_choice = "CAMHS")
create_error_report(dataset_choice = "PT")


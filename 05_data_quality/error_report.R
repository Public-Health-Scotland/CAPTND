##################################################.
## Create CAPTND Data Error Reports for each HB ##
##################################################.

#Author: Luke Taylor
#Written: 13/10/2025

#source scripts
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")
source("05_data_quality/report_multi_ref_records.R")
source("05_data_quality/report_multi_chi.R")
source("05_data_quality/report_appts_missing_refs.R")
source("05_data_quality/report_invalid_accept_rej_status.R")
source("05_data_quality/check_impossible_dates.R")
source("05_data_quality/report_multi_ucpns.R")
source("05_data_quality/report_appts_after_ref_rej.R")
source("05_data_quality/report_cancellation_date_issues.R")
source("05_data_quality/report_invalid_unav_periods.R")

month_start <- as.Date('2025-08-01')
month_end <- as.Date('2025-08-30')

df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#create csv files
multi_ref_pathways()
multi_chi_pathways()
appts_missing_refs()
invalid_accept_status()
impossible_app_dates()
impossible_case_closed_dates()
multi_ucpn_sub()
appts_after_rej_ref()
missing_cancel_dates()
cancel_date_error()
invalid_unav_period() 

create_error_report <- function(){

for (hb in hb_vector){
  
  wb <- loadWorkbook("../../../report_templates/data_quality/error_report_template.xlsx")

#write data into tabs
date_style <- createStyle(numFmt = "yyyy/mm/dd")

#Tab1
multi_ref_records_df <- read_xlsx(paste0(stats_checked_dir, "/multi_ref_records_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 1", 
          x = multi_ref_records_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(multi_ref_records_df) > 0) {
  8:(nrow(multi_ref_records_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 1", style = date_style, cols = 6:7, rows = row_range, gridExpand = T)

#Tab2
multi_chi_pathways_df <- read_xlsx(paste0(stats_checked_dir, "/multi_chi_pathways_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 2", 
          x = multi_chi_pathways_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(multi_chi_pathways_df) > 0) {
  8:(nrow(multi_chi_pathways_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 2", style = date_style, cols = 6, rows = row_range, gridExpand = T)

#Tab3
appts_missing_refs_df <- read_xlsx(paste0(stats_checked_dir, "/appts_missing_ref_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 3", 
          x = appts_missing_refs_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(appts_missing_refs_df) > 0) {
  8:(nrow(appts_missing_refs_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 3", style = date_style, cols = 6, rows = row_range, gridExpand = T)

#Tab4
rej_with_no_date_df <- read_xlsx(paste0(stats_checked_dir, "/rej_with_no_date_", month_start, ".xlsx")) |>
  filter(hb_name == hb) 

writeData(wb, sheet = "Tab 4", 
          x = rej_with_no_date_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(rej_with_no_date_df) > 0) {
  8:(nrow(rej_with_no_date_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 4", style = date_style, cols = 2, rows = row_range, gridExpand = T)
addStyle(wb, sheet = "Tab 4", style = date_style, cols = 7:8, rows = row_range, gridExpand = T)

#Tab5
accept_with_rej_date_df <- read_xlsx(paste0(stats_checked_dir, "/accept_with_rej_date_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 5", 
          x = accept_with_rej_date_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(accept_with_rej_date_df) > 0) {
  8:(nrow(accept_with_rej_date_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 5", style = date_style, cols = 2, rows = row_range, gridExpand = T)
addStyle(wb, sheet = "Tab 5", style = date_style, cols = 7:8, rows = row_range, gridExpand = T)
addStyle(wb, sheet = "Tab 5", style = date_style, cols = 10, rows = row_range, gridExpand = T)

#Tab6
impossible_app_dates_df <- read_xlsx(paste0(stats_checked_dir, "/impossible_appts_", month_start, ".xlsx")) |>
  filter(hb_name == hb) 

writeData(wb, sheet = "Tab 6", 
          x = impossible_app_dates_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(impossible_app_dates_df) > 0) {
  8:(nrow(impossible_app_dates_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 6", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)

#Tab7
impossible_cc_dates_df <- read_xlsx(paste0(stats_checked_dir, "/impossible_cc_ref_", month_start, ".xlsx")) |>
  filter(hb_name == hb) 

writeData(wb, sheet = "Tab 7", 
          x = impossible_cc_dates_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(impossible_cc_dates_df) > 0) {
  8:(nrow(impossible_cc_dates_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 7", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)

#Tab8
multi_ucpn_sub_df <- read_xlsx(paste0(stats_checked_dir, "/multi_ucpns_sub_mth_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 8", 
          x = multi_ucpn_sub_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(multi_ucpn_sub_df) > 0) {
  8:(nrow(multi_ucpn_sub_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 8", style = date_style, cols = 7, rows = row_range, gridExpand = T)

#Tab9
appts_after_rej_ref_df <- read_xlsx(paste0(stats_checked_dir, "/appts_after_rej_ref_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 9", 
          x = appts_after_rej_ref_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(appts_after_rej_ref_df) > 0) {
  8:(nrow(appts_after_rej_ref_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 9", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)

#Tab10
missing_cancel_date_df <- read_xlsx(paste0(stats_checked_dir, "/no_cancel_date_", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 10", 
          x = missing_cancel_date_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(missing_cancel_date_df) > 0) {
  8:(nrow(missing_cancel_date_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 10", style = date_style, cols = 6, rows = row_range, gridExpand = T)

#Tab11
cancel_date_error_df <- read_xlsx(paste0(stats_checked_dir, "/app_purp_not_can", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 11", 
          x = cancel_date_error_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(cancel_date_error_df) > 0) {
  8:(nrow(cancel_date_error_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 11", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)

#Tab12
unav_validity_df <- read_xlsx(paste0(stats_checked_dir, "/invalid_unav", month_start, ".xlsx")) |>
  filter(hb_name == hb)

writeData(wb, sheet = "Tab 12", 
          x = unav_validity_df, 
          startCol = 2, startRow = 8, colNames = FALSE)

row_range <- if (nrow(unav_validity_df) > 0) {
  8:(nrow(unav_validity_df) + 7)
} else {
  integer(0)
}

addStyle(wb, sheet = "Tab 12", style = date_style, cols = 6:8, rows = row_range, gridExpand = T)
addStyle(wb, sheet = "Tab 12", style = date_style, cols = 10, rows = row_range, gridExpand = T)


title <- paste0("CAPTND: Error Report ", month_end)

writeData(wb, sheet = "Contents", 
          x = title,  
          startCol = 2, startRow = 2)
addStyle(wb, sheet = "Contents", style = createStyle(fontName = 'Arial', 
                                                  fontSize = 14,
                                                  textDecoration = c("bold")),rows = 2, cols = 2)

#save workbook
saveWorkbook(wb,paste0("/PHI_conf/MentalHealth5/CAPTND/data_quality_report/", "captnd_data_error_report_", hb, 
                       "_", month_end, ".xlsx"), 
             overwrite = TRUE)

}
  
}

create_error_report()



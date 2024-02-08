###############################################.
###  Percentage of unusable records report  ###
###############################################.

# 1 Load libraries and packages ---------------------------------------------
# library(dplyr)
# library(lubridate)
# library(readr)


# 2 Function ----------------------------------------------------------------

report_unusable_records <- function(df_raw, saveName) {
  
  timePeriod=1 #time in years that the report will report on. 
  level_order <- c('NHS Ayrshire and Arran',
                   'NHS Borders',
                   'NHS Dumfries and Galloway',
                   'NHS Fife',
                   'NHS Forth Valley',
                   'NHS Grampian',
                   'NHS Greater Glasgow and Clyde',
                   'NHS Highland',
                   'NHS Lanarkshire',
                   'NHS Lothian',
                   'NHS Orkney',
                   'NHS Shetland',
                   'NHS Tayside',
                   'NHS Western Isles',
                   'NHS24',
                   'NHS Scotland')
  #calculates number and percentages of missing ucpns and patient ids. 
  #records are considered unusable if missing ucpn or patient id
  
  df_stats <- df_raw %>%
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
    select(!!hb_name_o,!!dataset_type_o,!!ucpn_o,!!patient_id_o,!!submission_date_o) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
    summarise(!!total_rows_o:=n(),
              #patient_id_na=sum(is.na(!!sym(patient_id_o))),
              #ucpn_na=sum(is.na(!!sym(ucpn_o))),
              #ucpn_and_patient_id_na=sum(is.na(!!sym(ucpn_o)) & is.na(!!sym(patient_id_o))),
              removed_rows=sum(is.na(!!sym(ucpn_o)) | is.na(!!sym(patient_id_o))),
              .groups = "drop") %>% 
    group_by(!!sym(submission_date_o), !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"NHS Scotland"),
                        .groups = "keep")) %>% 
    group_by(!!sym(hb_name_o)) %>%
    mutate(#patient_id_na_perc= round((patient_id_na * 100)/!!sym(total_rows_o), 3),
              #ucpn_na_perc= round((ucpn_na * 100)/!!sym(total_rows_o), 3),
              #ucpn_and_patient_id_na_perc= round((ucpn_and_patient_id_na * 100)/!!sym(total_rows_o), 3),
              perc_removed= round((removed_rows * 100)/!!sym(total_rows_o), 3),
           .after=!!submission_date_o,
           issue='removed_missing_pat_id_ucpn') %>% 
    ungroup() 
  
  
  savingLocation <- paste0(stats_removed_dir,'/', 
                           saveName,
                           "_removed_missing_pat_id_ucpn_stats")
  
  write_csv(df_stats, paste0(savingLocation,
                             ".csv"))
  
  message(paste0('Stats on removed records due to lack of one of the key variables
                 Patient ID and/or UCPN was saved in\n',
                 data_removed_dir))

}


##############################################################.
### Create aggregate and CAPTND measure comparison reports ### 
##############################################################.

# Author: Charlie Smith
# Date: 2024-03-26

create_comparison_reports <- function(){
  
  # load latest comparison data
  dna <- read_parquet(paste0(dna_dir,"/comp_data_dna.parquet")) |> 
    mutate(measure = "dna", .before = everything()) |> 
    select(measure, dataset_type, hb_name, month = app_month, n_aggregate, 
           n_captnd = app_count, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name) |> 
    mutate(measure_type = "not_required", .after = "measure")
  
  first_contact <- read_parquet(paste0(first_contact_dir, "/comp_data_firstcontact.parquet")) |> 
    mutate(measure = "first_contact", .before = everything()) |> 
    select(measure, measure_type = contact_type, dataset_type, hb_name, month = app_month,  
           n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  open_cases <- read_parquet(paste0(open_cases_dir, "/comp_data_opencases_CAMHS.parquet")) |> 
    mutate(measure = "open_cases", .before = everything()) |> 
    select(measure, measure_type = demand_type, dataset_type, hb_name, month, n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  waits_patients_seen <- read_parquet(paste0(patients_seen_dir, "/comp_data_patientsseen.parquet")) |> 
    mutate(measure = "waits_patients_seen", .before = everything()) |> 
    select(measure, measure_type = waiting_period, dataset_type, hb_name, month = app_month, 
           n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  waits_patients_waiting <- read_parquet(paste0(patients_waiting_dir, "/comp_data_patients_waiting_monthly.parquet")) |> 
    select(measure, measure_type, dataset_type, hb_name, month, n_aggregate, n_captnd, captnd_perc_agg) |>
    arrange(dataset_type, hb_name)
  
  referrals <- read_parquet(paste0(referrals_dir, "/comp_data_referrals.parquet")) |> 
    mutate(measure = "referrals", .before = everything()) |> 
    select(measure, measure_type = ref_acc_last_reported, dataset_type, hb_name, month = referral_month, n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  # combine into one df
  df_mega <- rbind(
    dna,
    first_contact,
    open_cases,
    waits_patients_seen,
    waits_patients_waiting,
    referrals
  ) |> arrange(hb_name)
  
  # need to find way of identifying records to share with HBs for checking
  # starting with most basic for proof of concept
  
  measures_vec <- unique(df_mega$measure)
  tolerance_value = 10
  
  df_id_for_checking <- df_mega |> 
    mutate(perc_over_agg = captnd_perc_agg - 100) |> 
    filter(#(
      perc_over_agg > tolerance_value #| perc_over_agg < -tolerance_value) # outwith X% tolerance, underestimates are less of an issue (records being dropped due to lack of data keys)
      & 
        month == max(df_mega$month, na.rm = TRUE)) |>  # latest month
    arrange(measure, dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(comp_report_dir_patient_data, "/records_to_find"))
  
  
  # split into one df per HB
  df_mega_list <- df_mega |> 
    ungroup() |> 
    group_by(hb_name) |> 
    group_split() |> 
    setNames(unique(df_mega$hb_name))
  
  # where to save separate HB reports
  #comp_report_dir <- paste0(root_dir, "/data_export/0_comp_reports")
  #dir.create(comp_report_dir)
  
  library(writexl)
  # save each of these by HB name - not working
  for(i in 1:length(df_mega_list)){

    df_hb <- df_mega_list[[i]]
    
    df_split <- df_hb |> # split by measure
      arrange(measure) |> 
      group_by(measure) |> 
      group_split() |>
      setNames(sort(unique(df_hb$measure)))

    hb_name_no_space <- df_mega_list[[i]][1,4] |> # get hb name and format for filenames 
      tolower() |>
      str_replace_all(" ", "_")

    writexl::write_xlsx(df_split, paste0(comp_report_dir, "/comp_report_", hb_name_no_space,".xlsx")) # save each measure to separate tab in excel doc
    
  }
  
  message(paste0("Reports created and saved to: ", comp_report_dir))
  detach(package:writexl, unload = TRUE)
  
}











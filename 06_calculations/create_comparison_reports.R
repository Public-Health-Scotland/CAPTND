
##############################################################.
### Create aggregate and CAPTND measure comparison reports ### 
##############################################################.

# Author: Charlie Smith
# Date: 2024-03-26

create_comparison_reports <- function(){
  
  # load latest comparison data
  dna <- read_parquet(paste0(dna_dir,"/comp_data_dna.parquet")) |> 
    mutate(measure = "dna", .before = everything()) |> 
    select(measure, !!sym(dataset_type_o), !!sym(hb_name_o), month = app_month, n_aggregate, 
           n_captnd = app_count, captnd_perc_agg) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    mutate(measure_type = "not_required", .after = "measure")
  
  first_contact <- read_parquet(paste0(first_contact_dir, "/comp_data_firstcontact.parquet")) |> 
    mutate(measure = "first_contact", .before = everything()) |> 
    select(measure, measure_type = contact_type, !!sym(dataset_type_o), !!sym(hb_name_o), month = app_month,  
           n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(!!dataset_type_o, !!hb_name_o)
  
  open_cases <- read_parquet(paste0(open_cases_dir, "/comp_data_opencases_CAMHS.parquet")) |> 
    mutate(measure = "open_cases", .before = everything()) |> 
    select(measure, measure_type = demand_type, !!sym(dataset_type_o), !!sym(hb_name_o), month, n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(!!dataset_type_o, !!hb_name_o)
  
  waits_patients_seen <- read_parquet(paste0(patients_seen_dir, "/comp_data_patientsseen.parquet")) |> 
    mutate(measure = "waits_patients_seen", .before = everything()) |> 
    select(measure, measure_type = waiting_period, !!sym(dataset_type_o), !!sym(hb_name_o), month = app_month, 
           n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(!!dataset_type_o, !!hb_name_o)
  
  waits_patients_waiting <- read_parquet(paste0(patients_waiting_dir, "/comp_data_patients_waiting_monthly.parquet")) |> 
    select(measure, measure_type, !!sym(dataset_type_o), !!sym(hb_name_o), month, n_aggregate, n_captnd, captnd_perc_agg) |>
    arrange(!!dataset_type_o, !!hb_name_o)
  
  referrals <- read_parquet(paste0(referrals_dir, "/comp_data_referrals.parquet")) |> 
    mutate(measure = "referrals", .before = everything()) |> 
    select(measure, measure_type = ref_acc_last_reported, !!sym(dataset_type_o), !!sym(hb_name_o), month = referral_month, n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(!!dataset_type_o, !!hb_name_o)
  
  # combine into one df
  df_mega <- rbind(
    dna,
    first_contact,
    open_cases,
    waits_patients_seen,
    waits_patients_waiting,
    referrals) |> 
    arrange(!!hb_name_o)
  

  # apply criteria for further investigation
  
  measures_vec <- unique(df_mega$measure)
  tolerance_value = 10
  
  df_id_for_checking <- df_mega |> # used later for getting patient-level data
    mutate(perc_over_agg = captnd_perc_agg - 100) |> 
    filter(perc_over_agg > tolerance_value & 
            month == max(df_mega$month, na.rm = TRUE)) |>  # latest month
    arrange(measure, !!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(comp_report_dir_patient_data, "/records_to_find"))
  
  
  # split into one df per HB
  df_mega_list <- df_mega |> 
    ungroup() |> 
    group_by(!!sym(hb_name_o)) |> 
    group_split() |> 
    setNames(unique(df_mega$hb_name))
  

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












#############################################################.
### Find and combine aggregate and CAPTND comparison data ### 
#############################################################.

# Author: Charlie Smith
# Date: 2024-03-26

combine_comparison_data <- function(){
  
  # load data
  
  # Quickly get filespaths for all .parquet files in folder
  # files <- list.files(path = paste0("../../../output/analysis_", data_analysis_latest_date, "/data_export/"),
  #                     full.names = TRUE, recursive = TRUE, pattern = ".parquet$")
   
  dna <- read_parquet("../../../output/analysis_2024-03-18/data_export/dna/comp_data_dna.parquet") |> 
    mutate(measure = "dna", .before = everything()) |> 
    select(measure, dataset_type, hb_name, month = app_month, n_aggregate, 
           n_captnd = app_count, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  first_contact <- read_parquet("../../../output/analysis_2024-03-18/data_export//first_contact/comp_data_firstcontact.parquet") |> 
    mutate(measure = "first_contact", .before = everything()) |> 
    select(measure, measure_type = contact_type, dataset_type, hb_name, month = app_month,  
           n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  open_cases <- read_parquet("../../../output/analysis_2024-03-18/data_export//open_cases/comp_data_opencases_CAMHS.parquet") |> 
    mutate(measure = "open_cases", .before = everything()) |> 
    select(measure, measure_type = demand_type, dataset_type, hb_name, month, n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  waits_patients_seen <- read_parquet("../../../output/analysis_2024-03-18/data_export//patients_seen/comp_data_patientsseen.parquet") |> 
    mutate(measure = "waits_patients_seen", .before = everything()) |> 
    select(measure, measure_type = waiting_period, dataset_type, hb_name, month = app_month, 
           n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  # waits_patients_waiting <- read_parquet("../../../output/analysis_2024-03-18/data_export//patients_waiting/by_month/monthly_waits_patients_waiting_hb.parquet") |> 
  #   mutate(measure = "waits_patients_waiting", .before = everything()) |> 
  #   select(measure, measure_type = wait_group_unadj, dataset_type, hb_name, month = month_start, 
  #          n_aggregate, n_captnd = n, captnd_perc_agg) |> 
  #   arrange(dataset_type, hb_name)
  
  referrals <- read_parquet("../../../output/analysis_2024-03-18/data_export//referrals/comp_data_referrals.parquet") |> 
    mutate(measure = "referrals", .before = everything()) |> 
    select(measure, measure_type = ref_acc_last_reported, dataset_type, hb_name, month = referral_month, n_aggregate, n_captnd = n, captnd_perc_agg) |> 
    arrange(dataset_type, hb_name)
  
  
  
  
  
  
}

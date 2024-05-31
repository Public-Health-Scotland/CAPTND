
####################################.
### Publication - non-acceptance ###
####################################.

# Author: Charlie Smith
# Date: 2024-05-31

summarise_non_acceptance <- function(df){
  
  # create for for saving output files in
  non_acc_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance/")
  dir.create(non_acc_dir)
  measure_label <- "non_acceptance"
  
  # get referral source lookup table
  lookup_acc <- import('../../../data/captnd_codes_lookup.xlsx', which = 'Ref_Accepted') |> 
    rename(ref_acc_last_reported = Code,
           ref_acc_desc = Values) |> 
    select(1:2)
  
  # get data to work on
  df_rej_reasons <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    mutate(ref_rej_month = floor_date(ref_rej_date, unit = "months")) |> 
    filter(ref_rej_month %in% date_range) 
    
    
  
}







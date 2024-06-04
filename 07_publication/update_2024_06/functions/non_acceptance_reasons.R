
###############################################.
### Publication - non-acceptance by reasons ###
###############################################.

# Author: Charlie Smith
# Date: 2024-05-31

summarise_non_acceptance_reason <- function(df){
  
  # create for for saving output files in
  non_acc_reasons_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance_reasons/")
  dir.create(non_acc_reasons_dir)
  measure_label <- "non_acceptance_reasons"
  
  # get referral source lookup table
  lookup_rej_reasons <- import('../../../data/captnd_codes_lookup.xlsx', which = 'Rej_Reason') |> 
    rename(ref_rej_reason = REJ_REASON,
           ref_rej_reason_desc = Rej_Reason) |> 
    select(1:2)
  
  # get data to work on
  df_rej_reasons <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    mutate(ref_rej_month = floor_date(ref_rej_date, unit = "months")) |> 
    filter(ref_rej_month %in% date_range) 
    
    
  
}







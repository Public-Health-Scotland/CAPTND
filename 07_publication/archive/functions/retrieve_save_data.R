
#######################################.
### Retrieve and save required data ###
#######################################.

# Author: Charlie Smith
# Date: 2023-11-14


retrieve_save_data <- function(){
  
  df_captnd <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    mutate(ref_month = floor_date(ref_rec_date_opti, unit = "month"),
           ref_quarter = ceiling_date(ref_month, unit = "quarter") - 1,
           ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
    select(-ref_quarter) |> 
    filter(ref_month %in% date_range) |>  # only looking at referrals for now
    save_as_parquet(path = paste0(data_working_safe, "captnd_pub"))  
  
  #return(df_captnd)
  
}


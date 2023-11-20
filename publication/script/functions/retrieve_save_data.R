
#######################################.
### Retrieve and save required data ###
#######################################.

# Author: Charlie Smith
# Date: 2023-11-14


retrieve_save_data <- function(){
  
  df_captnd <- open_last_parquet_with_rrt_eval()['df'][[1]] |>
    mutate(ref_month = floor_date(ref_rec_date_opti, unit = "month")) |> 
    filter(ref_month %in% date_range) |>  # only looking at referrals for now
    save_as_parquet(path = paste0(data_working_safe, "captnd_pub"))  
  
}



##################################################.
### Create table for quarterly referrals by HB ###
##################################################.

create_table_referrals_quarterly <- function(){
  
  measure_label <- "referrals_"
  
  # load data
  df_ref <- read_parquet(paste0(ref_dir, measure_label, "quarter_hb.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    mutate(quarter_ending = format(quarter_ending, "%b '%y"), 
           count = format(count, big.mark = ",")) |> 
    pivot_wider(names_from = quarter_ending, values_from = count, values_fill = "..") |> 
    rename(`Health board` = hb_name) |> 
    save_as_parquet(paste0(ref_dir, "table_referrals_quarterly"))

}
  
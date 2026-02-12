
##################################################.
### Create table for quarterly referrals by HB ###
##################################################.

create_table_referrals_quarterly <- function(){
  
  measure_label <- "referrals_"
  
  # load data
  df_ref <- read_parquet(paste0(ref_dir, measure_label, "quarter_hb.parquet")) |> 
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    mutate(quarter_ending = format(quarter_ending, "%b-%y"), 
           count = format(count, big.mark = ",")) |> 
    pivot_wider(names_from = quarter_ending, values_from = count, values_fill = "..") |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    
    rename(`Health board` = !!sym(hb_name_o)) |> 
    filter(`Health board` != "NHS 24")
  
    df_ref[is.na(df_ref)] <- ".."
    df_ref[df_ref == 0] <- "-"
    
    save_as_parquet(df_ref, paste0(ref_dir, "table_referrals_quarterly"))
  
}
  
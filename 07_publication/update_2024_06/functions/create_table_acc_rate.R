
########################################################.
### Create Table 2: acceptance rates, latest quarter ###
########################################################.

# Author: Charlie Smith
# Date: 2024-07-02


create_table_acceptance_rate <- function(){
  
  measure_label <- "non_acceptance_summary_"
  
  # load data
  df_acc <- read_parquet(paste0(non_acc_dir, measure_label, "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending)) |> 
    select(-c(quarter_ending, total, prop)) |> 
    mutate(ref_acc_desc = case_when(
      is.na(ref_acc_desc) | ref_acc_desc == "Pending" | ref_acc_desc == "No information" ~ "Other",
      TRUE ~ ref_acc_desc), 
           ref_acc_desc = factor(ref_acc_desc, 
                                 levels = c('Referral accepted', "Referral not accepted", "Other"))) |> 
    group_by(dataset_type, hb_name, ref_acc_desc) |> 
    summarise(count = sum(count, na.rm = TRUE)) |> 
    pivot_wider(names_from = ref_acc_desc, values_from = count, 
                values_fill = 0) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    mutate(hb_name = as.character(hb_name)) |> 
    as.data.frame() |> 
    mutate(total = `Referral accepted` + `Referral not accepted`+ Other,
           prop_accepted = as.double(round(`Referral accepted` / total * 100, 1))) 
    
  df_acc[is.na(df_acc)] <- ".."
  df_acc[df_acc == 0] <- "-" 
    
  df_acc <- df_acc |> 
    mutate(prop_accepted = paste0(prop_accepted, "%"), 
           `Referral accepted` = format(`Referral accepted`, big.mark = ","),
           `Referral not accepted` = format(`Referral not accepted`, big.mark = ","),
           `Other` = format(`Other`, big.mark = ","),
           `total` = format(`total`, big.mark = ",")) |> 
    rename(`Health board` = hb_name, Accepted = `Referral accepted`, 
           `Not accepted` = `Referral not accepted`, Total = total, 
           `Acceptance rate` = prop_accepted)
  
  df_acc[df_acc == "..%"] <- ".." 
  df_acc[df_acc == "-%"] <- "0.0%"   
  
    save_as_parquet(df_acc, paste0(non_acc_dir, "table_acc_rate"))
  
}




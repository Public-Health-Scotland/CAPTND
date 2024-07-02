
########################################################.
### Create Table 2: acceptance rates, latest quarter ###
########################################################.

# Author: Charlie Smith
# Date: 2024-07-02


create_table_acceptance_rate <- function(){
  
  # load data
  df_acc <- read_parquet(paste0(non_acc_dir, measure_label, "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending)) |> 
    select(-c(quarter_ending, total, prop)) |> 
    mutate(ref_acc_desc = case_when(
      is.na(ref_acc_desc) ~ "Other", 
      ref_acc_desc == "Pending" ~ "Other", 
      TRUE ~ ref_acc_desc), 
           ref_acc_desc = factor(ref_acc_desc, 
                                 levels = c('Referral accepted', "Referral not accepted", "Other"))) |> 
    arrange(dataset_type, hb_name, ref_acc_desc) |> 
    mutate(row = row_number()) |> 
    pivot_wider(names_from = ref_acc_desc, values_from = count, 
                values_fill = 0
                ) |> 
    select(-row) |> 
    mutate(total = `Referral accepted` + `Referral not accepted`+ Other, 
           prop_accepted = round(`Referral accepted` / total * 100, 1)) |> 
    save_as_parquet(paste0(non_acc_dir, "table_acc_rate"))
  
}




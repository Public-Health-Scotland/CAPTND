
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
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_acc_desc) |> 
    summarise(count = sum(count, na.rm = TRUE)) |> 
    pivot_wider(names_from = ref_acc_desc, values_from = count, 
                values_fill = 0) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    as.data.frame() |> 
    mutate(total = `Referral accepted` + `Referral not accepted`+ Other,
           prop_accepted = as.double(round(`Referral accepted` / total * 100, 1))) 
    
  df_acc <- df_acc |> 
    mutate(prop_accepted = paste0(prop_accepted, "%")) |> 
    rename(`Health board` = !!sym(hb_name_o), 
           Accepted = `Referral accepted`, 
           `Not accepted` = `Referral not accepted`, Total = total, 
           `Acceptance rate` = prop_accepted)
    
  df_acc$Accepted <- trimws(format(df_acc$Accepted, big.mark = ","))
  df_acc$`Not accepted` <- trimws(format(df_acc$`Not accepted`, big.mark = ","))
  df_acc$Other <- trimws(format(df_acc$Other, big.mark = ","))
  df_acc$Total <- trimws(format(df_acc$Total, big.mark = ","))
  
  df_acc[df_acc == "0"] <- "-"
  df_acc[df_acc == "NA"] <- ".."
  df_acc[df_acc == "0%"] <- "-"
  df_acc[df_acc == "NA%"] <- ".."
  
  save_as_parquet(df_acc, paste0(non_acc_dir, "table_acc_rate"))
  
}




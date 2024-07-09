
#########################################################################.
### Create Table 3: not accepted by reason and action, latest quarter ###
#########################################################################.

# Author: Charlie Smith
# Date: 2024-07-04


create_table_acceptance_reason_action <- function(){
  
  df_reason <- read_parquet(paste0(non_acc_reason_dir, "non_acceptance_reason_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending, na.rm = TRUE) #&
             #hb_name == "NHS Scotland"
           ) |> 
    select(-c(quarter_ending, total, prop)) |> 
    mutate(ref_rej_reason_desc = if_else(ref_rej_reason_desc == "Unsuitable", "Unsuitable", "Other"),
           ref_rej_reason_desc = factor(ref_rej_reason_desc, levels = c("Unsuitable", "Other"))) |> 
    group_by(dataset_type, hb_name, ref_rej_reason_desc) |> 
    summarise(count = sum(count), .groups = "drop") |>
    group_by(dataset_type, hb_name) |> 
    mutate(total = sum(count)) |> 
    pivot_wider(names_from = "ref_rej_reason_desc", 
                values_from = count, 
                values_fill = 0)
  
  
  df_action <- read_parquet(paste0(non_acc_action_dir, "non_acceptance_action_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending, na.rm = TRUE) #&
             #hb_name == "NHS Scotland"
             ) |> 
    select(-c(quarter_ending, total, prop)) |> 
    mutate(ref_rej_act_desc = case_when(
      ref_rej_act_desc == "Onward referral - in NHS" ~ "Onward referral/signposting",
      ref_rej_act_desc == "Onward referral - outside NHS" ~ "Onward referral/signposting",
      ref_rej_act_desc == "Returned to original referrer - with signposting" ~ "Onward referral/signposting",
      ref_rej_act_desc == "Signposted" ~ "Onward referral/signposting",
      TRUE ~ "Other"),
      ref_rej_act_desc = factor(ref_rej_act_desc, levels = c("Onward referral/signposting", "Other"))) |> 
    group_by(dataset_type, hb_name, ref_rej_act_desc) |> 
    summarise(count = sum(count), .groups = "drop") |>
    #group_by(dataset_type, hb_name) |> 
    #mutate(total = sum(count)) |> 
    pivot_wider(names_from = "ref_rej_act_desc", 
                values_from = count, 
                values_fill = 0)
    
  
  df_joined <- df_reason |> 
    left_join(df_action, by = c("dataset_type", "hb_name")) |> 
    save_as_parquet(paste0(non_acc_dir, "table_acc_reason_action"))
  
}









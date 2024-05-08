
################################.
### Append submission status ###
################################.

# Author: Charlie Smith
# Date: 2024-05-07

append_submission_status <- function(df){
  
  df_sub_status <- read_parquet(paste0(data_quality_report_dir, "/captnd_counts.parquet")) |> 
    group_by(header_date_month, dataset_type, hb_name) |> 
    summarise(submission_status = "submitted") |> 
    ungroup()
  
  df_status_added <- df |> 
    left_join(df_sub_status, by = c("header_date_month", "dataset_type", "hb_name")) |>  # add sub status to df
    mutate(submission_status = case_when(
      hb_name == "NHS 24" & dataset_type == "CAMHS" |
        dataset_type == "CAMHS" & variable %in% c("vet", "preg_perinatal_ref", "preg_perinatal_app", 
                                                  "act_code_sent_date", "treat_group_or_ind_1", "treat_group_or_ind_2",
                                                  "treat_group_or_ind_3") |
        dataset_type == "PT" & variable == "looked_after_c" ~ "no submission possible", # Jo would rather "NA" - I think more useful if descriptive
      is.na(submission_status) ~ "not submitted",
      variable %in% c("outcome_1", "outcome_2", "outcome_3") &
        header_date_month >= "2023-10-01" ~ "variable retired",
      hb_name == "NHS Scotland" ~ "not applicable",
      TRUE ~ submission_status)) %>% 
    ungroup() %>% 
    select(submission_status, everything())
  
  return(df_status_added)
  
}


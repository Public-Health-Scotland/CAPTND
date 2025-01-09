
#################################.
### Analyse Data Quality Data ###
#################################.

# Author: Charlie Smith
# Date: 2024-05-03


# ANALYSIS

# Phase 1
df <- load_dq_data() |> 
  filter_wrangle_data() |> 
  get_dq_counts() |> 
  add_scotland_totals() |> 
  save_as_parquet(paste0(data_quality_report_dir, "/captnd_counts"))

rm(df)
gc()

# Phase 2
df_counts <- read_parquet(paste0(data_quality_report_dir, "/captnd_counts.parquet")) |> 
  complete_absent_vars_na() |> 
  append_submission_status() |> 
  get_dq_proportions() |> 
  label_impossible_combis_na() |> 
  label_retired_variables_na() |> 
  append_patient_management_system() |> 
  # drop unnecessary vars?
  # add variable types? e.g. id_vars, ref_vars, etc.
  arrange_dq_df() |> 
  save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_all")) |>  
  filter(header_date_month == month_latest) |> # get latest month's data for DQ heatmaps 
  save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_latest"))

rm(df_counts)
gc()



#############################.
### Summarise Submissions ###
#############################.

# Author: Charlie Smith
# Date: 2024-05-21


df_submissions <- read_parquet(file = paste0(data_quality_report_dir, "/captnd_dq_clean_latest.parquet")) |> 
  get_submission_detail_table() |> 
  get_submission_summary_table()

rm(df_submissions)
  
  
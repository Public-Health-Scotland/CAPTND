
####################################.
### Get submission table summary ###
####################################.

# Author: Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2023-07-18

get_submission_summary_table <- function(df){
  
  submission_summary <- df %>% 
    group_by(dataset_type, `Submission Status`) %>% 
    summarise(Count = n()) %>% 
    mutate(Total = sum(Count, na.rm = TRUE),
           Proportion = round(Count / Total * 100, 1)) %>% 
    save_as_parquet(paste0(data_quality_report_dir, "/submission_summary"))
  
  return(submission_summary)
  
}
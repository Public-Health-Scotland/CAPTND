
###################################.
### Get Submission Detail Table ###
###################################.

# Author: Charlie Smith
# Date: 2024-05-21

get_submission_detail_table <- function(df){
  
  submission_detail <- df %>% 
    filter(! hb_name %in% c("NHS Scotland")) %>% 
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           submission_status = factor(submission_status, levels = c("not submitted", "submitted"))) %>% 
    filter(!is.na(hb_name) & !is.na(submission_status)) %>% 
    group_by(dataset_type, hb_name, pms, submission_status) %>% 
    slice(1) %>% 
    summarise(n = n()) %>% 
    arrange(dataset_type, submission_status) %>% 
    select(-n) %>% 
    rename(`Submission Status` = submission_status) %>% 
    ungroup() %>% 
    filter(dataset_type != "CAMHS" | hb_name != "NHS24") |> 
    save_as_parquet(paste0(data_quality_report_dir, "/submission_detail"))
  
  return(submission_detail)
  
}


###################################.
### Report negative waits by HB ###
###################################. 

# Author: Charlie Smith
# Date: 2024-01-22


report_negative_waits <- function(df){
  
  # load negative waits (patients seen)
  df_negs_seen <- import(paste0(patients_seen_dir,'/patients_negative_waitingTimes_seen.csv')) 
  
  # summarise count of negative waits (seen)
  df_neg_seen_summary <- df_negs_seen |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o))) |> 
    summarise(count_neg = n())
  
  # by quarter
  app_quarter_o <- "app_quarter" 
  
  df_neg_seen_summary_quart <- df_negs_seen |> 
    mutate(!!app_quarter_o := floor_date(!!sym(app_month_o), unit = "quarter")) |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, app_quarter_o))) |> 
    summarise(count_neg = n())
  
  # by year
  app_year_o <- "app_year"
  
  df_neg_seen_summary_year <- df_negs_seen |> 
    mutate(!!app_year_o := floor_date(!!sym(app_month_o), unit = "year")) |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, app_year_o))) |> 
    summarise(count_neg = n()) |> 
    ungroup()
  
  df_neg_seen_summary_year_latest <- df_neg_seen_summary_year |> 
    filter(!!sym(app_year_o) == max(!!sym(app_year_o)))
  
  # save both to one .xlsx file? 
  
  
  
}




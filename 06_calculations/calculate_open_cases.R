#####################################.
## Calculates open cases - Updated ##.
#####################################.

#author: JBS
#updated: Luke Taylor
#last updated: 13/12/24

#most_recent_month_in_data <- "2024-10-01"

# load data
#df_glob_swift_completed_rtt <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

source('06_calculations/save_data_board.R')

calculate_open_cases <- function(df_glob_swift_completed_rtt, most_recent_month_in_data) {
  
  sub_month_end <- ymd(most_recent_month_in_data)
  sub_month_start <- ymd(most_recent_month_in_data) - months(14)
  
  month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
  df_month_seq_start <- data.frame(sub_month_start = floor_date(month_seq, unit = "month")) 
  
  df_open <- df_glob_swift_completed_rtt %>% 
    select(!!!syms(data_keys), !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), 
           !!sym(rtt_eval_o), !!sym(referral_month_o),!!sym(case_closed_date_o), 
           !!sym(case_closed_month_o), !!sym(act_code_sent_date_o), !!sym(first_treat_app_o)) |>
    filter(!!sym(referral_month_o) <= most_recent_month_in_data) |> 
    
    #filter for patients with treatment start date
    filter(!is.na(!!sym(first_treat_app_o)) | !is.na(!!sym(act_code_sent_date_o))) |> #flag patients with first treat app or act code sent date
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_start) |>
    
    #filter for patients without case closed date, or case closed within last 15 months
    mutate(first_treat_app = case_when(is.na(!!sym(first_treat_app_o)) ~ !!sym(act_code_sent_date_o),
                                       TRUE ~ !!sym(first_treat_app_o))) |> #when first treat appt date missing complete using act_code_sent_date
    mutate(first_treat_app_month = floor_date(!!sym(first_treat_app_o), unit = "month")) |> #create first treatment appt month column
    filter(sub_month_start >= first_treat_app_month) |>
    filter(is.na(!!sym(case_closed_date_o)) | #flag patients without a case closed date
             sub_month_start <= !!sym(case_closed_date_o)) |> #flag rows with a sub_month less than or equal to case_closed_date
    as.data.frame() 
  
  
  df_open_complete <- df_open |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    group_by(sub_month_start, !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    save_as_parquet(path = paste0(comp_report_dir_patient_data, "/open_cases"))
  
  write_csv_arrow(df_open_complete, paste0(open_cases_dir,'/openCases_subSource.csv'))
  
  message(paste0('Your output files are in ',open_cases_dir))
  
}





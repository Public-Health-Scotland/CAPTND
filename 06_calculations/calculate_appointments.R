##################################.
###   Calculate appointments   ###
##################################.

#author: JBS
#date: 20/11/23

#Number of appointments per calendar month 
#Number of appointment days by calendar month 
#(1 patient with 2 apps on the same day for the same journey count as 1)
#add appointment month column (and add that to new_cols script)

# 1 load functions --------------------------------------------------------

source('06_calculations/save_data_board.R')
source('06_calculations/compare_app_days_app_number.R')
source('06_calculations/plot_line_app_days_app_count.R')
source('06_calculations/plot_rate_app_days_app_count.R')
source('06_calculations/plot_bar_outliers_app_days_app.R')


# 2 Function --------------------------------------------------------------

calculate_appointments <- function(df){
  
  df_app_pre_calc <- df %>% 
    filter(!is.na(!!sym(app_date_o))) %>% 
    group_by(across(all_of(c(data_keys,app_month_o,app_date_o)))) %>% 
    summarise(n_app_patient_same_day=n(), 
              .groups = 'drop') |> 
    distinct() |> 
    save_as_parquet(paste0(comp_report_dir_patient_data, "/all_appointments"))
  
  df_app_attended <- df %>% 
    filter(!is.na(!!sym(app_date_o)),
           !!sym(att_status_o) == 1) %>% 
    group_by(across(all_of(c(data_keys,app_month_o,app_date_o)))) %>% 
    summarise(n_app_patient_same_day=n(), 
              .groups = 'drop')
  
  df_over_4_apps <- df_app_pre_calc %>% 
    filter(n_app_patient_same_day>=4) %>% 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) %>% 
    group_split() %>% 
    map2(., 'patients_4plus_apps', save_data_board, appointments_dir_by_board)
  
  df_app_days <- df_app_pre_calc %>% 
    group_by(across(all_of(c(hb_name_o, dataset_type_o, app_month_o)))) %>% 
    summarise(n_app_days_month=n(),
              .groups = 'drop')
  
  df_app_number <- df %>% 
    filter(!is.na(!!sym(app_date_o))) %>% 
    group_by(across(all_of(c(hb_name_o, dataset_type_o, app_month_o)))) %>% 
    summarise(n_app_month=n(),
              .groups = 'drop')
    
  df_app <- inner_join(df_app_number, df_app_days, by=c(hb_name_o, dataset_type_o, app_month_o)) %>% 
    group_by(across(all_of(c(dataset_type_o, app_month_o)))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop"))
  
  
  df_app_days_details <- df %>% 
    filter(!is.na(!!sym(app_date_o))) %>% 
    select(all_of(c(data_keys, app_date_o, simd_quintile_o, sex_reported_o, age_group_o, app_month_o))) %>% 
    distinct() %>%
    group_by(across(all_of(c(hb_name_o, dataset_type_o, app_month_o, simd_quintile_o, sex_reported_o, age_group_o)))) %>% 
    summarise(n_app_days_month=n(),
              .groups = 'drop') 
  
  
  df_app_number_details <- df %>% 
    filter(!is.na(!!sym(app_date_o))) %>% 
    select(all_of(c(data_keys, app_date_o, simd_quintile_o, sex_reported_o, age_group_o, app_month_o))) %>%
    group_by(across(all_of(c(hb_name_o, dataset_type_o, app_month_o, simd_quintile_o, sex_reported_o, age_group_o)))) %>% 
    summarise(n_app_month=n(),
              .groups = 'drop')
  
  df_app_details <- inner_join(df_app_number_details, df_app_days_details, 
                               by=c(hb_name_o, dataset_type_o, app_month_o, simd_quintile_o, sex_reported_o, age_group_o))
    
  x=df_app %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'appointments', save_data_board, appointments_dir_by_board)

  write_csv_arrow(df_app, paste0(appointments_dir,'/appointments.csv'))
  
  write_csv_arrow(df_app_details, paste0(appointments_dir,'/appointments_sex_age_simd.csv'))
  
  # plot comparisons
  compare_app_days_app_number(df = df_app, ds_type = "CAMHS")
  compare_app_days_app_number(df = df_app, ds_type = "PT")
  
  plot_line_app_days_app_count(df_app, 'CAMHS')
  plot_line_app_days_app_count(df_app, 'PT')
  
  plot_rate_app_days_app_count(df_app)
  
  plot_bar_outliers_app_days_app(df_app_attended, 'CAMHS')   #change df to ad_app_pre_calc for all apps not attended apps
  plot_bar_outliers_app_days_app(df_app_attended, 'PT')
  
  
  message(paste0('Your output files are in ',appointments_dir))
  
  }













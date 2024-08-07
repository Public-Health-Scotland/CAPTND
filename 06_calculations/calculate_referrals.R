# library(dplyr)
# library(lubridate)
# library(arrow)
# library(phsmethods)
# library(purrr)
# library(tidyr)
# library(conflicted)
# conflict_prefer('filter','dplyr')
# conflict_prefer('mutate','dplyr')
# conflict_prefer('rename','dplyr')

source('06_calculations/save_data_board.R')
source('06_calculations/plot_referrals_sex.R')
source('06_calculations/plot_referrals_simd.R')
source('06_calculations/plot_referrals_age.R')


calculate_referrals <- function(df, extractDate) {
  
  df_referrals=df %>%
    select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) %>% 
    distinct() %>% 
    mutate(!!ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                  !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                  TRUE ~ 'pending')) %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(ref_acc_last_reported_o)) %>% 
    summarise(n=n()) %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!ref_acc_last_reported_o, ~"total"),
                        .groups = "drop")) #|> 
    #save_as_parquet(path = paste0(comp_report_dir_patient_data, "/referrals"))   # save df for later HB comparison checks 
  
  w=df_referrals %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'referrals', save_data_board, referrals_dir_by_board)
  
  df_referrals_details=df %>%
    filter(!is.na(!!sym(ref_acc_last_reported_o))) %>% 
    #mutate(!!referral_month_o := floor_date(!!sym(ref_rec_date_opti_o), 'month')) %>% 
    select(all_of(c(data_keys,ref_acc_last_reported_o, referral_month_o,simd_quintile_o, sex_reported_o, age_group_o))) %>% 
    distinct() %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o),
             !!sym(ref_acc_last_reported_o), !!sym(simd_quintile_o), !!sym(sex_reported_o), !!sym(age_group_o)) %>% 
    summarise(n=n(), .groups = 'drop') %>% 
    mutate(!!ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                  !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                  !!sym(ref_acc_last_reported_o)==3 ~ 'pending')) %>% 
    group_by(!!sym(referral_month_o),!!sym(ref_acc_last_reported_o),!!sym(dataset_type_o),
             !!sym(simd_quintile_o), !!sym(sex_reported_o), !!sym(age_group_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o),
             !!sym(simd_quintile_o), !!sym(sex_reported_o), !!sym(age_group_o)) %>%
    mutate(n_total=sum(n)) %>% 
    ungroup()

    
  plot_referrals_sex(df_referrals_details, 'CAMHS')
  plot_referrals_sex(df_referrals_details, 'PT')
  
  plot_referrals_simd(df_referrals_details, 'CAMHS')
  plot_referrals_simd(df_referrals_details, 'PT')
  
  plot_referrals_age(df_referrals_details, 'CAMHS')
  plot_referrals_age(df_referrals_details, 'PT')
    
  write_csv_arrow(df_referrals, paste0(referrals_dir,'/referrals.csv'))
  write_csv_arrow(df_referrals_details, paste0(referrals_dir,'/referrals_sex_age_simd.csv'))
  
  message(paste('Your files are in',referrals_dir))
  
}

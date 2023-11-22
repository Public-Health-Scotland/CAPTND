library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)
library(purrr)
library(tidyr)
library(conflicted)
conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')
conflict_prefer('rename','dplyr')

source('calculations/save_data_board.R')
source('calculations/plot_referrals.R')



calculate_referrals <- function(df, extractDate) {
  
  df_referrals=df %>%
    filter(!is.na(!!sym(ref_acc_o))) %>% 
    mutate(!!referral_month_o := floor_date(!!sym(ref_rec_date_opti_o), 'month')) %>% 
    select(all_of(data_keys),!!ref_acc_o, !!referral_month_o) %>% 
    distinct() %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(ref_acc_o)) %>% 
    summarise(n=n(), .groups = 'drop') %>% 
    mutate(!!ref_acc_o:=case_when(!!sym(ref_acc_o)==1 ~ 'accepted',
                                  !!sym(ref_acc_o)==2 ~ 'not accepted',
                                  !!sym(ref_acc_o)==3 ~ 'pending')) %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!ref_acc_o, ~"total"),
                        .groups = "drop"))
  
  w=df_referrals %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'referrals', save_data_board, referrals_dir_by_board)
  
  df_referrals_details=df %>%
    filter(!is.na(!!sym(ref_acc_o))) %>% 
    mutate(!!referral_month_o := floor_date(!!sym(ref_rec_date_opti_o), 'month')) %>% 
    select(all_of(c(data_keys,ref_acc_o, referral_month_o,simd_quintile_o, sex_reported_o, age_group_o))) %>% 
    distinct() %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o),
             !!sym(ref_acc_o), !!sym(simd_quintile_o), !!sym(sex_reported_o), !!sym(age_group_o)) %>% 
    summarise(n=n(), .groups = 'drop') %>% 
    mutate(!!ref_acc_o:=case_when(!!sym(ref_acc_o)==1 ~ 'accepted',
                                  !!sym(ref_acc_o)==2 ~ 'not accepted',
                                  !!sym(ref_acc_o)==3 ~ 'pending')) %>% 
    group_by(!!sym(referral_month_o),!!sym(ref_acc_o),!!sym(dataset_type_o),
             !!sym(simd_quintile_o), !!sym(sex_reported_o), !!sym(age_group_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) %>% 
    group_by(!!sym(referral_month_o),!!sym(hb_name_o),!!sym(dataset_type_o),
             !!sym(simd_quintile_o), !!sym(sex_reported_o), !!sym(age_group_o)) %>%
    inner_join(summarise(.,
                        across(where(is.numeric), sum),
                        .groups = "drop"),
               by=c(referral_month_o,hb_name_o,dataset_type_o,
                    simd_quintile_o, sex_reported_o,age_group_o)) %>% 
    rename(n=`n.x`, n_total=`n.y`)
    
  plot_referrals(df_referrals_details, 'NHS Scotland', sex_reported_o, 'month', 'n')
  plot_referrals(df_referrals_details, 'NHS Scotland', sex_reported_o, 'month', 'n_perc')
    
  write_csv_arrow(df_referrals, paste0(referrals_dir,'/referrals.csv'))
  write_csv_arrow(df_referrals_details, paste0(referrals_dir,'/referrals_sex_age_simd.csv'))
  
  message(paste('Your files are in',referrals_dir))
  
}

#patients with reportyed dob and chi dob not matching


library(dplyr)
library(readr)
library(lubridate)


report_dob_conflicting <- function(df_dob) {
  
  df_dob_conflicting <- df_dob %>%
    filter(!!sym(dob_recorded_matches_chi_o)=='no match') %>%
    select(!!hb_name_o,!!dataset_type_o,!!chi_o,!!dob_from_chi_o,!!dob_o,!!dob_recorded_matches_chi_o,!!dob_verified_o) %>%
    distinct()
  
  df_dob_conflicting_gen_stats <- df_dob_conflicting %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    summarise(n_conflicting_dob=n(), .groups = 'drop')
  
  df_dob_conflicting_detailed_stats <-  df_dob %>%
    filter(!!sym(dob_recorded_matches_chi_o)=='no match') %>%
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
    select(!!hb_name_o,!!dataset_type_o,!!submission_date_o,!!chi_o,!!dob_from_chi_o,!!dob_o,!!dob_recorded_matches_chi_o,!!dob_verified_o) %>%
    distinct() %>%  
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
    summarise(n_conflicting_dob=n(), .groups = 'drop')
  
  write_csv(df_dob_conflicting,paste0(dob_conflicting_dir,"/dob_conflicting.csv"))
  write_csv(df_dob_conflicting_gen_stats,paste0(dob_conflicting_dir,"/dob_conflicting_gen_stats.csv"))
  write_csv(df_dob_conflicting_detailed_stats,paste0(dob_conflicting_dir,"/dob_conflicting_detailed_stats.csv"))
  
  npatients=nrow(df_dob_conflicting)
  
  message(paste(npatients,'patients with conflicting dob between reported and chi were found\n',
                'More details can be found in',dob_conflicting_dir))
  
}


  
  
  #write_csv(df_dob_conflicting, paste0('../../../output/dob_conflicting_', today(), '.csv'))


# x=read_parquet('../../../output/df_glob_swift_completed.parquet')
# 
# 
# y=x %>% filter(dataset_type=='CAMHS',
#                hb_name=='NHS Greater Glasgow and Clyde',
#                ucpn=='103105235',
#                patient_id=='0605085463')
# 
# y2=complete_diag_outc_into_appt(y)
# 
# 
# x_with_diag=x %>% filter(!is.na(diag_1))
# 
# z=x %>% 
#   select(hb_name,dataset_type,patient_id,dob,dob_from_chi,dob_recorded_matches_chi) %>% 
#   distinct() %>% 
#   filter(dob_recorded_matches_chi == 'no match')

#df_dob=x


##################################.
### Product 2 - data wrangling ###
##################################.

#author: Charlie Smith & JBS
#date: 23/11/23


# 1 Load libraries --------------------------------------------------------

#library()


# 2 Function --------------------------------------------------------------

calculate_product2_df <- function(df){

  
  df_rtt <- df %>% 
    filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>% 
    group_by(!!!syms(data_keys)) %>% 
    mutate(rtt_possible=case_when(
      
      #first case is patients seen
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !is.na(!!sym(app_date_o)) &
          !!sym(ref_acc_o) == 1 &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(2,3,5)
        ) ~ 'seen',
      #second case is patients who had online treatment
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !!sym(ref_acc_o) == 1 &
          !is.na(!!sym(act_code_sent_date_o))
        ) ~ 'seen - online',
      
      #patients assessed and then discharged
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !is.na(!!sym(app_date_o)) &
          !!sym(ref_acc_o) == 1 &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(1,4,6) &
          !is.na(!!sym(case_closed_date_o))
        
      ) ~ 'case closed after assessment',
      
      #patients waiting (seen but no treatment)
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !is.na(!!sym(app_date_o)) &
          !!sym(ref_acc_o) == 1 &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(1,4,6)
      ) ~ 'waiting - after assessment',
      
      #patients waiting - no app purpose information
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !is.na(!!sym(app_date_o)) &
          !!sym(ref_acc_o) == 1 &
          !!sym(att_status_o) == 1 &
          !(!!sym(app_purpose_o) %in% c(2,3,5))
      ) ~ 'unknown',
      
      #case closed due to no attendance
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !!sym(ref_acc_o) == 1 &
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) %in% c(2,3,5,8) &
          !is.na(!!sym(case_closed_date_o))
      ) ~ 'case closed due to non attendance',
      
      #patients waiting no attendance
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !!sym(ref_acc_o) == 1 &
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) %in% c(2,3,5,8) 
      ) ~ 'waiting - not attended',
      
      #case closed prior to app
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !!sym(ref_acc_o) == 1 &
            is.na(!!sym(app_date_o)) &
          !is.na(!!sym(case_closed_date_o))
      ) ~ 'case closed with no app',
      
      #patients waiting prior to app
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !!sym(ref_acc_o) == 1 &
          is.na(!!sym(app_date_o))
      ) ~ 'waiting - no app',
      
      
      #referral pending
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          is.na(!!sym(app_date_o)) &
          (!!sym(ref_acc_o) == 3 | is.na(!!sym(ref_acc_o)))
      ) ~ 'referral pending',
      
      #referral rejected
      any(
        !is.na(!!sym(ref_rec_date_opti_o)) &
          !!sym(ref_acc_o) == 2 
      ) ~ 'referral not accepted',
      
      TRUE ~ 'unknown'
      ),
      .after=rtt_eval) %>% 
    ungroup()
  
  df_rtt_stats <- df_rtt %>% 
    filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>% 
    select(all_of(data_keys),rtt_possible) %>% 
    distinct() %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o)),rtt_possible) %>% 
    summarise(n=n(),
              .groups='drop') %>%
    group_by(!!!syms(c(hb_name_o,dataset_type_o))) %>% 
    inner_join(summarise(.,
                         across(where(is.numeric), sum),
                         .groups = "drop"),
               by=c(hb_name_o,dataset_type_o)) %>% 
    rename(n=`n.x`,
           total=`n.y`)
    
  
}



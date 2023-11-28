########################.
### RTT - evaluation ###
########################.

#author: JBS
#date: 28/11/23


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------


add_rtt_eval <- function(df) {
  
  df_rtt <- df %>% 
    filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>% 
    group_by(!!!syms(data_keys)) %>% 
    
    mutate(
      ref_acc_last_reported := last(!!sym(ref_acc_o),order_by=!!sym(header_date_o), na_rm = TRUE),
      .after=rtt_eval
    ) %>% 
    
    mutate(
      has_any_app_date = case_when(any(!is.na(app_date)) ~ TRUE,
                                   TRUE ~ FALSE),
      has_ref_rec_date_opti = case_when(any(!is.na(ref_rec_date_opti)) ~ TRUE,
                                        TRUE ~ FALSE),
      has_act_code_sent_date = case_when(any(!is.na(act_code_sent_date)) ~ TRUE,
                                         TRUE ~ FALSE),
      is_case_closed = case_when(any(!is.na(case_closed_date)) ~ TRUE,
                                 TRUE ~ FALSE),
      .after=rtt_eval
    ) %>% 
    
    mutate(!!rtt_eval_o := case_when(
      
      #first case is patients seen whose treatment is ongoing
      has_ref_rec_date_opti == TRUE &
        has_any_app_date == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(2,3,5)
        ) ~ 'seen - active',
      
      #next case is patients seen whose treatment is finished
      has_ref_rec_date_opti == TRUE &
        has_any_app_date == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(2,3,5)
        ) ~ 'seen - closed',
      
      #second case is patients who had online treatment which is still ongoing
      has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        has_any_app_date == FALSE &
        ref_acc_last_reported == 1 &
        any(
          has_act_code_sent_date == TRUE
        ) ~ 'seen - online - active',
      
      #another case is patients who had online treatment that has finished
      has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        has_any_app_date == FALSE &
        ref_acc_last_reported == 1 &
        any(
          has_act_code_sent_date == TRUE 
        ) ~ 'seen - online - closed',
      
      # patients assessed and then discharged
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(1,4,6)
          
        ) &
        !any(
          !is.na(!!sym(app_date_o)) &
            !!sym(app_purpose_o) %in% c(2,3,5) 
        ) ~ 'case closed after assessment',
      
      #patients waiting (assessed but no treatment)
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(1,4,6)
        ) ~ 'waiting - after assessment',
      
      #rtt not possible - no app purpose information
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        ref_acc_last_reported == 1 &
        (is.na(!!sym(app_purpose_o)) | !!sym(app_purpose_o) == 99) &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 
        ) ~ 'rtt not possible - attended app but no purpose',
      
      #rtt not possible - no app attendance information
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        #is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        (!!sym(att_status_o) == 99 | is.na(!!sym(att_status_o))) 
      ~ 'rtt not possible - app date but no attendance status',
      
      
      #case closed due to no attendance
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) %in% c(2,3,5,8) 
        ) ~ 'case closed due to non attendance',
      
      #patients waiting no attendance
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) %in% c(2,3,5,8) 
        ) ~ 'waiting - not attended',
      
      #case closed prior to app
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 
      ~ 'case closed with no app',
      
      #patients waiting prior to app
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1
      ~ 'waiting - no app',
      
      
      #referral pending
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        (ref_acc_last_reported == 3 | is.na(ref_acc_last_reported))
      ~ 'referral pending',
      
      #referral pending but case closed
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        (ref_acc_last_reported == 3 | is.na(ref_acc_last_reported))
      ~ 'case closed - referral pending',
      
      #referral rejected
      #has_any_app_date == FALSE & #patient could have assessment appt for example
      has_ref_rec_date_opti == TRUE &
        ref_acc_last_reported == 2 
      ~ 'referral not accepted',
      
      #no ref acc but has app date
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is.na(!!sym(ref_acc_o)) 
      ~ 'rtt not possible - app with no referral acc',
      
      #referral pending but person had appt
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        (ref_acc_last_reported == 3 | is.na(ref_acc_last_reported))
      ~ 'rtt not possible - patient had appt and ref is pending',
      
      
      TRUE ~ 'rtt not possible - unknown'),
      .after=!!chi_valid_o) %>% 
    ungroup()
  
  
  return(df_rtt)

}
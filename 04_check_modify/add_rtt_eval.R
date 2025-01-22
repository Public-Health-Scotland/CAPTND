########################.
### RTT - evaluation ###
########################.

#author: JBS
#date: 28/11/23
#updated by Bex  Madden - 22/01/2025


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------


add_rtt_eval <- function(df, evalAllData=FALSE) {
  
  if(evalAllData==FALSE){
    df=df %>% filter(!!sym(ref_rec_date_opti_o) >= ymd(210801))
  }
  
  
  df_rtt <- df %>% 
    #filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>% 
    group_by(!!!syms(data_keys)) %>% 
    
    mutate(
      ref_acc_last_reported := last(!!sym(ref_acc_o),order_by=!!sym(header_date_o), na_rm = TRUE),
      
      has_any_app_date = fcase(any(!is.na(app_date)), TRUE,
                                   default = FALSE),
      has_ref_rec_date_opti = fcase(any(!is.na(ref_rec_date_opti)), TRUE,
                                    default = FALSE),
      has_act_code_sent_date = fcase(any(!is.na(act_code_sent_date)), TRUE,
                                     default = FALSE),
      is_case_closed = fcase(any(!is.na(case_closed_date)), TRUE,
                             default = FALSE),
      .after=!!ref_acc_o
    ) %>% 
    
    mutate(!!rtt_eval_o := fcase(
      
      #first case is patients seen whose treatment is ongoing but start_treat columns is filled
      has_ref_rec_date_opti == TRUE &
        #has_any_app_date == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
            !is.na(!!sym(first_treat_app_o))
        ), 'seen - active', 
      
      #other case is patients seen whose treatment is ongoing
      has_ref_rec_date_opti == TRUE &
        has_any_app_date == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(2,3,5)
        ), 'seen - active',
      
      #other case is patients seen whose treatment is finished and start_treat columns is filled
      has_ref_rec_date_opti == TRUE &
        #has_any_app_date == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
        any(
            !is.na(!!sym(first_treat_app_o))
        ), 'seen - closed', 
      
      #next case is patients seen whose treatment is finished
      has_ref_rec_date_opti == TRUE &
        has_any_app_date == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(2,3,5)
        ), 'seen - closed',
      
      
      #second case is patients who had online treatment which is still ongoing
      has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        has_any_app_date == FALSE &
        ref_acc_last_reported == 1 &
        any(
          has_act_code_sent_date == TRUE
        ), 'seen - online - active',
      
      #another case is patients who had online treatment that has finished
      has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        has_any_app_date == FALSE &
        ref_acc_last_reported == 1 &
        any(
          has_act_code_sent_date == TRUE 
        ), 'seen - online - closed',
      
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
        ), 'case closed after assessment',
      
      #patients waiting (assessed but no treatment)
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 &
            !!sym(app_purpose_o) %in% c(1,4,6)
        ), 'waiting - after assessment',
      
      #rtt not possible - no app purpose information
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        ref_acc_last_reported == 1 &
        (is.na(!!sym(app_purpose_o)) | !!sym(app_purpose_o) == 99) &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) == 1 
        ), 'rtt not possible - attended app but no purpose', 
      
      #rtt not possible - no app attendance information
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        #is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        (!!sym(att_status_o) == 99 | is.na(!!sym(att_status_o))) 
      , 'rtt not possible - app date but no attendance status', 
      
      
      #case closed due to no attendance
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) %in% c(2,3,5,8) 
        ), 'case closed due to non attendance', 
      
      #patients waiting no attendance
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        any(
          !is.na(!!sym(app_date_o)) &
            !!sym(att_status_o) %in% c(2,3,5,8) 
        ), 'waiting - not attended', 
      
      #case closed prior to app
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 
      , 'case closed with no app',
      
      #patients waiting prior to app
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1
      , 'waiting - no app',
      
      
      #referral pending
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        (ref_acc_last_reported == 3 | is.na(ref_acc_last_reported))
      , 'referral pending',
      
      #referral pending but case closed
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        (ref_acc_last_reported == 3 | is.na(ref_acc_last_reported))
      , 'case closed - referral pending',
      
      #referral rejected
      #has_any_app_date == FALSE & #patient could have assessment appt for example
      has_ref_rec_date_opti == TRUE &
        ref_acc_last_reported == 2 
      , 'referral not accepted',
      
      #no ref acc but has app date
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is.na(!!sym(ref_acc_last_reported_o)) 
      , 'seen - app with no referral acc', #changed to 'seen' 22/01/2025 to allow ref_acc pending/NA
      
      #referral pending but person had appt
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        (ref_acc_last_reported == 3)
      , 'seen - patient had appt and ref is pending', #changed to 'seen' 22/01/2025 to allow ref_acc pending/NA
      
      
      default = 'rtt not possible - unknown'),
      .after=!!chi_valid_o) %>% 
    ungroup()
  
  message('RTT evaluation added\n')
  return(df_rtt)

}
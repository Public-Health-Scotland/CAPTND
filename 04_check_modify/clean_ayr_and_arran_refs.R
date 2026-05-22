#########################################################
## NHS Ayrshire and Arran Referral Preference function ##
#########################################################

#Function has been developed to preferentially use the first referral record
#in a pathway where more than one referral record has been submitted and more than one
#ref_rec_date exists.
#NHS Ayrshire and Arran asked that the first referral be used as the valid clock start 
#for each pathway. If an accepted referral exists for a pathway, all other referral records
#are updated to 01 'Accepted', and arranged in date order.
#The earliest accepted referral based on the updated status is then kept.
#If only pending and rejected referrals exists for a pathway, precedence is given to the pending 
#referral when reordering these.
#Every opportunity is therefore given to keep the pathway open and active in CAPTND.

#Author: Luke Taylor
#Date: 22/04/2026

ayr_and_arran_ref_fix <- function(df){
  
  identify_dup_aaa_refs <- df |>
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o), !!sym(ucpn_o), !!sym(chi_o)) |>
    mutate(ref_rec_count = sum(!is.na(!!sym(ref_rec_date_o))),
           aaa_pt_dup_flag = !!sym(hb_name_o) == "NHS Ayrshire and Arran" & !!sym(dataset_type_o) == "PT" &
             ref_rec_count >= 2,
           any_accept_ref = ref_rec_count >= 2 & any(!!sym(ref_acc_o) == "01", na.rm = TRUE)) |>
    ungroup() 
  
  clean_refs_ayr_and_arran <- identify_dup_aaa_refs |>
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o), !!sym(ucpn_o), !!sym(chi_o)) |>
    mutate(ref_acc = case_when(ref_rec_count >= 2 & any_accept_ref == TRUE & !is.na(ref_rec_date) ~ "01",
                               TRUE ~ ref_acc)) |>
    arrange(!!sym(ucpn_o),
            case_when(aaa_pt_dup_flag ~ match(!!sym(ref_acc_o), c("01","03","02")),
                      TRUE ~ Inf),
            !!sym(ref_rec_date_o), .by_group = TRUE) |>
    mutate(first_ref_rec_date_flag = case_when(aaa_pt_dup_flag == TRUE & row_number() == 1 ~ TRUE,
                                               aaa_pt_dup_flag == FALSE ~ TRUE,
                                               is.na(!!sym(ref_rec_date_o)) & 
                                                 is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_date_o)) ~ TRUE,
                                               TRUE ~ FALSE)) |>
    filter(first_ref_rec_date_flag == TRUE) |>
    mutate(across(c(ref_rej_date, ref_rej_reason, ref_rej_act),
                  ~ if_else(ref_acc == "01" & aaa_pt_dup_flag == TRUE & row_number() == 1 , NA, .))) |>
    select(-first_ref_rec_date_flag, -ref_rec_count, -aaa_pt_dup_flag)
  
  message('NHS Ayrshire and Arran referrals re-ordered\n')
  return(clean_refs_ayr_and_arran)
  
}




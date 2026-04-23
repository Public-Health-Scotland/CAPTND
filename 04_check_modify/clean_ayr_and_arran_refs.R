#########################################################
## NHS Ayrshire and Arran Referral Preference function ##
#########################################################

#Function has been developed to preferentially use the first accepted referral record
#in a pathway where more than one referral record has been submitted which all have different
#ref_rec_dates.
#NHS Ayrshire and Arran asked that the first accepted referral be used as the valid clock start 
#for each pathway if present.
#Precedence was set as earliest ref_acc == 01, earliest rref_acc == 03 and then earliest ref_acc == 02.
#Every opportunity is therefore given to keep the pathway open and active in CAPTND.

#Author: Luke Taylor
#Date: 22/04/2026

ayr_and_arran_ref_fix <- function(df){
  
  identify_dup_aaa_refs <- df |>
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o), !!sym(ucpn_o), !!sym(chi_o)) |>
    mutate(ref_rec_count = sum(!is.na(!!sym(ref_rec_date_o))),
           aaa_pt_dup_flag = !!sym(hb_name_o) == "NHS Ayrshire and Arran" & !!sym(dataset_type_o) == "PT" &
             ref_rec_count >= 2) |>
    ungroup() 
  
  clean_refs_ayr_and_arran <- identify_dup_aaa_refs |>
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o), !!sym(ucpn_o), !!sym(chi_o)) |>
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
    select(-first_ref_rec_date_flag, -ref_rec_count, -aaa_pt_dup_flag)
  
  message('NHS Ayrshire and Arran referrals re-ordered\n')
  return(clean_refs_ayr_and_arran)
  
}




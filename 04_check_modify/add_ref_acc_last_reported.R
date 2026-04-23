#####################################
## Referral Last Accepted Function ##
#####################################

#Function had to be developed further to deal with instances of referral records with
#the same ref_rec_date submitted in the same month with different referral
#acceptance statuses.
#Where we have an accepted and rejected record, and the rejection date is the same day 
#as the referral received date, precedence is given to the accepted referral.
#If the referral rejection date is after the ref_rec_date, the referral is 
#considered rejected.

#Author: Luke Taylor
#Date: 20/04/2026

referral_last_accepted <- function(df){

  #identify duplicate referrals
  df_dup_refs <- df |>
    #count number of referrals with the same header date and ref_rec_date
    group_by(!!!syms(data_keys), !!sym(header_date_o), !!sym(ref_rec_date_o)) |>
    mutate(n_dup_refs = sum(!is.na(!!sym(ref_date_o)) | !is.na(!!sym(ref_rec_date_o)))) |> ungroup() |>
    #count the number of referrals with the same referral acceptance status
    group_by(!!!syms(data_keys), !!sym(ref_acc_o)) |>
    mutate(n_ref_acc = sum(!is.na(ref_date) | !is.na(ref_rec_date))) |> ungroup() |>
    #identify referral records where the referral rejection date does not equal the ref_rec_date
    group_by(!!!syms(data_keys)) |>
    mutate(rej_date_check = case_when(any(!!sym(ref_rej_date_o) != !!sym(ref_rec_date_o)) ~ 1,
                                      TRUE ~ 0)) |>
    mutate(dup_ref_to_be_reordered = case_when(any(n_dup_refs >= 2) & any(n_ref_acc == 1)
                                               & rej_date_check == 0 ~ "dup_ref_reorder",
                                               any(n_dup_refs >= 2) & any(n_ref_acc == 1)
                                               & rej_date_check == 1 ~ "dup_ref_use_rej_date",
                                               TRUE ~ NA_character_)) |> ungroup()
  
  #arrange and apply latest acceptance status
  df_accept_last_rep <- df_dup_refs |>
    group_by(!!!syms(data_keys)) |>
    arrange(!!sym(ucpn_o), case_when(
      dup_ref_to_be_reordered == "dup_ref_reorder" ~ match(ref_acc, c(3, 2, 1)),
      dup_ref_to_be_reordered == "dup_ref_use_rej_date" ~ match(ref_acc, c(3, 1, 2)),
      TRUE ~ NA_integer_), !!sym(ref_rec_date_o),.by_group = TRUE) |>
    mutate(ref_acc_last_reported :=last(!!sym(ref_acc_o),order_by = !!sym(header_date_o),na_rm = TRUE)) |>
    ungroup()
  
  message('Referral acceptance last reported added\n')
  return(df_accept_last_rep)

}
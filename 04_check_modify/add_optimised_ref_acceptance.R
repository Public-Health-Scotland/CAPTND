################################################.
### Get optimised referral acceptance status ###
################################################.


#Author: Bex Madden
#Date: 22/01/2025

#For pathways where a referral has been left pending or acceptance status has not been entered
# but an appointment has occurred, this function applies a status of 'accepted' in the opti column

add_optimised_ref_acceptance <- function(df){

df_completed <- df |> 
  mutate(ref_acc_opti = case_when(!!sym(ref_acc_last_reported_o) == 3 & has_any_app_date == TRUE ~ 1,
                                  is.na(!!sym(ref_acc_last_reported_o)) & has_any_app_date == TRUE ~ 1,
                                  TRUE ~ !!sym(ref_acc_last_reported_o)),
         .after=!!sym(ref_acc_last_reported_o))

# save out affected pathways
ref_acc_changed <- df_completed |> 
  filter((ref_acc_last_reported == 3 & ref_acc_opti == 1) |
           is.na(ref_acc_last_reported) & ref_acc_opti == 1) |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  save_as_parquet(paste0(stats_checked_dir, "/ref_acc_status_changed"))

return(df_completed)
}
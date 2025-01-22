################################################.
### Get optimised referral acceptance status ###
################################################.


#Author: Bex Madden
#Date: 22/01/2025


add_optimised_ref_acceptance <- function(df){

df_completed <- df |> 
  mutate(ref_acc_opti = case_when(!!sym(ref_acc_last_reported_o) == 3 & has_any_app_date == TRUE ~ 1,
                                  TRUE ~ !!sym(ref_acc_last_reported_o)),
         .after=!!sym(ref_acc_last_reported_o))

ref_acc_changed <- df_completed |> 
  filter(ref_acc_last_reported == 3 & ref_acc_opti == 1) |> 
  write_csv(paste0(stats_checked_dir, "/ref_acc_status_changed.csv"))

return(df_completed)
}
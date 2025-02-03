#################################################################.
### Report pathways with appointments after referral rejected ###
#################################################################.

# Author: Bex Madden
# Date: 3/2/2025

report_apps_after_ref_rej <- function(df){
  
df_ref_rej_apps <- df |>
  group_by(!!!syms(data_keys)) |>
  filter(any(ref_acc_opti == "2") & any(!is.na(app_date))) |> 
  arrange(ucpn, app_date) |> 
  lazy_dt() |> 
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  
  save_as_parquet(paste0(stats_checked_dir, "/appointment_after_ref_rejected"))
                                    
}

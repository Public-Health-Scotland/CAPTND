################################################.
### Report pathways missing referral records ###
################################################.

#Author: Bex Madden
#Date: 30/01/2025

# Report the data keys of pathways which are missing a referral record, for feedback to boards

report_missing_referrals <- function(df){

  latest_date <- floor_date(max(df$header_date), 'month')
  
df_missing_ref <- df |> 
  group_by(!!!syms(data_keys)) |> 
  filter(any(!is.na(app_date) | 
               !is.na(case_closed_date) | 
               !is.na(treat_start_date)) & # should catch pathways which DO have appointment, discharge, or diagnosis records
           is.na(ref_date)) |>    # but DON'T have referral info (filled down)
  
  mutate(n_app_rows = length(na.omit(app_date)),
         n_dis_rows = length(na.omit(case_closed_date)),
         n_diag_rows = length(na.omit(treat_start_date))) |> # reports number of each record type present for that pathway
  
  arrange(ucpn, header_date) |> # shows most recent activity month dplyr::desc(
  select(header_date, hb_name, dataset_type, ucpn, 
         patient_id, n_app_rows, n_dis_rows, n_diag_rows) |> 
  slice(1) |> 

  save_as_parquet(paste0(stats_checked_dir, "/pathways_missing_referrals"))

message(paste0('Data on pathways missing referral info found in ', stats_checked_dir))

}

# had the thought that if you return(plot) it would pop up and remind whoever's runnign control_swift to check source data
# latest_month_plot <- df_missing_ref |> 
#   mutate(header_month = floor_date(header_date, 'month')) |> 
#   lazy_dt() |> 
#   group_by(hb_name, dataset_type, header_month) |> 
#   summarise(n = n()) |> 
#   ungroup() |> 
#   as.data.frame() |> 
#   filter(header_month == latest_date)
# 
# max = max(latest_month_plot$n)+100
#   
#   plot <- ggplot(latest_month_plot, aes(x = hb_name, y = n, group = dataset_type, fill = dataset_type)) +
#     geom_bar(stat = 'identity', position = 'dodge') +
#     coord_flip() +
#     theme_minimal() +
#     scale_y_sqrt(limits = c(0,max), breaks = seq(0,max, by=500)) +
#     labs(caption = "Pathways with missing referral information \n that had activity in the latest month")
# 
#   rm(latest_month_plot)
#   
#   return(plot)

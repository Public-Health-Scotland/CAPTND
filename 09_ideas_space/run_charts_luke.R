
library(NHSRplotthedots)


pat_seen_df <- read_parquet(paste0(shorewise_pub_data_dir, "/patients_seen/pat_seen_unadj_wait_grp_mth.parquet"))

rtt_runchart <- function(ds_type, hb){
  
  pat_seen <- pat_seen_df |>
    filter(dataset_type == ds_type,
           hb_name == hb,
           unadj_rtt_group == '0 to 18 weeks')
  
  pat_seen |>
    ptd_spc(
      value_field = perc,
      date_field = first_treat_month,
      improvement_direction = "increase"
    ) |>
    ptd_create_ggplot(
      y_axis_label = "% patients seen within 18 weeks",
      x_axis_label = 'Month of first treatment',
      x_axis_date_format = "%m/%Y",
      main_title = paste0("Proportion of patients starting treatment within 18 weeks of referral to ", ds_type ," services in ", hb)
    )
  
    
}

rtt_df <- rtt_runchart('PT', 'NHS Greater Glasgow and Clyde')






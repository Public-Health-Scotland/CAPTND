#######################################################################.
#### Create Table: total appointments raw vs optimised comparison  ####.
#######################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_table_opti_raw_appt_comp <- function(df){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  df_raw <- read_parquet(paste0(root_dir, "/swift_extract.parquet"))
  
  df_raw_app <- df_raw |>
    remove_borders_int_refs() |>
    mutate(app_month = floor_date(!!sym(app_date_o), "month"),
           app_quarter_ending = make_date(lubridate::year(app_month), 
                                          lubridate::quarter(app_month) * 3, 1)) |>
    select(!!dataset_type_o, !!hb_name_o, !!ucpn_o, !!chi_o, !!app_date_o, !!app_month_o, 
           !!att_status_o, !!att_cat_o, !!app_purpose_o, !!ref_acc_o, !!app_date_o, app_quarter_ending) |> 
    filter(!is.na(!!sym(app_date_o))) 
  
  df_raw_app <- df_raw_app |>
    filter(as.Date(!!sym(app_month_o)) %in% as.Date(date_range)) |>
    mutate(Attendance = fcase(
      !!sym(att_status_o) == "1", "Attended",
      !!sym(att_status_o) == "2", "Clinic cancelled",
      !!sym(att_status_o) == "3", "Patient cancelled",
      !!sym(att_status_o) == "5", "Patient CNW",
      !!sym(att_status_o) == "8", "Patient DNA",
      !!sym(att_status_o) == "9", "Patient died",
      !!sym(att_status_o) == "99", "Not known",
      default = "Not recorded"))
  
  # get total apps for each time period
  #quarterly
  df_raw_app_qt <- df_raw_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
    summarise(total_apps_raw = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "tot_raw_qt_appt_df"))
  
  #monthly
  df_raw_app_mth <- df_raw_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_month) |>  
    summarise(total_apps_raw = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "tot_raw_mth_appt_df"))
  
  #combine opti and raw appt figures
  df_appt_combined <- df |>
    left_join(df_raw_app_qt, by = c("hb_name", "app_quarter_ending", "dataset_type")) |>
    select(-app_quarter_ending) |>
    group_by(dataset_type, hb_name) |>
    #filter(app_quarter_ending == max(app_quarter_ending)) |>
    mutate(total_apps = sum(total_apps),
           total_apps_raw = sum(total_apps_raw),
           num_diff = total_apps_raw - total_apps,
           per_diff = sprintf("%.1f%%", num_diff/total_apps_raw*100, 1),
           across(total_apps:num_diff, ~prettyNum(., big.mark = ","))) |>
    distinct() |>
    ungroup() |>
    #right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |>  
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    select(dataset_type, hb_name, total_apps_raw, total_apps, num_diff, per_diff) |>
    
    rename(`Health board` = !!sym(hb_name_o),
           `Appointment count (in optimised dataset)` = total_apps,
           `Appointment count (submitted)` = total_apps_raw,
           `Numeric difference` = num_diff,
           `Percent difference` = per_diff) |>
    filter(!is.na(`Health board`))  # remove empty nhs 24 row
  
  df_appt_combined[is.na(df_appt_combined)] <- ".." # make NAs '..'
  df_appt_combined[df_appt_combined == "0"] <- "-" # make 0 '-'
  df_appt_combined[df_appt_combined == "0.0%"] <- "-" # make 0% '-'
  
  save_as_parquet(df_appt_combined, paste0(shorewise_pub_data_dir, "/appointments_att/table_tot_appt_comp_latest_pub_period")) 
    
    
}



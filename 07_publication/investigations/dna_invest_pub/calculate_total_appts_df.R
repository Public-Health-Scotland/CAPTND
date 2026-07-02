################################################################.
#### DNA Focused Publication - Total Appointments Dataframe ####.
################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

total_appt_dataframe <- function(df){

  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  # get appointments df
  df_app <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    remove_borders_int_refs() |>
    
    mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
           app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(app_quarter, unit = "month"))
  
  # demographic df
  demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group", 
                    "location", "prof_group", "ethnicity_last_reported", "ur8_2022_name")
  
  # appt df with key variables
  df_app <- df_app |>
    select(all_of(data_keys), !!app_date_o, !!app_month_o, !!att_status_o, !!att_cat_o, !!app_purpose_o,
           all_of(demographics), !!ref_acc_o, !!app_date_o, app_quarter_ending) |> 
    filter(!is.na(!!sym(app_date_o))) 
  
  df_app_att <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |>
    mutate(Attendance = fcase(
      !!sym(att_status_o) == "1", "Attended",
      !!sym(att_status_o) == "2", "Clinic cancelled",
      !!sym(att_status_o) == "3", "Patient cancelled",
      !!sym(att_status_o) == "5", "Patient CNW",
      !!sym(att_status_o) == "8", "Patient DNA",
      !!sym(att_status_o) == "9", "Patient died",
      !!sym(att_status_o) == "99", "Not known",
      default = "Not recorded"),
      !!sym(age_group_o) := as.character(!!sym(age_group_o))) |>
    add_sex_description() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "tot_appts_df"))
  
  # get total apps for each time period
  #quarterly
  df_tot_app_qt <- df_app_att |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "tot_quarterly_appt_df"))
  
  #monthly
  df_tot_app_mth <- df_app_att |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_month) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "tot_monthly_appt_df"))
  
}
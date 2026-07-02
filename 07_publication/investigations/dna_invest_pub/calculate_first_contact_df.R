###############################################################.
#### DNA Focused Publication - First Contact DNA Dataframe ####.
###############################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_dataframe <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
  # get appointments df
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    remove_borders_int_refs() |>
    
    mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
           app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(app_quarter, unit = "month"))
  
  demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group", 
                    "location", "prof_group", "ethnicity_last_reported", "ur8_2022_name")
  
  df_app <- df |>
    select(all_of(data_keys), ref_rec_date_opti, !!app_date_o, !!app_month_o, !!att_status_o, !!att_cat_o, !!app_purpose_o,
           all_of(demographics), !!ref_acc_o, !!app_date_o, app_quarter_ending) |> 
    filter(!is.na(!!sym(app_date_o)))  
  
  
  # get first contact appointment date per pathway only
  df_first_app <- df_app |>
    arrange(!!!syms(data_keys), !!sym(app_date_o)) |> 
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    slice(1)|> 
    ungroup() |> 
    as.data.frame() |> 
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
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "all_first_appts_df"))
  
  # get total apps for each time period
  #quarterly
  df_tot_app_qt <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "tot_quarterly_appt_df"))
  
  #monthly
  df_tot_app_mth <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o)) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), !!sym(app_month_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))|> 
    ungroup() |>
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "tot_monthly_appt_df"))

}


##########################################################################.
#### DNA Focused Publication - First Contact DNAs by Quarter and Wait ####.
##########################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_quarter_wait <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
  
  # by hb, quarter, wait for appt
  df_wait_time_first_appt <- df |>
    mutate(wait_for_first_appt = as.numeric(app_date - ref_rec_date_opti),
           wait_cat = case_when(wait_for_first_appt <= 126 ~ "0 to 18 weeks",
                                wait_for_first_appt > 126 & wait_for_first_appt <= 245 ~ "19 to 35 weeks",
                                wait_for_first_appt > 245 & wait_for_first_appt <= 364 ~ "36 to 52 weeks",
                                wait_for_first_appt > 364 ~ "Over 52 weeks")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, wait_cat) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, wait_cat) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, wait_cat) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, wait_cat) |>  
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_wait"))
  
}




###############################################################################.
#### DNA Focused Publication - First Contact DNAs by Quarter and Age Group ####.
###############################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_quarter_age <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
  # by hb, quarter, and age group - for presenting in supplement
  first_att_qt_age <- df |>
    mutate(!!sym(age_group_o) := as.character(!!sym(age_group_o))) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
             app_quarter_ending, !!sym(age_group_o)) |> 
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, !!sym(age_group_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, 
             !!sym(age_group_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_month = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            readr::parse_number(!!sym(age_group_o))) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_age"))
  
}




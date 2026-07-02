#################################################################.
#### DNA Focused Publication - First Contact DNAs by Quarter ####.
#################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_quarter <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names

  # by hb and quarter
  first_att_qt <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |> 
    mutate(first_contact = sum(firstcon_att), 
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1),
           !!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d")) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    ungroup() |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb"))

}

###########################################################################.
#### DNA Focused Publication - Total DNAs by Quarter and Appt Location ####.
###########################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

total_appts_quarter_loc <- function(df){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  # by hb, quarter, and location - for presenting in supplement
  # lookup codes for care contact location
  loc_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Location") %>% 
    rename(location = Code,
           loc_label = Location) |>
    mutate(loc_label = str_to_sentence(loc_label),
           loc_label = case_when(loc_label == 'Attend anywhere e.g. skype call,  ecbt' ~ 'Attend anywhere',
                                 loc_label == 'Public place e.g. street, third sector' ~ 'Public place',
                                 loc_label == 'Nhs near me' ~ 'NHS Near Me',
                                 TRUE ~ loc_label)) 
  
  df_app_att <- df |> 
    left_join(loc_lookup, by = "location") 
  
  
  df_app_qt_loc <- df_app_att |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             loc_label) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, loc_label) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, loc_label) |> 
    mutate(total_loc = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_loc*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_loc"))
  
  
  df_app_qt_loc_simd <- df_app_att |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             loc_label, simd2020_quintile) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, loc_label, simd2020_quintile) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, loc_label, simd2020_quintile) |> 
    mutate(total_loc = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_loc*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_loc_simd"))
  
}




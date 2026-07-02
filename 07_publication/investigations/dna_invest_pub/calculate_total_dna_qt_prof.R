########################################################################.
#### DNA Focused Publication - Total DNAs by Quarter and Prof Group ####.
########################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

total_appts_quarter_prof <- function(df){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  # by hb, quarter, and professional group - for presenting in supplement
  # lookup codes for professional group
  prof_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Pro_Group") %>% 
    rename(prof_group = Code,
           prof_label = Pro_Group) |>
    mutate(prof_label = str_to_sentence(prof_label),
           prof_label = case_when(prof_label == 'Cbt therapist' ~ 'CBT therapist',
                                  TRUE ~ prof_label)) 
  
  df_app_att <- df |> 
    left_join(prof_lookup, by = "prof_group") 
  
  
  df_app_qt_prof <- df_app_att |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             prof_label) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, prof_label) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, prof_label) |> 
    mutate(total_prof = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_prof*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_prof"))
  
}




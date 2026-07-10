#################################################################.
#### DNA Focused Publication - Total DNAs by Quarter and Age ####.
#################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

total_appt_quarter_agg_age <- function(df){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  # by hb, quarter, agg age and sex - for presenting in supplement
  updated_age_groups_df <- df |>
    mutate(agg_age_groups = case_when(#PT age groups
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) <= 24 ~ 'Under 25',
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 25 & !!sym(age_at_ref_rec_o) <= 39 ~ '25-39',
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 40 & !!sym(age_at_ref_rec_o) <= 64 ~ '40-64',
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 65 ~ '65 plus',
      #CAMHS age groups
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) < 6 ~ 'Under 6',
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) >= 6 & !!sym(age_at_ref_rec_o) <= 11 ~ '6-11',
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) >= 12 & !!sym(age_at_ref_rec_o) <= 15 ~ '12-15',
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) > 15 ~ 'Over 15',
      #NAs with invalid CHI
      is.na(!!sym(age_at_ref_rec_o)) ~ 'Data missing'))
  
  # by hb, quarter, and age group - for presenting in supplement
  df_app_qt_agg_age_group <- updated_age_groups_df |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance, agg_age_groups) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance, agg_age_groups) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, agg_age_groups) |> 
    mutate(total_age = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_age*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o) |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_agg_age_group"))
  
  # by hb, month, and age group - for presenting in supplement
  df_app_mth_agg_age_group <- updated_age_groups_df |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_month, Attendance, agg_age_groups) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_month, Attendance, agg_age_groups) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_month, agg_age_groups) |> 
    mutate(total_age = sum(apps_att)) |> 
    ungroup() |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_age*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o) |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_agg_age_group"))
  
}




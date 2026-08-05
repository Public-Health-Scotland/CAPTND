#########################################################################################.
#### DNA Focused Publication - First Contact DNAs by Quarter, Aggregated Age and Sex ####.
#########################################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_quarter_age_sex <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
  #skeleton dataframes
  att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                             "Patient CNW", "Not known", "Not recorded"))
  
  sex_df <- data.frame(sex_reported = c("Male", "Female", "Not known", "Data missing"))
  
  camhs_df <- data.frame(agg_age_groups = c("Under 6", "6-11", "12-15", "Over 15", "Data missing"))
  
  pt_df <- data.frame(agg_age_groups = c("Under 25", "25-39", "40-64", "65 plus", "Data missing"))
  
  agg_age_grps_df <- bind_rows(camhs_df %>% mutate(dataset_type = "CAMHS"),
                               pt_df %>% mutate(dataset_type = "PT"))
  
  qt_df <- df |>
    select(app_quarter_ending) |> distinct()
  
  #complete skeleton df
  df_age_sex_mth_hb <- df_ds_hb_name |>
    cross_join(att_status_df) |>
    cross_join(qt_df) |>
    cross_join(sex_df) |>
    left_join(agg_age_grps_df, by = c("dataset_type")) |>
    filter(hb_name != 'NHS Scotland')
  
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
  
  df_age_sex_first_appt <- updated_age_groups_df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             agg_age_groups, !!sym(sex_reported_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    mutate(sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                         TRUE ~ sex_reported)) |>
    right_join(df_age_sex_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status", "sex_reported", 
                                      "agg_age_groups", "app_quarter_ending")) |>
    mutate(firstcon_att = case_when(is.na(firstcon_att) ~ 0,
                                    TRUE ~ firstcon_att)) |>
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, agg_age_groups, 
             !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, agg_age_groups, 
             !!sym(sex_reported_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            !!sex_reported_o, agg_age_groups) |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_age_sex"))
  
}



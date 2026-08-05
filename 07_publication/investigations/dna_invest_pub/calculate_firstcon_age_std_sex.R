##############################################################################.
#### DNA Focused Publication - Age Standardised First Contact DNAs by Sex ####.
##############################################################################.

# Author: Luke Taylor
# Date: 2026-07-28

age_std_firstcon_appt_dna_sex <- function(df, firstcon_std_pop){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
  #skeleton dataframes
  att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                             "Patient CNW", "Not known", "Not recorded"))
  
  sex_df <- data.frame(sex_reported = c("Male", "Female"))
  
  camhs_df <- data.frame(age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "Data missing"))
  
  pt_df <- data.frame(age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                    "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+", "Data missing"))
  
  agg_age_grps_df <- bind_rows(camhs_df %>% mutate(dataset_type = "CAMHS"),
                               pt_df %>% mutate(dataset_type = "PT"))
  
  #complete skeleton df
  df_age_sex_mth_hb <- df_ds_hb_name |>
    cross_join(att_status_df) |>
    cross_join(sex_df) |>
    left_join(agg_age_grps_df, by = "dataset_type")
  
  #update agg age groups
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
  
  #dna rate by age, sex
  firstcon_mth_age_sex <- updated_age_groups_df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance,
             !!sym(sex_reported_o), !!sym(age_group_o)) |>  
    summarise(tot_dnas = n(), .groups = "drop") |> 
    mutate(sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported),
           age_group = case_when(is.na(age_group) ~ 'Data missing',
                                 TRUE ~ age_group)) |>
    right_join(df_age_sex_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                         "sex_reported", "age_group")) |>
    mutate(tot_dnas = case_when(is.na(tot_dnas) ~ 0,
                                TRUE ~ tot_dnas)) |>
    group_by(!!sym(dataset_type_o), Attendance, !!sym(sex_reported_o), !!sym(age_group_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(sex_reported_o), !!sym(age_group_o)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o), !!sym(age_group_o)) |> 
    mutate(tot_appts_by_group = sum(tot_dnas),
           dna_rate = tot_dnas/tot_appts_by_group*100,
           dna_rate = case_when(tot_dnas == 0 & tot_appts_by_group == 0 ~ 0,
                                TRUE ~ dna_rate)) |> 
    ungroup() |>  
    filter(hb_name == 'NHS Scotland',
           Attendance == 'Patient DNA') 
  
  age_std_tot_firstcon_age_sex <- firstcon_mth_age_sex |>
    left_join(firstcon_std_pop, by = c("dataset_type", "hb_name", "age_group")) |>
    mutate(st_dna_rate = dna_rate*weight) |>
    group_by(dataset_type, sex_reported) |>
    summarise(std_rate_by_sex = round(sum(st_dna_rate, na.rm = TRUE), 1),
              .groups = "drop") |>
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_age_std_sex"))
  
}




###############################################################################.
#### DNA Focused Publication - Age Standardised Total DNAs by SIMD and Sex ####.
###############################################################################.

# Author: Luke Taylor
# Date: 2026-06-30

age_std_tot_appt_dna_simd_sex <- function(df, total_std_pop){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  #skeleton dataframes
  simd_df <- data.frame(simd2020_quintile = c(1,2,3,4,5))
  
  att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                             "Patient CNW", "Not known", "Not recorded"))
  sex_df <- data.frame(sex_reported = c("Male", "Female"))
  
  agg_age_grps_df <- data.frame(agg_age_groups = c("Under 6", "6-11", "12-15", "Over 15",
                                                   "Under 25", "25-39", "40-64", "65 plus"),
                                dataset_type = c("CAMHS", "CAMHS", "CAMHS", "CAMHS",
                                                 "PT", "PT", "PT", "PT"))
  #complete skeleton df
  df_simd_mth_hb <- df_ds_hb_name |>
    cross_join(simd_df) |>
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
  
  #dna rate by simd, age, sex
  tot_mth_simd_age_sex <- updated_age_groups_df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, simd2020_quintile,
             sex_reported, agg_age_groups) |>  
    summarise(tot_dnas = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), Attendance, simd2020_quintile,
             sex_reported, agg_age_groups) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    right_join(df_simd_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                      "simd2020_quintile", "sex_reported", "agg_age_groups")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, simd2020_quintile,
            sex_reported, agg_age_groups) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), simd2020_quintile,
             sex_reported, agg_age_groups) |> 
    mutate(tot_dnas = case_when(is.na(tot_dnas) ~ 0,
                                TRUE ~ tot_dnas),
           tot_appts_by_group = sum(tot_dnas),
           dna_rate = round(tot_dnas/tot_appts_by_group*100, 1)) |> 
    ungroup() |>  
    filter(hb_name == 'NHS Scotland',
           Attendance == 'Patient DNA') 
  
  age_std_tot_dna_simd_sex <- tot_mth_simd_age_sex |>
    left_join(total_std_pop, by = c("dataset_type", "hb_name", "agg_age_groups")) |>
    mutate(st_dna_rate = round(dna_rate*weight, 1)) |>
    group_by(dataset_type, simd2020_quintile, sex_reported) |>
    mutate(std_rate_by_simd_sex = sum(st_dna_rate)) |>
    select(dataset_type, simd2020_quintile, sex_reported, std_rate_by_simd_sex) |>
    distinct() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_age_std_simd_sex"))
  
}




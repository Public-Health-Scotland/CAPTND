###########################################################################################.
#### DNA Focused Publication - Age Standardised Total DNAs by Location and Sex ####.
###########################################################################################.

# Author: Luke Taylor
# Date: 2026-06-30

age_std_tot_appt_dna_loc_sex <- function(df, total_std_pop){
  
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  #skeleton dataframes
  att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                             "Patient CNW", "Not known", "Not recorded"))
  sex_df <- data.frame(sex_reported = c("Male", "Female"))
  
  camhs_df <- data.frame(age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29"))
  
  pt_df <- data.frame(age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                    "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"))
  
  agg_age_grps_df <- bind_rows(camhs_df %>% mutate(dataset_type = "CAMHS"),
                               pt_df %>% mutate(dataset_type = "PT"))
  
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
  
  #complete skeleton df
  loc_df <- loc_lookup |>
    select(loc_label)
  
  df_simd_mth_hb <- df_ds_hb_name |>
    cross_join(att_status_df) |>
    cross_join(sex_df) |>
    cross_join(loc_df) |>
    left_join(agg_age_grps_df, by = "dataset_type")
  
  df_app_att <- df |> 
    left_join(loc_lookup, by = "location") 
  
  
  tot_att_mth_loc_age_sex <- df_app_att |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, loc_label,
             !!sym(sex_reported_o), !!sym(age_group_o)) |>  
    summarise(tot_dnas = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), Attendance, loc_label,
             !!sym(sex_reported_o), !!sym(age_group_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    right_join(df_simd_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status", 
                                      "loc_label", "sex_reported", "age_group")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, loc_label,
            !!sym(sex_reported_o), !!sym(age_group_o)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), loc_label,
             !!sym(sex_reported_o), !!sym(age_group_o)) |> 
    mutate(tot_dnas = case_when(is.na(tot_dnas) ~ 0,
                                TRUE ~ tot_dnas),
           tot_appts_by_group = sum(tot_dnas),
           dna_rate = round(tot_dnas/tot_appts_by_group*100, 1),
           dna_rate = case_when(tot_dnas == 0 & tot_appts_by_group == 0 ~ 0,
                                TRUE ~ dna_rate)) |> 
    ungroup() |>  
    filter(hb_name == 'NHS Scotland',
           Attendance == 'Patient DNA') 
  
  age_std_tot_dna_loc_sex <- tot_att_mth_loc_age_sex |>
    left_join(total_std_pop, by = c("dataset_type", "hb_name", "age_group")) |>
    mutate(st_dna_rate = round(dna_rate*weight, 1)) |>
    group_by(dataset_type, loc_label, sex_reported) |>
    mutate(std_rate_by_loc_sex = sum(st_dna_rate)) |>
    select(dataset_type, loc_label, sex_reported, std_rate_by_loc_sex) |>
    distinct() |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_age_std_loc_sex"))
  
}


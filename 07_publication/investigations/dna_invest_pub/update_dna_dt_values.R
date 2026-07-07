####################################.
### Update DNA data table values ###
####################################.

# Author: Luke Taylor
# Date: 2026-07-03

update_dna_dt_values <- function(wb){
  
  # replace CAMHS in lookup with PT
  if(dataset_choice == "PT"){
    writeData(wb, sheet = "Lookups", 
              x = "PT",  
              startCol = 2, startRow = 2, #headerStyle = style_text, 
              colNames = FALSE)
  }
  
  
  #Tab 1  
  df_tot_dna_sex <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_sex, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, sex_reported) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), sex_reported) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(apps_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported)) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 1 Data", 
            x = df_tot_dna_sex, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 1", style = style_count, cols = 3, rows = 11:13, stack = TRUE)
  addStyle(wb, sheet = "Tab 1", style = createStyle(halign = "right"), cols = 4, rows = 11:13, stack = TRUE)
  addStyle(wb, sheet = "Tab 1", style = createStyle(halign = "right"), cols = 5, rows = 11:13, stack = TRUE)
  
  #Tab 2  
  df_tot_dna_age <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_agg_age_group.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_age, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, agg_age_groups) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), agg_age_groups) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(apps_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           agg_age_groups = case_when(is.na(agg_age_groups) ~ 'Data missing',
                                      TRUE ~ agg_age_groups)) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 2 Data", 
            x = df_tot_dna_age, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_count, cols = 3, rows = 11:15, stack = TRUE)
  addStyle(wb, sheet = "Tab 2", style = createStyle(halign = "right"), cols = 4, rows = 11:15, stack = TRUE) 
  addStyle(wb, sheet = "Tab 2", style = createStyle(halign = "right"), cols = 5, rows = 11:15, stack = TRUE)
  
  df_age_groups <- data.frame(ds = c('CAMHS', 'PT'),
                              age_groups = c('Under 6', 'Under 25', '6-11', '25-39',
                                             '12-15', '40-64', 'Over 15', '65 plus'))
  
  df_age_groups <- df_age_groups |> filter(ds == dataset_choice) |> select(age_groups)
  
  writeData(wb, sheet = "Tab 2", 
            x = df_age_groups,  
            startCol = 2, startRow = 11, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_text, cols = 2, rows = 11:14, stack = TRUE)
  
  #Tab 3
  df_tot_dna_simd <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_simd.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_simd, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(simd_quintile_o)) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(apps_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Data missing',
                                         TRUE ~ simd2020_quintile)) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 3 Data", 
            x = df_tot_dna_simd, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 3", style = style_count, cols = 3, rows = 11:16, stack = TRUE)
  addStyle(wb, sheet = "Tab 3", style = createStyle(halign = "right"), cols = 4, rows = 11:16, stack = TRUE) 
  addStyle(wb, sheet = "Tab 3", style = createStyle(halign = "right"), cols = 5, rows = 11:16, stack = TRUE)
  
  #Tab 4
  df_tot_dna_simd_sex <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_simd_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_simd, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(simd_quintile_o),
             !!sym(sex_reported_o)) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o), !!sym(sex_reported_o)) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    mutate(nhs_scot_tot_dnas = sum(apps_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Data missing',
                                         TRUE ~ simd2020_quintile),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported))
  
  age_std_simd_sex <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_age_std_simd_sex.parquet")) |>
    mutate(simd2020_quintile = as.character(simd2020_quintile))
  
  df_tot_dna_simd_sex <- df_tot_dna_simd_sex |>
    left_join(age_std_simd_sex, by = c("dataset_type", "simd2020_quintile", "sex_reported")) |>
    filter(dataset_type == dataset_choice) 
  
  writeData(wb, sheet = "Tab 4 Data", 
            x = df_tot_dna_simd_sex,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 4", style = style_count, cols = 3, rows = 12:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = style_count, cols = 4, rows = 12:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = createStyle(halign = "right"), cols = 5, rows = 12:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = createStyle(halign = "right"), cols = 6, rows = 12:17, stack = TRUE)
  
  addStyle(wb, sheet = "Tab 4", style = style_count, cols = 3, rows = 24:29, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = style_count, cols = 4, rows = 24:29, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = createStyle(halign = "right"), cols = 5, rows = 24:29, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = createStyle(halign = "right"), cols = 6, rows = 24:29, stack = TRUE)
  
  #Tab 5
  df_tot_dna_ur_sex <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_ur_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_ur, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, ur8_2022_name,
             !!sym(sex_reported_o)) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ur8_2022_name, !!sym(sex_reported_o)) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    mutate(nhs_scot_tot_dnas = sum(apps_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           ur8_2022_name = case_when(is.na(ur8_2022_name) ~ 'Data missing',
                                     TRUE ~ ur8_2022_name),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported))
  
  age_std_ur_sex <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_age_std_ur_sex.parquet")) 
  
  df_tot_dna_ur_sex <- df_tot_dna_ur_sex |>
    left_join(age_std_ur_sex, by = c("dataset_type", "ur8_2022_name", "sex_reported")) |>
    filter(dataset_type == dataset_choice) 
  
  writeData(wb, sheet = "Tab 5 Data", 
            x = df_tot_dna_ur_sex, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 5", style = style_text, cols = 3, rows = 12:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = style_count, cols = 4, rows = 12:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 5, rows = 12:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 6, rows = 12:20, stack = TRUE)
  
  addStyle(wb, sheet = "Tab 5", style = style_text, cols = 3, rows = 27:35, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = style_count, cols = 4, rows = 27:35, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 5, rows = 27:35, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 6, rows = 27:35, stack = TRUE)
  
  #Tab 6
  df_firstcon_dna_sex <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending, -app_month) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, sex_reported) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), sex_reported) |>
    mutate(tot_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(firstcon_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported)) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 6 Data", 
            x = df_firstcon_dna_sex, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 6", style = style_text, cols = 3, rows = 11:13, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = style_count, cols = 4, rows = 11:13, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = createStyle(halign = "right"), cols = 5, rows = 11:13, stack = TRUE)
  
  #Tab 7
  df_firstcon_dna_age <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_agg_age_group.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_age, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, agg_age_groups) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), agg_age_groups) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(apps_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           agg_age_groups = case_when(is.na(agg_age_groups) ~ 'Data missing',
                                      TRUE ~ agg_age_groups)) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 7 Data", 
            x = df_firstcon_dna_age, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 7", style = style_count, cols = 3, rows = 11:15, stack = TRUE)
  addStyle(wb, sheet = "Tab 7", style = createStyle(halign = "right"), cols = 4, rows = 11:15, stack = TRUE) 
  addStyle(wb, sheet = "Tab 7", style = createStyle(halign = "right"), cols = 5, rows = 11:15, stack = TRUE)
  
  df_age_groups <- data.frame(ds = c('CAMHS', 'PT'),
                              age_groups = c('Under 6', 'Under 25', '6-11', '25-39',
                                             '12-15', '40-64', 'Over 15', '65 plus'))
  
  df_age_groups <- df_age_groups |> filter(ds == dataset_choice) |> select(age_groups)
  
  writeData(wb, sheet = "Tab 7", 
            x = df_age_groups,  
            startCol = 2, startRow = 11, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_text, cols = 2, rows = 11:14, stack = TRUE)
  
  #Tab 8  
  df_firstcon_dna_simd <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_simd.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(simd_quintile_o)) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    mutate(tot_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(firstcon_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Data missing',
                                         TRUE ~ simd2020_quintile)) |>
    filter(dataset_type == dataset_choice) 
  
  
  writeData(wb, sheet = "Tab 8 Data", 
            x = df_firstcon_dna_simd,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 8", style = style_count, cols = 3, rows = 11:16, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = style_count, cols = 4, rows = 11:16, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = createStyle(halign = "right"), cols = 6, rows = 11:16, stack = TRUE)
  
  #Tab 9
  df_firstcon_dna_simd_sex <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_simd_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(simd_quintile_o),
             !!sym(sex_reported_o)) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o), !!sym(sex_reported_o)) |>
    mutate(tot_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    mutate(nhs_scot_tot_dnas = sum(firstcon_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Data missing',
                                         TRUE ~ simd2020_quintile),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported))
  
  age_std_simd_sex <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_age_std_simd_sex.parquet")) |>
    mutate(simd2020_quintile = as.character(simd2020_quintile))
  
  df_firstcon_dna_simd_sex <- df_firstcon_dna_simd_sex |>
    left_join(age_std_simd_sex, by = c("dataset_type", "simd2020_quintile", "sex_reported")) |>
    filter(dataset_type == dataset_choice) 
  
  writeData(wb, sheet = "Tab 9 Data", 
            x = df_firstcon_dna_simd_sex,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 3, rows = 12:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 4, rows = 12:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 5, rows = 12:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 6, rows = 12:17, stack = TRUE)
  
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 3, rows = 24:29, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 4, rows = 24:29, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 5, rows = 24:29, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 6, rows = 24:29, stack = TRUE)
  
  #Tab 10
  df_firstcon_dna_ur_sex <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_ur_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, ur8_2022_name,
             !!sym(sex_reported_o)) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ur8_2022_name, !!sym(sex_reported_o)) |>
    mutate(tot_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    mutate(nhs_scot_tot_dnas = sum(firstcon_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2),
           ur8_2022_name = case_when(is.na(ur8_2022_name) ~ 'Data missing',
                                     TRUE ~ ur8_2022_name),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported))
  
  age_std_ur_sex <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_age_std_ur_sex.parquet")) 
  
  df_firstcon_dna_ur_sex <- df_firstcon_dna_ur_sex |>
    left_join(age_std_ur_sex, by = c("dataset_type", "ur8_2022_name", "sex_reported")) |>
    filter(dataset_type == dataset_choice) 
  
  writeData(wb, sheet = "Tab 10 Data", 
            x = df_firstcon_dna_ur_sex, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 10", style = style_text, cols = 3, rows = 12:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = style_count, cols = 4, rows = 12:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = createStyle(halign = "right"), cols = 5, rows = 12:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = createStyle(halign = "right"), cols = 6, rows = 12:20, stack = TRUE)
  
  addStyle(wb, sheet = "Tab 10", style = style_text, cols = 3, rows = 27:35, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = style_count, cols = 4, rows = 27:35, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = createStyle(halign = "right"), cols = 5, rows = 27:35, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = createStyle(halign = "right"), cols = 6, rows = 27:35, stack = TRUE)
  
  
  #Tab 11
  df_firstcon_dna_wait <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_wait.parquet")) |> 
    ungroup() |> 
    select(-prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, wait_cat) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), wait_cat) |>
    mutate(tot_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(nhs_scot_tot_dnas = sum(firstcon_att),
           nhs_scot_tot_apps = sum(tot_apps),
           nhs_scot_att_rate = round(nhs_scot_tot_dnas/nhs_scot_tot_apps*100,2)) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 11 Data", 
            x = df_firstcon_dna_wait, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 11", style = style_text, cols = 3, rows = 11:15, stack = TRUE)
  addStyle(wb, sheet = "Tab 11", style = style_count, cols = 4, rows = 11:15, stack = TRUE)
  addStyle(wb, sheet = "Tab 11", style = createStyle(halign = "right"), cols = 5, rows = 11:15, stack = TRUE)
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}




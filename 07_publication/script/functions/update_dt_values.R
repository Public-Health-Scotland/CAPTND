
################################.
### Update data table values ###
################################.

# Author: Charlie Smith
# Date: 2024-08-13
# Updated: 2024-12-30

update_dt_values <- function(wb){
  
  # get quarters ending for all dts
  df_quarts <- read_parquet(paste0(ref_dir, "referrals_quarter_hb.parquet")) |> 
    ungroup() |> select(quarter_ending) |> unique() #|> pull()
  
  df_qt_ds_hb <- df_ds_hb_name |> cross_join(df_quarts)
  quarter_range <- df_quarts |> pull()
  
  simd_df <- data.frame(simd2020_quintile = c('1','2', '3', '4','5', 'Data missing'))
  
  df_simd_qt_hb <- df_qt_ds_hb |>
    cross_join(simd_df) |>
    mutate(att_status = 'Patient DNA')
  
  
  # replace CAMHS in lookup with PT
  if(dataset_choice == "PT"){
    writeData(wb, sheet = "Lookups", 
              x = "PT",  
              startCol = 2, startRow = 2, #headerStyle = style_text, 
              colNames = FALSE)
  }
  
  #Tab 1  
  df_ref_sex <- read_parquet(paste0(ref_demo_dir, "referrals_", "quarter_hb_sex.parquet")) |> 
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 1 Data", 
            x = df_ref_sex, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 1", style = style_count, cols = 3, rows = 15:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 1", style = createStyle(halign = "right"), cols = 4, rows = 15:17, stack = TRUE)
  addStyle(wb, sheet = "Tab 1", style = createStyle(halign = "right"), cols = 5, rows = 15:17, stack = TRUE)
  
  #Tab 2  
  df_ref_age <- read_parquet(paste0(ref_demo_dir, "referrals_", "quarter_hb_age.parquet")) |> 
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 2 Data", 
            x = df_ref_age, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_count, cols = 3, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 2", style = createStyle(halign = "right"), cols = 4, rows = 15:19, stack = TRUE) 
  addStyle(wb, sheet = "Tab 2", style = createStyle(halign = "right"), cols = 5, rows = 15:19, stack = TRUE)
  
  df_age_groups <- data.frame(ds = c('CAMHS', 'PT'),
                              age_groups = c('Under 6', 'Under 25', '6-11', '25-39',
                                             '12-15', '40-64', 'Over 15', '65 plus'))
  
  df_age_groups <- df_age_groups |> filter(ds == dataset_choice) |> select(age_groups)
  
  writeData(wb, sheet = "Tab 2", 
            x = df_age_groups,  
            startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_text, cols = 2, rows = 15:18, stack = TRUE)
  
  #Tab 3
  df_ref_simd <- read_parquet(paste0(ref_demo_dir, "referrals_", "quarter_hb_simd.parquet")) |> 
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 3 Data", 
            x = df_ref_simd, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 3", style = style_count, cols = 3, rows = 15:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 3", style = createStyle(halign = "right"), cols = 4, rows = 15:20, stack = TRUE) 
  addStyle(wb, sheet = "Tab 3", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  #Tab 4
  df_acc_status <- read_parquet(paste0(non_acc_dir, "non_acceptance_summary_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending %in% date_range) |> 
    select(-total, -prop) |> 
    pivot_wider(names_from = ref_acc_desc, values_from = count,
                values_fill = 0) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    pivot_longer(cols = 4:7, values_to = "count", names_to = "ref_acc_desc") |> 
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(total = sum(count, na.rm = TRUE),
           prop = round(count / total * 100, 1)) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    filter(!!sym(dataset_type_o) == dataset_choice) 
  
  
  writeData(wb, sheet = "Tab 4 Data", 
            x = df_acc_status,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 4", style = style_count, cols = 3, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = style_count, cols = 4, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 4", style = createStyle(halign = "right"), cols = 5, rows = 15:19, stack = TRUE)
  
  writeData(wb, sheet = "Tab 4", 
            x = df_quarts,  
            startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 4", style = style_date, cols = 2, rows = 15:19, stack = TRUE)
  
  #Tab 5
  df_non_acc_reason <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance_reason/non_acceptance_reason_quarter_hb.parquet")) |> 
    select(-total, -prop) |>
    right_join(df_qt_ds_hb, by = c("dataset_type", "hb_name", "quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           ref_rej_reason_desc = case_when(is.na(ref_rej_reason_desc) ~ 'No data',
                                           TRUE ~ ref_rej_reason_desc),
           count = case_when(is.na(count) ~ 0,
                             TRUE ~ count)) |> 
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    arrange(desc(count), .by_group = TRUE) |>
    mutate(rank = row_number(),
           top5 = case_when(rank >5 ~ "All other non acceptance reasons",
                            TRUE ~ ref_rej_reason_desc)) |>
    ungroup() |>
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
    mutate(count = sum(count)) |>
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(rank >= 1 & rank <= 6) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) %>%
    
    bind_rows(summarise(.,
                        across(count, sum),
                        across(top5, ~"Total"),
                        across(rank, ~ 7),
                        across(prop, ~ 100),
                        .groups = "drop")) |>
    select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), rej_reason = top5, 
           count, rank, total, prop) |>
    mutate(rej_reason = case_when(is.na(rej_reason) ~ 'Missing data',
                                 TRUE ~ rej_reason)) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 5 Data", 
            x = df_non_acc_reason, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 5", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  #Tab 6
  df_non_acc_actions <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance_action/non_acceptance_action_quarter_hb.parquet")) |> 
    select(-total, -prop) |>
    right_join(df_qt_ds_hb, by = c("dataset_type", "hb_name", "quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           ref_rej_act_desc = case_when(is.na(ref_rej_act_desc) ~ 'No data',
                                        TRUE ~ ref_rej_act_desc),
           count = case_when(is.na(count) ~ 0,
                             TRUE ~ count)) |> 
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    arrange(desc(count), .by_group = TRUE) |>
    mutate(rank = row_number(),
           top5 = case_when(rank >5 ~ "All other non acceptance actions",
                            TRUE ~ ref_rej_act_desc)) |>
    ungroup() |>
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
    mutate(count = sum(count)) |>
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(rank >= 1 & rank <= 6) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) %>%
    
    bind_rows(summarise(.,
                        across(count, sum),
                        across(top5, ~"Total"),
                        across(rank, ~ 7),
                        across(prop, ~ 100),
                        .groups = "drop")) |>
    select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), rej_action = top5, 
           count, rank, total, prop) |>
    mutate(rej_action = case_when(is.na(rej_action) ~ 'Missing data',
                                 TRUE ~ rej_action)) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 6 Data", 
            x = df_non_acc_actions, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 6", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  #Tab 7
  df_ref_source <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/ref_source_quarter_hb.parquet")) |> 
    select(-total, -prop) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    arrange(desc(count), .by_group = TRUE) |>
    mutate(rank = row_number(),
           top5 = case_when(rank >5 ~ "All other referral sources",
                            TRUE ~ ref_source_name)) |>
    ungroup() |> 
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
    mutate(count = sum(count)) |>
    group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(rank >= 1 & rank <= 6) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) %>%
    
    bind_rows(summarise(.,
                        across(count, sum),
                        across(top5, ~"Total"),
                        across(rank, ~ 7),
                        across(prop, ~ 100),
                        .groups = "drop")) |>
    select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), ref_source_name = top5, 
           count, rank, total, prop) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 7 Data", 
            x = df_ref_source, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 7", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 7", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 7", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  #Tab 8  
  first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb.parquet")) |> 
    select(-prop_firstcon_att) |> 
    pivot_wider(names_from = Attendance, values_from = firstcon_att, values_fill = 0) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    pivot_longer(cols = 6:12, names_to = "att_status", values_to = "count") |> 
    mutate(prop = round(count / first_contact * 100, 1)) |> 
    select(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, att_status,
           count, first_contact, prop, total_apps) |> 
    filter(!!sym(dataset_type_o) == dataset_choice) 
  
  
  writeData(wb, sheet = "Tab 8 Data", 
            x = first_att_latest,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 8", style = style_count, cols = 3, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = style_count, cols = 4, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = style_count, cols = 5, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = createStyle(halign = "right"), cols = 6, rows = 15:19, stack = TRUE)
  
  writeData(wb, sheet = "Tab 8", 
            x = df_quarts,  
            startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 8", style = style_date, cols = 2, rows = 15:19, stack = TRUE)
  
  #Tab 9
  tot_dnas_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |>
    filter(Attendance == 'Patient DNA') |>
    select(!!sym(dataset_type_o), !!(hb_name_o), app_quarter_ending, dna_count = apps_att, total_apps, 
           dna_rate = prop_apps_att) |>
    mutate(dna_rate = dna_rate/100) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    ungroup() |>
    arrange(!!dataset_type_o, !!hb_name_o) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 9 Data", 
            x = tot_dnas_latest,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 3, rows = 14:18, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 4, rows = 14:18, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 5, rows = 14:18, stack = TRUE)
  
  writeData(wb, sheet = "Tab 9", 
            x = df_quarts,  
            startCol = 2, startRow = 14, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 9", style = style_date, cols = 2, rows = 14:18, stack = TRUE)
  
  
  #Tab 10
  df_care_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_qt_hb.parquet")) |> 
    select(-total_apps, -prop) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    arrange(desc(count), .by_group = TRUE) |>
    mutate(rank = row_number(),
           top5 = case_when(rank >5 ~ "All other care locations",
                            TRUE ~ loc_label)) |>
    ungroup() |>
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
    mutate(count = sum(count)) |>
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(rank >= 1 & rank <= 6) |>
    add_proportion_ds_hb(vec_group = c("app_quarter_ending", "dataset_type", "hb_name")) %>%
    
    bind_rows(summarise(.,
                        across(count, sum),
                        across(top5, ~"Total"),
                        across(rank, ~ 7),
                        across(prop, ~ 100),
                        .groups = "drop")) |>
    select(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), loc_label = top5, 
           count, rank, total, prop) |>
    mutate(loc_label = case_when(is.na(loc_label) ~ 'Missing data',
                                 TRUE ~ loc_label)) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 10 Data", 
            x = df_care_loc, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 10", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 10", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  
  #Tab 11
  df_prof_group <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_qt_hb.parquet")) |> 
    select(-total_apps, -prop) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    arrange(desc(count), .by_group = TRUE) |>
    mutate(rank = row_number(),
           top5 = case_when(rank >5 ~ "All other professional groups",
                            TRUE ~ prof_label)) |>
    ungroup() |>
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
    mutate(count = sum(count)) |>
    group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(rank >= 1 & rank <= 6) |>
    add_proportion_ds_hb(vec_group = c("app_quarter_ending", "dataset_type", "hb_name")) %>%
    
    bind_rows(summarise(.,
                        across(count, sum),
                        across(top5, ~"Total"),
                        across(rank, ~ 7),
                        across(prop, ~ 100),
                        .groups = "drop")) |>
    select(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), prof_label = top5, 
           count, rank, total, prop) |>
    mutate(prof_label = case_when(is.na(prof_label) ~ 'Missing data',
                                 TRUE ~ prof_label)) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 11 Data", 
            x = df_prof_group, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 11", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 11", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 11", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  #Tab 12
  first_con_dna_simd <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb_simd.parquet")) |> 
    select(-prop_firstcon_att, -total_apps) |>
    rename(firstcon_dnas = firstcon_att,
           firstcon_appts = first_contact) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>
    mutate(first_contact_dnas_tot = sum(firstcon_dnas),
           first_contact_tot = sum(firstcon_appts),
           simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Data missing',
                                         TRUE ~ simd2020_quintile)) |> ungroup() |>
    right_join(df_simd_qt_hb, by = c("app_quarter_ending" = "quarter_ending", "dataset_type", "hb_name", "simd2020_quintile", "Attendance" = "att_status")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, simd2020_quintile) |>
    mutate(firstcon_dnas = case_when(is.na(firstcon_dnas) ~ 0,
                                     TRUE ~ firstcon_dnas),
           firstcon_appts = case_when(is.na(firstcon_appts) ~ 0,
                                      TRUE ~ firstcon_appts)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>
    fill(first_contact_dnas_tot, .direction = "updown") |>
    fill(first_contact_tot, .direction = "updown") |>
    mutate(prop = round(firstcon_dnas / firstcon_appts * 100, 1),
           tot_prop = round(first_contact_dnas_tot/first_contact_tot *100, 1)) |> 
    select(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, simd2020_quintile,
           firstcon_dnas, firstcon_appts, first_contact_dnas_tot, first_contact_tot, prop, tot_prop) |> 
    filter(!!sym(dataset_type_o) == dataset_choice)  
  
  
  writeData(wb, sheet = "Tab 12 Data",
            x = first_con_dna_simd,
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 12", style = style_count, cols = 3, rows = 15:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 12", style = style_count, cols = 4, rows = 15:20, stack = TRUE)
  addStyle(wb, sheet = "Tab 12", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}

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
    change_nhsscotland_label() |>
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
    change_nhsscotland_label() |>
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
    change_nhsscotland_label() |>
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
    change_nhsscotland_label() |>
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
    change_nhsscotland_label() |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 5 Data", 
            x = df_ref_source, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 5", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  #Tab 6  
  first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb.parquet")) |> 
    #select(-prop_app_att) |> 
    #pivot_wider(names_from = Attendance, values_from = apps_att, values_fill = 0) |> 
    #right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    #mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    #arrange(!!dataset_type_o, !!hb_name_o) |> 
    #pivot_longer(cols = 5:11, names_to = "att_status", values_to = "count") |> 
    #mutate(prop = round(count / total_apps * 100, 1)) |> 
    #select(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, att_status,
    #count, prop, total_apps) |> 
    #filter(!!sym(dataset_type_o) == dataset_choice)
    select(-prop_firstcon_att) |> 
    pivot_wider(names_from = Attendance, values_from = firstcon_att, values_fill = 0) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    pivot_longer(cols = 6:12, names_to = "att_status", values_to = "count") |> 
    mutate(prop = round(count / first_contact * 100, 1)) |> 
    select(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, att_status,
           count, first_contact, prop, total_apps) |> 
    change_nhsscotland_label() |>
    filter(!!sym(dataset_type_o) == dataset_choice) 
  
  
  writeData(wb, sheet = "Tab 6 Data", 
            x = first_att_latest,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 6", style = style_count, cols = 3, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = style_count, cols = 4, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = style_count, cols = 5, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 6", style = createStyle(halign = "right"), cols = 6, rows = 15:19, stack = TRUE)
  
  writeData(wb, sheet = "Tab 6", 
            x = df_quarts,  
            startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 6", style = style_date, cols = 2, rows = 15:19, stack = TRUE)
  
  #Tab 7
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
    change_nhsscotland_label() |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 7 Data", 
            x = tot_dnas_latest,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 7", style = style_count, cols = 3, rows = 14:18, stack = TRUE)
  addStyle(wb, sheet = "Tab 7", style = style_count, cols = 4, rows = 14:18, stack = TRUE)
  addStyle(wb, sheet = "Tab 7", style = createStyle(halign = "right"), cols = 5, rows = 14:18, stack = TRUE)
  
  writeData(wb, sheet = "Tab 7", 
            x = df_quarts,  
            startCol = 2, startRow = 14, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 7", style = style_date, cols = 2, rows = 14:18, stack = TRUE)
  
  
  #Tab 8
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
    change_nhsscotland_label() |>
    mutate(loc_label = case_when(is.na(loc_label) ~ 'Missing data',
                                 TRUE ~ loc_label)) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 8 Data", 
            x = df_care_loc, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 8", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 8", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  
  #Tab 9
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
    change_nhsscotland_label() |>
    mutate(prof_label = case_when(is.na(prof_label) ~ 'Missing data',
                                 TRUE ~ prof_label)) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  writeData(wb, sheet = "Tab 9 Data", 
            x = df_prof_group, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 9", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
  addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}
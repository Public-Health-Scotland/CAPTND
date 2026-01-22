

update_mmi_dt_values_comp <- function(wb, time_period){
  
    df_months <- read_parquet(paste0(ref_dir, "referrals_month_hb.parquet")) |> 
      ungroup() |> select(referral_month) |> unique() #|> pull()
    
    df_month_ds_hb <- df_ds_hb_name |> cross_join(df_months)
    month_range <- df_months |> pull()
    
    simd_df <- data.frame(simd2020_quintile = c('1','2', '3', '4','5', 'Data missing'))
    att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                            "Patient CNW", "Not known", "Not recorded"))
    
    df_simd_mth_hb <- df_month_ds_hb |>
      cross_join(simd_df) |>
      mutate(att_status = 'Patient DNA')
      #cross_join(att_status_df)
    
    # replace CAMHS in lookup with PT
    if(dataset_choice == "PT"){
      writeData(wb, sheet = "Lookups", 
                x = "PT",  
                startCol = 2, startRow = 2, #headerStyle = style_text, 
                colNames = FALSE)
    }
    
    ##TAB 1##
    df_ref_sex <- read_parquet(paste0(ref_demo_dir, "referrals_", "month_hb_sex.parquet")) |> 
      ungroup() |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      right_join(df_month_ds_hb, by = c("referral_month", "dataset_type", "hb_name")) |> 
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
    
  
    ##TAB 2##
    df_ref_age <- read_parquet(paste0(ref_demo_dir, "referrals_", "month_hb_age.parquet")) |> 
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
    
    
    ##TAB 3##
    df_ref_simd <- read_parquet(paste0(ref_demo_dir, "referrals_", "month_hb_simd.parquet")) |> 
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
    
    ##TAB 4##
    df_acc_status <- read_parquet(paste0(non_acc_dir, "non_acceptance_summary_", "month_hb.parquet")) |> 
      ungroup() |> 
      select(-total, -prop) |> 
      pivot_wider(names_from = ref_acc_desc, values_from = count,
                  values_fill = 0) |>
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
      pivot_longer(cols = 4:7, values_to = "count", names_to = "ref_acc_desc") |> 
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      mutate(total = sum(count, na.rm = TRUE),
             prop = round(count / total * 100, 1)) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!dataset_type_o, !!hb_name_o) |>
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice) 
    
    
    writeData(wb, sheet = "Tab 4 Data", 
              x = df_acc_status,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 4", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
    addStyle(wb, sheet = "Tab 4", style = style_count, cols = 4, rows = 15:29, stack = TRUE)
    addStyle(wb, sheet = "Tab 4", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
    
    writeData(wb, sheet = "Tab 4", 
              x = df_months,  
              startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
    addStyle(wb, sheet = "Tab 4", style = style_date, cols = 2, rows = 15:29, stack = TRUE)
    
    ## Tab 5 ##
    #non acceptance action
    df_non_acc_reason <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance_reason/non_acceptance_reason_month_hb.parquet")) |> 
      select(-total, -prop) |>
      right_join(df_month_ds_hb, by = c("dataset_type", "hb_name", "referral_month")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             ref_rej_reason_desc = case_when(is.na(ref_rej_reason_desc) ~ 'No data',
                                             TRUE ~ ref_rej_reason_desc),
             count = case_when(is.na(count) ~ 0,
                               TRUE ~ count)) |> 
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      arrange(desc(count), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other non acceptance reasons",
                              TRUE ~ ref_rej_reason_desc)) |>
      ungroup() |>
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
      mutate(count = sum(count)) |>
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      filter(rank >= 1 & rank <= 6) |>
      add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) %>%
      
      bind_rows(summarise(.,
                          across(count, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count, rank, total, prop) |>
      change_nhsscotland_label() |>
      mutate(top5 = case_when(is.na(top5) ~ 'Missing data',
                              TRUE ~ top5),
             variable = 'Non-acceptance reason') |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    #non acceptance actions
    df_non_acc_actions <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance_action/non_acceptance_action_month_hb.parquet")) |> 
      select(-total, -prop) |>
      right_join(df_month_ds_hb, by = c("dataset_type", "hb_name", "referral_month")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             ref_rej_act_desc = case_when(is.na(ref_rej_act_desc) ~ 'No data',
                                          TRUE ~ ref_rej_act_desc),
             count = case_when(is.na(count) ~ 0,
                               TRUE ~ count)) |> 
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      arrange(desc(count), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other non acceptance actions",
                              TRUE ~ ref_rej_act_desc)) |>
      ungroup() |>
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
      mutate(count = sum(count)) |>
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      filter(rank >= 1 & rank <= 6) |>
      add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) %>%
      
      bind_rows(summarise(.,
                          across(count, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count, rank, total, prop) |>
      change_nhsscotland_label() |>
      mutate(top5 = case_when(is.na(top5) ~ 'Missing data',
                              TRUE ~ top5),
             variable = 'Actions following referral non acceptance') |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    df_non_acc <- rbind(df_non_acc_reason, df_non_acc_actions)
    
    writeData(wb, sheet = "Tab 5 Data", 
              x = df_non_acc, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 5", style = style_text, cols = 3, rows = 16:22, stack = TRUE)
    addStyle(wb, sheet = "Tab 5", style = style_count, cols = 4, rows = 16:22, stack = TRUE)
    addStyle(wb, sheet = "Tab 5", style = createStyle(halign = "right"), cols = 5, rows = 16:21, stack = TRUE)

    ##TAB 6##
    df_ref_source <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/ref_source_month_hb.parquet")) |> 
      select(-total, -prop) |>
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      arrange(desc(count), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other referral sources",
                              TRUE ~ ref_source_name)) |>
      ungroup() |> 
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
      mutate(count = sum(count)) |>
      group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      filter(rank >= 1 & rank <= 6) |>
      add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) %>%
      
      bind_rows(summarise(.,
                          across(count, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), ref_source_name = top5, 
             count, rank, total, prop) |>
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    writeData(wb, sheet = "Tab 6 Data", 
              x = df_ref_source, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 6", style = style_text, cols = 3, rows = 15:21, stack = TRUE)
    addStyle(wb, sheet = "Tab 6", style = style_count, cols = 4, rows = 15:21, stack = TRUE)
    addStyle(wb, sheet = "Tab 6", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
    
    
    ##TAB 7##
    first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_mth_hb.parquet")) |> 
      select(-prop_firstcon_att) |> 
      pivot_wider(names_from = Attendance, values_from = firstcon_att, values_fill = 0) |> 
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      pivot_longer(cols = 6:12, names_to = "att_status", values_to = "count") |> 
      mutate(prop = round(count / first_contact * 100, 1)) |> 
      select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), att_status,
             count, first_contact, prop, total_apps) |> 
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice) 
    
    
    writeData(wb, sheet = "Tab 7 Data", 
              x = first_att_latest,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 7", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
    addStyle(wb, sheet = "Tab 7", style = style_count, cols = 4, rows = 15:29, stack = TRUE)
    addStyle(wb, sheet = "Tab 7", style = style_count, cols = 5, rows = 15:29, stack = TRUE)
    addStyle(wb, sheet = "Tab 7", style = createStyle(halign = "right"), cols = 6, rows = 15:29, stack = TRUE)
    
    writeData(wb, sheet = "Tab 7", 
              x = df_months,  
              startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
    addStyle(wb, sheet = "Tab 7", style = style_date, cols = 2, rows = 15:29, stack = TRUE)
    
    ## TAB 8##
    tot_dnas_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_mth_hb.parquet")) |>
      filter(Attendance == 'Patient DNA') |>
      select(!!sym(dataset_type_o), !!(hb_name_o), app_month, dna_count = apps_att, total_apps, 
             dna_rate = prop_apps_att) |>
      mutate(dna_rate = dna_rate/100) |>
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      group_by(app_month, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      ungroup() |>
      arrange(!!dataset_type_o, !!hb_name_o) |>
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 8 Data", 
              x = tot_dnas_latest,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 8", style = style_count, cols = 3, rows = 14:28, stack = TRUE)
    addStyle(wb, sheet = "Tab 8", style = style_count, cols = 4, rows = 14:28, stack = TRUE)
    addStyle(wb, sheet = "Tab 8", style = createStyle(halign = "right"), cols = 5, rows = 14:28, stack = TRUE)
    
    writeData(wb, sheet = "Tab 8", 
              x = df_months,  
              startCol = 2, startRow = 14, headerStyle = style_date, colNames = FALSE)
    addStyle(wb, sheet = "Tab 8", style = style_date, cols = 2, rows = 14:28, stack = TRUE)
    
    ##TAB 9##
    #care location
    df_care_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_mth_hb.parquet")) |> 
      select(-total_apps, -prop) |>
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      group_by(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      arrange(desc(count), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other care locations",
                              TRUE ~ loc_label)) |>
      ungroup() |>
      group_by(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
      mutate(count = sum(count)) |>
      group_by(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      filter(rank >= 1 & rank <= 6) |>
      add_proportion_ds_hb(vec_group = c("app_month", "dataset_type", "hb_name")) %>%
      
      bind_rows(summarise(.,
                          across(count, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count, rank, total, prop) |>
      change_nhsscotland_label() |>
      mutate(top5 = case_when(is.na(top5) ~ 'Missing data',
                              TRUE ~ top5),
             variable = 'Care location') |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    #professional group
    df_prof_group <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_mth_hb.parquet")) |> 
      select(-total_apps, -prop) |>
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      group_by(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      arrange(desc(count), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other professional groups",
                              TRUE ~ prof_label)) |>
      ungroup() |>
      group_by(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), top5) |> 
      mutate(count = sum(count)) |>
      group_by(!!sym(app_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      filter(rank >= 1 & rank <= 6) |>
      add_proportion_ds_hb(vec_group = c("app_month", "dataset_type", "hb_name")) %>%
      
      bind_rows(summarise(.,
                          across(count, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(app_month, !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count, rank, total, prop) |>
      change_nhsscotland_label() |>
      mutate(top5 = case_when(is.na(top5) ~ 'Missing data',
                              TRUE ~ top5),
             variable = 'Professional group') |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    df_appt_prof_loc <- rbind(df_prof_group, df_care_loc)
    
    writeData(wb, sheet = "Tab 9 Data", 
              x = df_appt_prof_loc, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 9", style = style_text, cols = 3, rows = 16:22, stack = TRUE)
    addStyle(wb, sheet = "Tab 9", style = style_count, cols = 4, rows = 16:22, stack = TRUE)
    addStyle(wb, sheet = "Tab 9", style = createStyle(halign = "right"), cols = 5, rows = 16:21, stack = TRUE)
    
    ##Tab 10##
    first_con_dna_simd <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_mth_hb_simd.parquet")) |> 
      select(-prop_firstcon_att, -total_apps) |>
      rename(firstcon_dnas = firstcon_att,
             firstcon_appts = first_contact) |>
      filter(Attendance == 'Patient DNA') |>
      group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o)) |>
      mutate(first_contact_dnas_tot = sum(firstcon_dnas),
             first_contact_tot = sum(firstcon_appts),
             simd2020_quintile = as.character(simd2020_quintile),
             simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Data missing',
                                           TRUE ~ simd2020_quintile)) |> ungroup() |>
      right_join(df_simd_mth_hb, by = c("app_month" = "referral_month", "dataset_type", "hb_name", "simd2020_quintile", "Attendance" = "att_status")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), simd2020_quintile) |>
      mutate(firstcon_dnas = case_when(is.na(firstcon_dnas) ~ 0,
                                       TRUE ~ firstcon_dnas),
             firstcon_appts = case_when(is.na(firstcon_appts) ~ 0,
                                        TRUE ~ firstcon_appts)) |>
      group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o)) |>
      fill(first_contact_dnas_tot, .direction = "updown") |>
      fill(first_contact_tot, .direction = "updown") |>
      mutate(prop = round(firstcon_dnas / firstcon_appts * 100, 1),
             tot_prop = round(first_contact_dnas_tot/first_contact_tot *100, 1)) |> 
      select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), simd2020_quintile,
             firstcon_dnas, firstcon_appts, first_contact_dnas_tot, first_contact_tot, prop, tot_prop) |> 
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)  
    
    
    writeData(wb, sheet = "Tab 10 Data", 
              x = first_con_dna_simd,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 10", style = style_count, cols = 3, rows = 15:20, stack = TRUE)
    addStyle(wb, sheet = "Tab 10", style = style_count, cols = 4, rows = 15:20, stack = TRUE)
    addStyle(wb, sheet = "Tab 10", style = createStyle(halign = "right"), cols = 5, rows = 15:20, stack = TRUE)
    
    ##TAB 11##
    
    variables <- c("Care plan inclusion status", "Adult protection status", "Veteran status")
    
    if(dataset_choice == "PT"){
      writeData(wb, sheet = "Lookups", 
                x = variables,
                startCol = 10, startRow = 2, 
                colNames = FALSE)
    }
    
    if(dataset_choice == "CAMHS"){
      
    df_ref_lac <- read_parquet(paste0(ref_lac_dir, "referrals_lac_", "mth_hb.parquet")) |> 
      ungroup() |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             variable = 'Looked after child status') |> 
      arrange(!!sym(hb_name_o), referral_month) |> 
      rename(response = looked_after_c_edited) |>
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)

    df_ref_cps <- read_parquet(paste0(ref_prot_dir, "referrals_prot_", "child_mth_hb.parquet")) |> 
      ungroup() |>  
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             variable = 'Child protection status') |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      rename(response = prot_label) |>
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    df_care_plan <- read_parquet(paste0(ref_care_plan_dir, "referrals_care_plan_", "mth_hb.parquet")) |> 
      ungroup() |>  
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             variable = 'Care plan inclusion status') |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      rename(response = care_plan_inc) |>
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    ref_variables <- rbind(df_ref_lac, df_ref_cps, df_care_plan)
    
    } else {
      
      
      df_ref_vets <- read_parquet(paste0(ref_vets_dir, "referrals_vets_", "mth_hb.parquet")) |> 
        ungroup() |> 
        mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
               variable = 'Veteran status') |> 
        arrange(!!sym(hb_name_o), referral_month) |> 
        rename(response = vet_label) |>
        change_nhsscotland_label() |>
        filter(dataset_type == dataset_choice)
      
      df_ref_aps <- read_parquet(paste0(ref_prot_dir, "referrals_prot_", "adult_mth_hb.parquet")) |> 
        ungroup() |>  
        mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
               variable = 'Adult protection status') |> 
        arrange(!!dataset_type_o, !!hb_name_o) |> 
        rename(response = prot_label) |>
        change_nhsscotland_label() |>
        filter(dataset_type == dataset_choice)
      
      df_care_plan <- read_parquet(paste0(ref_care_plan_dir, "referrals_care_plan_", "mth_hb.parquet")) |> 
        ungroup() |>  
        mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
               variable = 'Care plan inclusion status') |> 
        arrange(!!dataset_type_o, !!hb_name_o) |> 
        rename(response = care_plan_inc) |>
        change_nhsscotland_label() |>
        filter(dataset_type == dataset_choice)
      
      ref_variables <- rbind(df_ref_vets, df_ref_aps, df_care_plan)
      
    }
    
    writeData(wb, sheet = "Tab 11 Data", 
              x = ref_variables, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 11", style = style_count, cols = 3, rows = 16:20, stack = TRUE)
    addStyle(wb, sheet = "Tab 11", style = createStyle(halign = "right"), cols = 4, rows = 16:20, stack = TRUE)
    
    
    ##Tab 12##
    #presenting problem
    presenting_prob_df <- read_parquet(paste0(present_prob_dir, "presenting_prob_mth.parquet")) |>
      ungroup() |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             variable = 'Presenting problem',
             present_prob_name = case_when(is.na(present_prob_name) ~ 'Data missing',
                                           TRUE ~ present_prob_name)) |>
      group_by(app_month, !!sym(dataset_type_o), !!sym(hb_name_o), level) |>
      arrange(desc(n), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other presenting problems",
                              TRUE ~ present_prob_name)) |>
      ungroup() |>
      group_by(app_month, !!sym(dataset_type_o), !!sym(hb_name_o), level, top5) |> 
      mutate(n = sum(n)) |>
      group_by(app_month, !!sym(dataset_type_o), !!sym(hb_name_o), level, variable) |>
      filter(rank >= 1 & rank <= 6) |>
      mutate(total = sum(n),
             prop = round (n / total * 100 , 1)) %>%
      
      bind_rows(summarise(.,
                          across(n, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(month = app_month, !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count = n, rank, total, prop, level, variable) |>
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)
      
    #treatment reason
    treat_reason_df <- read_parquet(paste0(treat_reason_dir, "/treat_reason_mth.parquet")) |>
      ungroup() |>
      filter(!is.na(treat_reason_desc)) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             variable = 'Reason for treatment') |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level) |>
      arrange(desc(n), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other treatment reasons",
                              TRUE ~ treat_reason_desc)) |>
      ungroup() |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level, top5) |> 
      mutate(n = sum(n)) |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level, variable) |>
      filter(rank >= 1 & rank <= 6) |>
      mutate(total = sum(n),
             prop = round (n / total * 100 , 1)) %>%
      
      bind_rows(summarise(.,
                          across(n, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(month = treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count = n, rank, total, prop, level, variable) |>
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)
      
    #treatment intervention
    treat_inter_df <- read_parquet(paste0(treat_intervention_dir, "/treat_intervention_mth.parquet")) |>
      ungroup() |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             variable = 'Treatment received') |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level) |>
      arrange(desc(n), .by_group = TRUE) |>
      mutate(rank = row_number(),
             top5 = case_when(rank >5 ~ "All other treatment interventions",
                              TRUE ~ treat_name_long)) |>
      ungroup() |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level, top5) |> 
      mutate(n = sum(n)) |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level, variable) |>
      filter(rank >= 1 & rank <= 6) |>
      mutate(total = sum(n),
             prop = round (n / total * 100 , 1)) %>%
      
      bind_rows(summarise(.,
                          across(n, sum),
                          across(top5, ~"Total"),
                          across(rank, ~ 7),
                          across(prop, ~ 100),
                          .groups = "drop")) |>
      select(month = treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), top5, 
             count = n, rank, total, prop, level, variable) |>
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    df_diag_treat <- rbind(presenting_prob_df, treat_reason_df, treat_inter_df)
    
    writeData(wb, sheet = "Tab 12 Data", 
              x = df_diag_treat, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 12", style = style_text, cols = 3, rows = 17:23, stack = TRUE)
    addStyle(wb, sheet = "Tab 12", style = style_count, cols = 4, rows = 17:23, stack = TRUE)
    addStyle(wb, sheet = "Tab 12", style = createStyle(halign = "right"), cols = 5, rows = 17:22, stack = TRUE)
    
    #Tab 13
    
    add_proportion <- function(df, vec_group = c("dataset_type", "hb_name")){
      df_prop <- df |> 
        group_by(across(all_of(vec_group))) |> 
        mutate(total = sum(count),
               prop = round ( count / total * 100 , 2))
      return(df_prop)
    }
    
    df_cgi_i <- read_parquet(paste0(clinical_outcomes_dir, "clinical_outcomes_cgi_i_monthly.parquet")) |>
      ungroup() |> 
      right_join(df_cgi_i_ds_hb, by = c("dataset_type", "hb_name", "header_date" = "month", "outcome_code", "outcome")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             count = case_when(hb_name == 'NHS Lothian' & is.na(count) ~ 0,
                               hb_name == 'NHS Scotland' & is.na(count) ~ 0,
                               TRUE ~ count)) |> 
      arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(header_date_o)) |> 
      add_proportion(vec_group = c("dataset_type", "hb_name", "header_date", "outcome")) |> 
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    
    df_pgi_i <- read_parquet(paste0(clinical_outcomes_dir, "clinical_outcomes_pgi_i_monthly.parquet")) |>
      ungroup() |> 
      right_join(df_pgi_i_ds_hb, by = c("dataset_type", "hb_name", "header_date" = "month", "outcome_code", "outcome")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             count = case_when(hb_name == 'NHS Lothian' & is.na(count) ~ 0,
                               hb_name == 'NHS Scotland' & is.na(count) ~ 0,
                           TRUE ~ count)) |> 
      arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(header_date_o)) |> 
      add_proportion(vec_group = c("dataset_type", "hb_name", "header_date", "outcome")) |> 
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    
    df_cgi_s <- read_parquet(paste0(clinical_outcomes_dir, "clinical_outcomes_cgi_s_monthly.parquet")) |>
      ungroup() |> 
      right_join(df_cgi_s_ds_hb, by = c("dataset_type", "hb_name", "header_date" = "month", "outcome_code", "outcome")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
             count = case_when(hb_name == 'NHS Lothian' & is.na(count) ~ 0,
                               hb_name == 'NHS Scotland' & is.na(count) ~ 0,
                           TRUE ~ count)) |> 
      arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(header_date_o)) |> 
      add_proportion(vec_group = c("dataset_type", "hb_name", "header_date", "outcome")) |> 
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    
    df_clinical_outcomes <- rbind(df_cgi_i, df_pgi_i, df_cgi_s)
    
    writeData(wb, sheet = "Tab 13 Data", 
              x = df_clinical_outcomes, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 13", style = style_text, cols = 3, rows = 16:24, stack = TRUE)
    addStyle(wb, sheet = "Tab 13", style = createStyle(halign = "right"), cols = 5, rows = 16:23, stack = TRUE)
    
    #Tab 14
    treat_group_ind_df <- read_parquet(paste0(group_ind_dir, "treat_group_ind_mth.parquet")) |>
      ungroup() |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |>
      group_by(treat_month, !!sym(dataset_type_o), !!sym(hb_name_o), level) |>
      mutate(total = sum(count),
             prop = round (count / total * 100 , 1)) %>%
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    writeData(wb, sheet = "Tab 14 Data", 
              x = treat_group_ind_df, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 14", style = style_text, cols = 3, rows = 16:19, stack = TRUE)
    addStyle(wb, sheet = "Tab 14", style = createStyle(halign = "right"), cols = 5, rows = 16:18, stack = TRUE)
    
    #Tab 15
    dis_df <- read_parquet(paste0(dis_dir, "discharges_month_hb.parquet")) |>
      ungroup() |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |>
      change_nhsscotland_label() |>
      filter(!!sym(dataset_type_o) == dataset_choice)
    
    writeData(wb, sheet = "Tab 15", 
              x = df_months,  
              startCol = 2, startRow = 14, headerStyle = style_date, colNames = FALSE)
    addStyle(wb, sheet = "Tab 15", style = style_date, cols = 2, rows = 14:28, stack = TRUE)
    
    writeData(wb, sheet = "Tab 15 Data", 
              x = dis_df, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 15", style = style_text, cols = 3, rows = 14:28, stack = TRUE)
    
    #Tab 16
    ppmh_df <- read_parquet(paste0(ref_ppmh_dir, "referrals_ppmh_mth_hb_sex.parquet")) |>
      ungroup() |>
      arrange(!!dataset_type_o, !!hb_name_o) |>  
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      change_nhsscotland_label() |>
      filter(dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 16 Data", 
              x = ppmh_df, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Tab 16", style = style_text, cols = 3, rows = 15:20, stack = TRUE)
    addStyle(wb, sheet = "Tab 16", style = createStyle(halign = "right"), cols = 5, rows = 15:19, stack = TRUE)
      
      
    ## Lookup ##
    
    writeData(wb, sheet = "Lookups", 
              x = df_months,  
              startCol = 7, startRow = 2, headerStyle = style_date, colNames = FALSE)
    addStyle(wb, sheet = "Lookups", style = style_date, cols = 7, rows = 2:16, stack = TRUE)
    
    hb_list <- hb_vector[1:14]
    
    writeData(wb, sheet = "Lookups", 
              x = hb_list,  
              startCol = 3, startRow = 3, headerStyle = style_text, colNames = FALSE)
    addStyle(wb, sheet = "Lookups", style = style_date, cols = 3, rows = 3:16, stack = TRUE)
    
    assign(x = "wb", value = wb, envir = .GlobalEnv)
    
  
}


############################################.
## Create CAPTND Publication Checks Sheet ##
############################################.

#Author: Luke Taylor
#Written: 12/11/2025

month_end <- "2025-12-01"

#source scripts
source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

create_error_report <- function(dataset_choice = c("CAMHS", "PT")){
  
  
  # get quarters ending for all dts
  df_quarts <- read_parquet(paste0(ref_dir, "referrals_quarter_hb.parquet")) |> 
    ungroup() |> select(quarter_ending) |> unique() #|> pull()
  
  df_qt_ds_hb <- df_ds_hb_name |> cross_join(df_quarts)
  quarter_range <- df_quarts |> pull()
  
  wb <- loadWorkbook("../../../report_templates/publication/pub_data_check_template.xlsx")
    
    #write data into tabs
    date_style <- createStyle(numFmt = "yyyy/mm/dd")
    
    #Tab1
    df_ref_sex <- read_parquet(paste0(ref_demo_dir, "referrals_", "quarter_hb_sex.parquet")) |> 
      ungroup() |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!sym(dataset_type_o), !!(hb_name_o)) |>
      filter(hb_name != 'NHS Scotland' & hb_name != 'NHS 24') |>
      group_by(quarter_ending, !!sym(dataset_type_o), !!sym(sex_reported_o)) |>
      mutate(nhsscot_sex_tot = sum(count, na.rm = TRUE),
             nhsscot_sex_tot_pop = sum(population, na.rm = TRUE),
             nhsscot_rate_per_1000 = round(nhsscot_sex_tot/nhsscot_sex_tot_pop*1000, 2)) |>
      select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o),
             pop_rate_1000, nhsscot_rate_per_1000) |>
      filter(dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 1 Data", 
              x = df_ref_sex, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
    # df_scot_tot <- df_ref_sex |>
    #   filter(!!sym(hb_name_o) == 'NHS Scotland',
    #          sex_reported != 'Not specified' & sex_reported != 'Not known') |>
    #   group_by(dataset_type, sex_reported) |>
    #   mutate(sd = sd(pop_rate_1000),
    #          sd2 = sd*2)
    
    
    #Tab 2  
    df_ref_age <- read_parquet(paste0(ref_demo_dir, "referrals_", "quarter_hb_age.parquet")) |> 
      ungroup() |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!dataset_type_o, !!hb_name_o) |>
      filter(hb_name != 'NHS Scotland' & hb_name != 'NHS 24') |>
      group_by(quarter_ending, !!sym(dataset_type_o), agg_age_groups) |>
      mutate(nhsscot_age_tot = sum(count, na.rm = TRUE),
             nhsscot_age_tot_pop = sum(population, na.rm = TRUE),
             nhsscot_rate_per_1000 = round(nhsscot_age_tot/nhsscot_age_tot_pop*1000, 2)) |>
      select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), agg_age_groups,
             pop_rate_1000, nhsscot_rate_per_1000) |>
      filter(dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 2 Data", 
              x = df_ref_age, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
    #Tab 3
    df_ref_simd <- read_parquet(paste0(ref_demo_dir, "referrals_", "quarter_hb_simd.parquet")) |> 
      ungroup() |> 
      arrange(!!dataset_type_o, !!hb_name_o) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      arrange(!!dataset_type_o, !!hb_name_o) |>
      filter(hb_name != 'NHS Scotland' & hb_name != 'NHS 24') |>
      group_by(quarter_ending, !!sym(dataset_type_o), simd2020_quintile) |>
      mutate(nhsscot_simd_tot = sum(count, na.rm = TRUE),
             nhsscot_simd_tot_pop = sum(population, na.rm = TRUE),
             nhsscot_rate_per_1000 = round(nhsscot_simd_tot/nhsscot_simd_tot_pop*1000, 2)) |>
      select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), simd2020_quintile,
             pop_rate_1000, nhsscot_rate_per_1000) |>
      filter(dataset_type == dataset_choice)
    
    writeData(wb, sheet = "Tab 3 Data", 
              x = df_ref_simd, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
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
      filter(hb_name != 'NHS Scotland' & hb_name != 'NHS 24') |>
      group_by(quarter_ending, !!sym(dataset_type_o), ref_acc_desc) |>
      mutate(nhsscot_count = sum(count),
             nhsscot_tot = sum(total),
             nhsscot_prop = round(nhsscot_count/nhsscot_tot*100, 1)) |>
      arrange(!!dataset_type_o, !!hb_name_o) |>
      filter(!!sym(dataset_type_o) == dataset_choice) 
    
    writeData(wb, sheet = "Tab 4 Data", 
              x = df_acc_status,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
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
      mutate(count = sum(count)) |> ungroup() |>
      filter(rank >= 1 & rank <= 6) |>
      add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name"))  |>
      select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), rej_reason = top5, 
             count, rank, total, prop) |>
      mutate(rej_reason = case_when(is.na(rej_reason) ~ 'Missing data',
                                    TRUE ~ rej_reason))
    
    df_scot_tot <- df_non_acc_reason |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(quarter_ending, dataset_type, rej_reason, nhsscot_count = count, nhsscot_rank = rank, nhsscot_prop = prop)
    
    df_non_acc_reason <- df_non_acc_reason |>
      left_join(df_scot_tot, by = c("quarter_ending", "dataset_type", "rej_reason")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 5 Data", 
              x = df_non_acc_reason, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
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
      select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), rej_action = top5, 
             count, rank, total, prop) |>
      mutate(rej_action = case_when(is.na(rej_action) ~ 'Missing data',
                                    TRUE ~ rej_action))
    
    df_scot_tot <- df_non_acc_actions |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(quarter_ending, dataset_type, rej_action, nhsscot_count = count, nhsscot_rank = rank, nhsscot_prop = prop)
    
    df_non_acc_actions <- df_non_acc_actions |>
      left_join(df_scot_tot, by = c("quarter_ending", "dataset_type", "rej_action")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 6 Data", 
              x = df_non_acc_actions, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
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
      select(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), ref_source_name = top5, 
             count, rank, total, prop) 
    
    df_scot_tot <- df_ref_source |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(quarter_ending, dataset_type, ref_source_name, nhsscot_count = count, nhsscot_rank = rank, nhsscot_prop = prop)
    
    df_ref_source <- df_ref_source |>
      left_join(df_scot_tot, by = c("quarter_ending", "dataset_type", "ref_source_name")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 7 Data", 
              x = df_ref_source, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
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
             count, first_contact, prop, total_apps)
    
    df_scot_tot <- first_att_latest |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(app_quarter_ending, dataset_type, att_status, nhsscot_count = count, nhsscot_prop = prop)
    
    first_att_latest <- first_att_latest |>
      left_join(df_scot_tot, by = c("app_quarter_ending", "dataset_type", "att_status")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 8 Data", 
              x = first_att_latest,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
    #Tab 9
    tot_dnas_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |>
      filter(Attendance == 'Patient DNA') |>
      select(!!sym(dataset_type_o), !!(hb_name_o), app_quarter_ending, dna_count = apps_att, total_apps, 
             dna_rate = prop_apps_att) |>
      right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
      group_by(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
      ungroup() |>
      arrange(!!dataset_type_o, !!hb_name_o) 
    
    df_scot_tot <- tot_dnas_latest |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(app_quarter_ending, dataset_type, Attendance, nhsscot_count = dna_count, nhsscot_prop = dna_rate)
    
    tot_dnas_latest <- tot_dnas_latest |>
      left_join(df_scot_tot, by = c("app_quarter_ending", "dataset_type", "Attendance")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 9 Data", 
              x = tot_dnas_latest,  
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
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
      select(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), loc_label = top5, 
             count, rank, total, prop) |>
      mutate(loc_label = case_when(is.na(loc_label) ~ 'Missing data',
                                    TRUE ~ loc_label))
    
    df_scot_tot <- df_care_loc |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(app_quarter_ending, dataset_type, loc_label, nhsscot_count = count, nhsscot_rank = rank, nhsscot_prop = prop)
    
    df_care_loc <- df_care_loc |>
      left_join(df_scot_tot, by = c("app_quarter_ending", "dataset_type", "loc_label")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 10 Data", 
              x = df_care_loc, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
    
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
      select(app_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), prof_label = top5, 
             count, rank, total, prop) |>
      mutate(prof_label = case_when(is.na(prof_label) ~ 'Missing data',
                                    TRUE ~ prof_label))
    
    df_scot_tot <- df_prof_group |> ungroup() |>
      filter(hb_name == 'NHS Scotland') |>
      select(app_quarter_ending, dataset_type, prof_label, nhsscot_count = count, nhsscot_rank = rank, nhsscot_prop = prop)
    
    df_prof_group <- df_prof_group |>
      left_join(df_scot_tot, by = c("app_quarter_ending", "dataset_type", "prof_label")) |>
      filter(!!sym(dataset_type_o) == dataset_choice,
             !!sym(hb_name_o) != 'NHS Scotland')
    
    writeData(wb, sheet = "Tab 11 Data", 
              x = df_prof_group, 
              startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
    
    
    #save workbook
    saveWorkbook(wb,paste0(root_dir, "/shorewise_publication/report/pub_data_checks_", 
                           dataset_choice, "_", month_end, ".xlsx"), 
                 overwrite = TRUE)
  
}

create_error_report(dataset_choice = "CAMHS")
create_error_report(dataset_choice = "PT")



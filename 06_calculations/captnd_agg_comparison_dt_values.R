

captnd_agg_comp_dt_values <- function(wb){
  
  # get months ending for all dts
  month_end <- "2024-12-01"
  
  month_end <- ymd(month_end)
  month_start <- ymd(month_end) - months(14)
  date_range <- seq.Date(from = month_start, to = month_end, by = "month")
  
  df_time <- data.frame(month = date_range) |> 
    append_quarter_ending(date_col = "month")
  
  df_months <- df_time |> select(month)
  df_quarts <- df_time |> select(quarter_ending)
  
  df_qt_ds_hb <- df_ds_hb_name |> cross_join(df_quarts) |> distinct()
  df_month_ds_hb <- df_ds_hb_name |> cross_join(df_months)
  
  
  # replace CAMHS in lookup with PT
  if(dataset_choice == "PT"){
    writeData(wb, sheet = "Lookups", 
              x = "PT",  
              startCol = 2, startRow = 2, #headerStyle = style_text, 
              colNames = FALSE)
  }
  
  
  #Tab 1  
  df_refs <- read_parquet(paste0(referrals_dir, "/comp_data_referrals.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    rename(n_captnd = n) |>
    change_nhsscotland_label() |>
    select(referral_month, dataset_type, hb_name, ref_acc_last_reported, n_captnd, n_aggregate, captnd_perc_agg) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 1 Data", 
            x = df_refs, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Referrals", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Referrals", style = style_count, cols = 4, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Referrals", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
  
  
  #Tab 2  
  df_dna <- read_parquet(paste0(dna_dir, "/comp_data_dna.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_month_ds_hb, by = c("app_month" = "month", "dataset_type", "hb_name")) |> 
    mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    rename(n_captnd = app_count) |>
    change_nhsscotland_label() |>
    select(app_month, dataset_type, hb_name, n_captnd, n_aggregate, captnd_perc_agg) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 2 Data", 
            x = df_dna, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "DNAs", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "DNAs", style = style_count, cols = 4, rows = 15:29, stack = TRUE) 
  addStyle(wb, sheet = "DNAs", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
  
  
  #Tab 3
  df_open_cases <- read_parquet(paste0(open_cases_dir, "/comp_data_opencases_CAMHS.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_month_ds_hb, by = c("month", "dataset_type", "hb_name")) |> 
    mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    change_nhsscotland_label() |>
    select(month, dataset_type, hb_name, n_captnd, n_aggregate, captnd_perc_agg) |>
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 3 Data", 
            x = df_open_cases, 
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Open cases", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Open cases", style = style_count, cols = 4, rows = 15:29, stack = TRUE) 
  addStyle(wb, sheet = "Open cases", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
  
  
  #Tab 4
  df_first_contact <- read_parquet(paste0(first_contact_dir, "/comp_data_firstcontact.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_month_ds_hb, by = c("app_month" = "month", "dataset_type", "hb_name")) |> 
    mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    rename(n_captnd = n) |>
    change_nhsscotland_label() |>
    select(app_month, dataset_type, hb_name, contact_type, n_captnd, n_aggregate, captnd_perc_agg) |>
    filter(dataset_type == dataset_choice)
  
  
  writeData(wb, sheet = "Tab 4 Data", 
            x = df_first_contact,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "First contact", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "First contact", style = style_count, cols = 4, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "First contact", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
  
  #writeData(wb, sheet = "Tab 4", 
            #x = df_quarts,  
            #startCol = 2, startRow = 15, headerStyle = style_date, colNames = FALSE)
  #addStyle(wb, sheet = "Tab 4", style = style_date, cols = 2, rows = 15:19, stack = TRUE)
  
  
  #Tab 5
  df_pat_wait <- read_parquet(paste0(patients_waiting_dir, "/comp_data_patients_waiting_monthly.parquet")) |> 
    select(-measure) |>
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_month_ds_hb, by = c("month", "dataset_type", "hb_name")) |> 
    mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    change_nhsscotland_label() |>
    select(month, dataset_type, hb_name, waiting_period, n_captnd, n_aggregate, captnd_perc_agg) |>
    filter(dataset_type == dataset_choice)
  
  
  writeData(wb, sheet = "Tab 5 Data", 
            x = df_pat_wait,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Patients waiting", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Patients waiting", style = style_count, cols = 4, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Patients waiting", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
  
  
  #Tab 6  
  df_pat_seen <- read_parquet(paste0(patients_seen_dir, "/comp_data_patientsseen.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_month_ds_hb, by = c("app_month" = "month", "dataset_type", "hb_name")) |> 
    mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    rename(n_captnd = n) |>
    change_nhsscotland_label() |>
    select(app_month, dataset_type, hb_name, waiting_period, n_captnd, n_aggregate, captnd_perc_agg) |>
    filter(dataset_type == dataset_choice)
  
  
  writeData(wb, sheet = "Tab 6 Data", 
            x = df_pat_seen,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Patients seen", style = style_count, cols = 3, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Patients seen", style = style_count, cols = 4, rows = 15:29, stack = TRUE)
  addStyle(wb, sheet = "Patients seen", style = createStyle(halign = "right"), cols = 5, rows = 15:29, stack = TRUE)
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}


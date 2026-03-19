
##############################.
### Update DQ table values ###
##############################.

# Author: Charlie Smith
# Date: 2024-09-03

#save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_all")) 
# save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_latest"))


update_dq_values <- function(wb){
  
  # update submission summaries
  df_subs_sum <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/submission_summary.parquet")) |> 
    rename(`Dataset Type` = dataset_type, 
           `Proportion (%)` = Proportion)
  
  writeDataTable(wb, sheet = "HB Submissions", 
                 x = df_subs_sum,  
                 startCol = 2, startRow = 12, #headerStyle = style_text, 
                 colNames = TRUE, withFilter = FALSE)
  addStyle(wb, sheet = "HB Submissions", style = style_text, cols = 2:3, rows = 12:14, 
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "HB Submissions", style = style_count, cols = 4:6, rows = 12:14, 
           stack = TRUE, gridExpand = TRUE)
  
  df_subs_detail <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/submission_detail.parquet")) |> 
    rename(`Dataset Type` = dataset_type,
           `Health Board`= hb_name,
           PMS = pms)
  
  writeDataTable(wb, sheet = "HB Submissions", 
                 x = df_subs_detail,  
                 startCol = 8, startRow = 12, #headerStyle = style_text, 
                 colNames = TRUE, withFilter = FALSE)
  addStyle(wb, sheet = "HB Submissions", style = style_text, cols = 8:11, rows = 12:41, 
           stack = TRUE, gridExpand = TRUE)
  
  
  # update "Heatmap Data"
  df_heat <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_latest.parquet")) |> 
    mutate(value = factor(value, levels = vec_value)) |> 
    append_nhsscotland_label_factor() |> 
    arrange(dataset_type, hb_name, variable, value) |> 
    update_nas_and_zeros() |>
    unique() |>
    rename(Month = header_date_month, 
           `Submission Status` = submission_status, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           PMS = pms,
           Variable = variable,
           `DQ Assessment` = value,
           Count = count, Total = total, Proportion = proportion)
  
  writeDataTable(wb, sheet = "Heatmap Data", 
                 x = df_heat,  
                 startCol = 2, startRow = 13, headerStyle = style_text, 
                 colNames = TRUE, withFilter = TRUE, keepNA = TRUE, na.string = "-")
  
  addStyle(wb, sheet = "Heatmap Data", style = style_text, cols = 2:11, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_date, cols = 2, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_count, cols = 9:10, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_percent, cols = 11, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  
  # update vec_timeframe to "DQ Trend"
  # vec_timeframe
  writeData(wb, sheet = "DQ Trend", 
            x = vec_timeframe,  
            startCol = 7, startRow = 20, #headerStyle = style_text, 
            colNames = TRUE, withFilter = TRUE)
  addStyle(wb, sheet = "DQ Trend", style = style_date, cols = 7, rows = 20:34, #41:(length(vec_timeframe)+41),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "DQ Trend", style = style_percent2, cols = 8:11, rows = 20:34, #41:(length(vec_timeframe)+41),
           stack = TRUE, gridExpand = TRUE)
  
  # update "Trend Data"
  df_trend <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_all.parquet")) |> 
    mutate(value = str_to_title(value),
           value = factor(value, levels = c("Known", "Missing", "Invalid", "Not Known"))) |> 
    arrange(header_date_month, dataset_type, hb_name, variable, value) |> 
    append_nhsscotland_label_factor() |> 
    select(Month = header_date_month, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           Variable = variable,
           `Submission Status` = submission_status, 
           PMS = pms,
           `DQ Assessment` = value,
           Count = count, -total, Proportion = proportion) |> 
    group_by(Month, Dataset, `Health Board`, Variable, `Submission Status`, PMS, `DQ Assessment`) |>   # added in to prevent pivoted table generating a list
    summarise(Count = sum(Count, na.rm = TRUE),                                                        # added in to prevent pivoted table generating a list
    Proportion = mean(Proportion, na.rm = TRUE), .groups = "drop") |>                                  # added in to prevent pivoted table generating a list
    pivot_wider(names_from = `DQ Assessment`, values_from = c(Count, Proportion),                    
                names_sep = " ")
  
  df_trend <- df_trend |> mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .)))
  
  trend_row <- nrow(df_trend)+1
  
  deleteData(wb, sheet = "Trend Data", cols = 2:14, rows = 2:trend_row, gridExpand = TRUE)
  
  writeData(wb, sheet = "Trend Data",
            x = df_trend,
            startCol = 2, startRow = 2, #headerStyle = style_text,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  # update concat depending on number of rows
  # for(i in 2:trend_row){
  #   formula = paste0("=CONCATENATE(B", i, ", C", i, ", D", i, ", G", i,")")
  #   writeFormula(wb, sheet = "Trend Data", x = formula, startCol = 1, startRow = i, array = FALSE)
  # }
  
  # trend - alternative table with tic mark
  
  # proportion
  df_trend2 <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_all.parquet")) |> 
    mutate(value = str_to_title(value),
           value = factor(value, levels = c("Known", "Missing", "Invalid", "Not Known"))) |> 
    append_nhsscotland_label_factor() |> 
    arrange(dataset_type, hb_name, header_date_month) |> 
    select(Month = header_date_month, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           Variable = variable,
           `Submission Status` = submission_status, 
           PMS = pms,
           `DQ Assessment` = value,
           -count, -total, Proportion = proportion) |> 
    group_by(Month, Dataset, `Health Board`, Variable, `Submission Status`, PMS, `DQ Assessment`) |>     # added in to prevent pivoted table generating a list
    summarise(Proportion = mean(Proportion, na.rm = TRUE), .groups = "drop") |>                          # added in to prevent pivoted table generating a list
    mutate(Month = format(parse_date_time(Month, orders = c("ymd", "Y-m", "b-Y")), "%b-%Y")) |>          # added in to prevent pivoted table generating a list
    pivot_wider(names_from = Month, values_from = Proportion) |>
    mutate(Measurement = "Proportion")|>
    relocate(Measurement, .after = `DQ Assessment`)
  
  df_trend2 <- df_trend2 |> mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .)))
  
  # reformat proportion for Heatmaps
  
  # month_levels <- names(df_trend2)[grepl("^[A-Z][a-z]{2}-\\d{4}$", names(df_trend2))]
  # 
  # var_cat_lookup <- df_charts %>%
  #   distinct(variable, var_cat)
  # 
  # df_trend2_restructured <- df_trend2 %>%
  #   pivot_longer(
  #     cols = all_of(month_levels),
  #     names_to = "month",
  #     values_to = "proportion_num"
  #   ) %>%
  #   transmute(
  #     header_date_month = dmy(paste0("01-", month)),
  #     submission_status = `Submission Status`,
  #     dataset_type      = Dataset,
  #     hb_name           = `Health Board`,
  #     pms               = PMS,
  #     variable          = Variable,
  #     value             = str_to_lower(as.character(`DQ Assessment`)),
  #     count             = NA_character_,
  #     total             = NA_character_,
  #     proportion        = as.character(proportion_num)
  #   ) %>%
  #   mutate(
  #     prop_group = case_when(
  #       is.na(as.numeric(proportion))                    ~ NA_character_,
  #       as.numeric(proportion) == 0                      ~ "0%",
  #       as.numeric(proportion) > 0  & as.numeric(proportion) <= 33  ~ ">0-33%",
  #       as.numeric(proportion) > 33 & as.numeric(proportion) <= 66  ~ ">33-66%",
  #       as.numeric(proportion) > 66 & as.numeric(proportion) < 90   ~ ">66-<90%",
  #       as.numeric(proportion) >= 90 & as.numeric(proportion) < 95  ~ "90-<95%",
  #       as.numeric(proportion) >= 95 & as.numeric(proportion) < 100 ~ "95-<100%",
  #       as.numeric(proportion) == 100                    ~ "100%",
  #       TRUE                                             ~ NA_character_
  #     )
  #   ) %>%
  #   left_join(var_cat_lookup, by = "variable") %>%
  #   mutate(
  #     header_date_month = as.Date(header_date_month),
  #     submission_status = as.character(submission_status),
  #     dataset_type      = as.character(dataset_type),
  #     hb_name           = factor(as.character(hb_name), levels = levels(df_charts$hb_name)),
  #     pms               = as.character(pms),
  #     variable          = factor(as.character(variable), levels = levels(df_charts$variable)),
  #     value             = as.character(value),
  #     count             = as.character(count),
  #     total             = as.character(total),
  #     proportion        = as.character(proportion),
  #     prop_group        = factor(as.character(prop_group), levels = levels(df_charts$prop_group)),
  #     var_cat           = factor(as.character(var_cat), levels = levels(df_charts$var_cat))
  #   ) %>%
  #   select(names(df_charts))
  
  
  # count
  df_trend2_count <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_all.parquet")) |> 
    mutate(value = str_to_title(value),
           value = factor(value, levels = c("Known", "Missing", "Invalid", "Not Known"))) |> 
    append_nhsscotland_label_factor() |> 
    arrange(dataset_type, hb_name, header_date_month) |> 
    select(Month = header_date_month, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           Variable = variable,
           `Submission Status` = submission_status, 
           PMS = pms,
           `DQ Assessment` = value,
           Count = count, -total, -proportion) |> 
    group_by(Month, Dataset, `Health Board`, Variable, `Submission Status`, PMS, `DQ Assessment`) |>     # added in to prevent pivoted table generating a list
    summarise(
      n_non_missing = sum(!is.na(Count)),
      Count = sum(Count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(Count = ifelse(n_non_missing == 0, NA_real_, Count)) |>
    select(-n_non_missing) |>
    mutate(Month = format(parse_date_time(Month, orders = c("ymd", "Y-m", "b-Y")), "%b-%Y")) |>
    pivot_wider(names_from = Month, values_from = Count) |>
    mutate(Measurement = "Count") |>
    relocate(Measurement, .after = `DQ Assessment`) 
  df_trend2_count <- df_trend2_count |> mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .))) 
  
  create_error_logs(df_trend2_count)
  
  # moving range  (current minus previous)
  df_trend2_moving_range <- df_trend2_count
  df_trend2_moving_range[, 8] <- NA                                                        # first month in the block has no prior value, so made NA
  for (j in 9:22) {
    df_trend2_moving_range[, j] <- abs(df_trend2_count[, j] - df_trend2_count[, j - 1])    # Replace each later month with the absolute difference from the previous month
  }
  df_trend2_moving_range$Measurement <- "Moving Range"                                     # Update Measurement
  
  # percentage change (current minus previous, divided by previous * 100)
  df_trend2_pct_change <- df_trend2_count
  
  # first month in the block has no prior value
  df_trend2_pct_change[, 8] <- NA_real_
  
  for (j in 9:22) {
    prev <- df_trend2_count[[j - 1]]
    curr <- df_trend2_count[[j]]
    
    df_trend2_pct_change[[j]] <- dplyr::case_when(
      is.na(prev) | is.na(curr) ~ NA_real_,
      prev == 0 & curr == 0     ~ 0,
      prev == 0 & curr != 0     ~ NA_real_,
      TRUE                      ~ round((abs(curr - prev) / prev) * 100, 1)
    )
  }
  
  df_trend2_pct_change$Measurement <- "Percentage Difference"
  
  # trend dates - variable is used in writing the data
  
  df_trend2_dates <- data.frame(dates = df_trend2 |> select(8:22) |> colnames()) |> 
    pivot_wider(names_from = dates, values_from = dates) 
  
  # export(df_trend2, 
  #        file = paste0(pre_shorewise_output_dir, "/02_data_quality/captnd_dq_trend_summary.xlsx"),
  #        format = "xlsx")
  
  trend_row_alt <- nrow(df_trend2)+1
  
  deleteData(wb, sheet = "Trend Data - Alt", cols = 2:22, rows = 1:trend_row_alt*2, gridExpand = TRUE)
  
  writeData(wb, sheet = "Trend Data - Alt",
            x = df_trend2,
            startCol = 2, startRow = 1, #headerStyle = style_text,
            colNames = TRUE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  writeData(wb, sheet = "Trend Data - Alt",
            x = df_trend2_count,
            startCol = 2, startRow = 6274, #headerStyle = style_text,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  writeData(wb, sheet = "Trend Data - Alt",
            x = df_trend2_moving_range,
            startCol = 2, startRow = 12546, #headerStyle = style_text,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  writeData(wb, sheet = "Trend Data - Alt",
            x = df_trend2_pct_change,
            startCol = 2, startRow = 18818, #headerStyle = style_text,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  writeData(wb, sheet = "DQ Trend - Alt",
            x = df_trend2_dates,
            startCol = 4, startRow = 17, headerStyle = style_header,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  writeData(wb, sheet = "DQ Trend - Changes",
            x = df_trend2_dates,
            startCol = 4, startRow = 17, headerStyle = style_header,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
 
  
  # update references
  vec_hb <- unique(df_trend$`Health Board`)
  writeData(wb, sheet = "Refs", 
            x = vec_hb,  
            startCol = 3, startRow = 3, #headerStyle = style_text, 
            colNames = FALSE, withFilter = FALSE)
  
  vec_vars <- unique(df_trend$Variable)
  writeData(wb, sheet = "Refs", 
            x = vec_vars,  
            startCol = 4, startRow = 3, #headerStyle = style_text, 
            colNames = FALSE, withFilter = FALSE)
  
  # save updates to GE
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  assign(x = "trend_row", value = trend_row, envir = .GlobalEnv)
  assign(x = "trend_row_alt", value = trend_row_alt, envir = .GlobalEnv)
  
  return(wb)
  
}

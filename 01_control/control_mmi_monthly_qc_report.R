library(readxl)
library(dplyr)
library(lubridate)
library(arrow)
library(openxlsx)

#### 1.0 ####
# =========================
# USER INPUT
# =========================
run_header_date_input <- "2026-01-01"   # YYYY-MM-DD, must be first day of month


#
run_header_date <- as.Date(run_header_date_input)

if (is.na(run_header_date)) {
  stop("run_header_date_input must be a valid date in YYYY-MM-DD format.")
}

if (format(run_header_date, "%d") != "01") {
  stop("run_header_date_input must be the first day of a month (DD = 01).")
}

previous_header_date <- run_header_date %m-% months(1)

#### 1.1 ####
# =========================
# PATHS
# =========================
source_folder <- "//PHI_conf/MentalHealth5/CAPTND/mmi_report"
qc_folder <- file.path(source_folder, "MMI_QC")
dir.create(qc_folder, showWarnings = FALSE, recursive = TRUE)

template_file <- file.path("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/report_templates/mmi/MMI_QC_TEMPLATE_001.xlsx")

camhs_file <- file.path(
  source_folder,
  paste0("mmi_data_tables_CAMHS_", format(run_header_date, "%Y-%m-%d"), ".xlsx")
)

pt_file <- file.path(
  source_folder,
  paste0("mmi_data_tables_PT_", format(run_header_date, "%Y-%m-%d"), ".xlsx")
)

previous_rolling_history_file <- file.path(
  qc_folder,
  paste0("MMI_QC_rolling_history_", format(previous_header_date, "%Y-%m-%d"), ".parquet")
)

current_rolling_history_parquet_file <- file.path(
  qc_folder,
  paste0("MMI_QC_rolling_history_", format(run_header_date, "%Y-%m-%d"), ".parquet")
)

previous_qc_workbook_file <- file.path(
  qc_folder,
  paste0("MMI_QC_", format(previous_header_date, "%Y-%m-%d"), ".xlsx")
)

current_qc_workbook_file <- file.path(
  qc_folder,
  paste0("MMI_QC_", format(run_header_date, "%Y-%m-%d"), ".xlsx")
)

#### 1.2 ####
# =========================
# HELPERS
# =========================
parse_mixed_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  
  out <- as.Date(x, format = "%d/%m/%Y")
  still_na <- is.na(out) & !is.na(x)
  out[still_na] <- as.Date(x[still_na], format = "%Y-%m-%d")
  
  still_na <- is.na(out) & !is.na(x)
  suppressWarnings(
    out[still_na] <- as.Date(as.numeric(x[still_na]), origin = "1899-12-30")
  )
  
  out
}

standardise_hb_name <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("NHSScotland", "NHS Scotland")] <- "NHS Scotland"
  x
}

normalise_empty_strings <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA_character_
    }
    x
  })
  df
}

first_non_empty_comment <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

ensure_required_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df
}

empty_history_df <- function() {
  tibble(
    header_date = as.Date(character()),
    previous_header_date = as.Date(character()),
    group_key = character(),
    month_ending = as.Date(character()),
    dataset_type = character(),
    hb_name = character(),
    total = numeric(),
    previous_total = numeric(),
    total_delta = numeric(),
    type = character(),
    analyst_comments = character()
  )
}

standardise_history_schema <- function(df) {
  df <- as_tibble(df)
  
  required_cols <- c(
    "header_date",
    "previous_header_date",
    "group_key",
    "month_ending",
    "dataset_type",
    "hb_name",
    "total",
    "previous_total",
    "total_delta",
    "type",
    "analyst_comments"
  )
  
  df <- ensure_required_cols(df, required_cols)
  
  df %>%
    mutate(
      header_date = parse_mixed_date(header_date),
      previous_header_date = parse_mixed_date(previous_header_date),
      month_ending = parse_mixed_date(month_ending),
      dataset_type = as.character(dataset_type),
      hb_name = as.character(hb_name),
      group_key = as.character(group_key),
      total = suppressWarnings(as.numeric(total)),
      previous_total = suppressWarnings(as.numeric(previous_total)),
      total_delta = suppressWarnings(as.numeric(total_delta)),
      type = as.character(type),
      analyst_comments = as.character(analyst_comments)
    ) %>%
    select(all_of(required_cols))
}

read_previous_comments_from_qc_workbook <- function(xlsx_path) {
  if (!file.exists(xlsx_path)) {
    return(NULL)
  }
  
  sheets <- excel_sheets(xlsx_path)
  comments_sheet_name <- "analyst_comments"
  
  if (!comments_sheet_name %in% sheets) {
    return(NULL)
  }
  
  out <- read_excel(
    xlsx_path,
    sheet = comments_sheet_name,
    guess_max = 10000,
    .name_repair = "unique"
  )
  
  out <- as_tibble(out)
  out <- normalise_empty_strings(out)
  
  if (nrow(out) == 0) {
    return(NULL)
  }
  
  out <- standardise_history_schema(out)
  
  out %>%
    filter(!is.na(analyst_comments) & trimws(analyst_comments) != "") %>%
    select(type, group_key, analyst_comments) %>%
    distinct()
}

read_previous_history <- function(parquet_path, qc_workbook_path = NULL) {
  if (!file.exists(parquet_path)) {
    warning(
      paste0(
        "Previous rolling history parquet not found: ",
        parquet_path,
        "\nStarting from current month only."
      )
    )
    return(empty_history_df())
  }
  
  prev <- read_parquet(parquet_path)
  prev <- standardise_history_schema(prev)
  
  prev_comments <- read_previous_comments_from_qc_workbook(qc_workbook_path)
  
  if (!is.null(prev_comments) && nrow(prev_comments) > 0) {
    prev <- prev %>%
      left_join(
        prev_comments %>% rename(comment_from_xlsx = analyst_comments),
        by = c("type", "group_key")
      ) %>%
      mutate(
        analyst_comments = if_else(
          (is.na(analyst_comments) | trimws(analyst_comments) == "") &
            !is.na(comment_from_xlsx) &
            trimws(comment_from_xlsx) != "",
          comment_from_xlsx,
          analyst_comments
        )
      ) %>%
      select(-comment_from_xlsx)
  }
  
  prev
}

summarise_tab1 <- function(df, file_date) {
  df <- normalise_empty_strings(df)
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  
  if ("month_ending" %in% names(df)) {
    df$month_ending <- parse_mixed_date(df$month_ending)
  }
  
  if ("hb_name" %in% names(df)) {
    df$hb_name <- standardise_hb_name(df$hb_name)
  }
  
  if ("dataset_type" %in% names(df)) {
    df$dataset_type <- trimws(as.character(df$dataset_type))
  }
  
  if ("total" %in% names(df)) {
    df$total <- suppressWarnings(as.numeric(df$total))
  } else {
    stop("Tab 1 Data is missing required column: total")
  }
  
  if (!"analyst_comments" %in% names(df)) {
    df$analyst_comments <- NA_character_
  }
  
  df %>%
    filter(!is.na(month_ending)) %>%
    mutate(
      header_date = file_date,
      previous_header_date = file_date %m-% months(1),
      type = "referrals",
      group_key = paste(
        dataset_type,
        hb_name,
        month_ending,
        header_date,
        previous_header_date,
        type,
        sep = "|"
      )
    ) %>%
    group_by(group_key) %>%
    summarise(
      header_date = first(header_date),
      previous_header_date = first(previous_header_date),
      month_ending = first(month_ending),
      dataset_type = first(dataset_type),
      hb_name = first(hb_name),
      total = first(total),
      type = first(type),
      analyst_comments = first_non_empty_comment(analyst_comments),
      .groups = "drop"
    ) %>%
    select(
      header_date,
      previous_header_date,
      month_ending,
      dataset_type,
      hb_name,
      total,
      type,
      analyst_comments
    )
}

summarise_tab7 <- function(df, file_date) {
  df <- normalise_empty_strings(df)
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  
  if ("month_ending" %in% names(df)) {
    df$month_ending <- parse_mixed_date(df$month_ending)
  }
  
  if ("hb_name" %in% names(df)) {
    df$hb_name <- standardise_hb_name(df$hb_name)
  }
  
  if ("dataset_type" %in% names(df)) {
    df$dataset_type <- trimws(as.character(df$dataset_type))
  }
  
  if ("total_apps" %in% names(df)) {
    df$total_apps <- suppressWarnings(as.numeric(df$total_apps))
  } else {
    stop("Tab 7/8 Data is missing required column: total_apps")
  }
  
  if (!"analyst_comments" %in% names(df)) {
    df$analyst_comments <- NA_character_
  }
  
  df %>%
    filter(!is.na(month_ending)) %>%
    mutate(
      header_date = file_date,
      previous_header_date = file_date %m-% months(1),
      type = "appointments",
      group_key = paste(
        dataset_type,
        hb_name,
        month_ending,
        header_date,
        previous_header_date,
        type,
        sep = "|"
      )
    ) %>%
    group_by(group_key) %>%
    summarise(
      header_date = first(header_date),
      previous_header_date = first(previous_header_date),
      month_ending = first(month_ending),
      dataset_type = first(dataset_type),
      hb_name = first(hb_name),
      total = first(total_apps),
      type = first(type),
      analyst_comments = first_non_empty_comment(analyst_comments),
      .groups = "drop"
    ) %>%
    select(
      header_date,
      previous_header_date,
      month_ending,
      dataset_type,
      hb_name,
      total,
      type,
      analyst_comments
    )
}

read_current_workbook <- function(file_path, dataset_label, file_date) {
  if (!file.exists(file_path)) {
    stop("Current source file not found: ", file_path)
  }
  
  sheets <- excel_sheets(file_path)
  out_list <- list()
  
  tab1 <- sheets[trimws(sheets) == "Tab 1 Data"]
  if (length(tab1) > 0) {
    df1 <- read_excel(
      path = file_path,
      sheet = tab1[1],
      guess_max = 10000,
      .name_repair = "unique"
    )
    
    df1 <- as_tibble(df1)
    
    if (dataset_label == "CAMHS" && "hb_name" %in% names(df1)) {
      df1 <- df1[!(tolower(trimws(df1$hb_name)) == "nhs 24"), , drop = FALSE]
    }
    
    out_list[["tab1"]] <- summarise_tab1(df1, file_date)
  }
  
  # Special case: 2025-07-01 uses Tab8 Data instead of Tab 7 Data
  tab7_name <- if (identical(file_date, as.Date("2025-07-01"))) "Tab 8 Data" else "Tab 7 Data"
  tab7 <- sheets[trimws(sheets) == tab7_name]
  
  if (length(tab7) > 0) {
    df7 <- read_excel(
      path = file_path,
      sheet = tab7[1],
      guess_max = 10000,
      .name_repair = "unique"
    )
    
    df7 <- as_tibble(df7)
    
    if (dataset_label == "CAMHS" && "hb_name" %in% names(df7)) {
      df7 <- df7[!(tolower(trimws(df7$hb_name)) == "nhs 24"), , drop = FALSE]
    }
    
    out_list[["tab7"]] <- summarise_tab7(df7, file_date)
  }
  
  if (length(out_list) == 0) {
    return(empty_history_df()[0, ])
  }
  
  bind_rows(out_list)
}

build_current_month_rows <- function(run_header_date, previous_history, current_rows) {
  previous_lookup <- previous_history %>%
    filter(header_date == (run_header_date %m-% months(1))) %>%
    select(
      type,
      month_ending,
      dataset_type,
      hb_name,
      previous_total_lookup = total
    ) %>%
    distinct()
  
  current_rows %>%
    left_join(
      previous_lookup,
      by = c("type", "month_ending", "dataset_type", "hb_name")
    ) %>%
    mutate(
      previous_total = previous_total_lookup,
      total_delta = total - previous_total,
      group_key = paste(
        dataset_type,
        hb_name,
        month_ending,
        header_date,
        previous_header_date,
        type,
        sep = "|"
      )
    ) %>%
    select(
      header_date,
      previous_header_date,
      group_key,
      month_ending,
      dataset_type,
      hb_name,
      total,
      previous_total,
      total_delta,
      type,
      analyst_comments
    ) %>%
    distinct(group_key, .keep_all = TRUE)
}

write_qc_workbook_from_template <- function(template_path, rolling_history_df, current_month_comments_df, output_path) {
  if (!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }
  
  wb <- loadWorkbook(template_path)
  
  if ("rolling_history" %in% names(wb)) {
    removeWorksheet(wb, "rolling_history")
  }
  addWorksheet(wb, "rolling_history", gridLines = TRUE)
  writeData(wb, "rolling_history", rolling_history_df, withFilter = TRUE)
  freezePane(wb, "rolling_history", firstRow = TRUE)
  
  if ("analyst_comments" %in% names(wb)) {
    removeWorksheet(wb, "analyst_comments")
  }
  addWorksheet(wb, "analyst_comments", gridLines = TRUE)
  writeData(wb, "analyst_comments", current_month_comments_df, withFilter = TRUE)
  freezePane(wb, "analyst_comments", firstRow = TRUE)
  
  saveWorkbook(wb, output_path, overwrite = TRUE)
}
#### 1.3 ####
# =========================
# READ PREVIOUS ROLLING HISTORY + COMMENTS
# =========================
previous_history <- read_previous_history(
  parquet_path = previous_rolling_history_file,
  qc_workbook_path = previous_qc_workbook_file
)
#### 1.4 ####
# =========================
# READ CURRENT MONTH SOURCE FILES
# =========================
current_camhs <- read_current_workbook(
  file_path = camhs_file,
  dataset_label = "CAMHS",
  file_date = run_header_date
)

current_pt <- read_current_workbook(
  file_path = pt_file,
  dataset_label = "PT",
  file_date = run_header_date
)

current_raw_rows <- bind_rows(current_camhs, current_pt)

if (nrow(current_raw_rows) == 0) {
  stop("No usable rows were found in the current month source files.")
}
#### 1.5 ####
# =========================
# BUILD CURRENT MONTH ROWS
# =========================
current_month_rows <- build_current_month_rows(
  run_header_date = run_header_date,
  previous_history = previous_history,
  current_rows = current_raw_rows
)
#### 1.6 ####
# =========================
# UPDATE ROLLING HISTORY
# =========================
rolling_history <- previous_history %>%
  filter(header_date != run_header_date) %>%
  bind_rows(current_month_rows) %>%
  arrange(header_date, type, dataset_type, hb_name, month_ending)

rolling_history <- standardise_history_schema(rolling_history)

#### 1.7 ####
# =========================
# CURRENT MONTH COMMENTS FOR OUTPUT
# =========================
current_month_comments <- rolling_history %>%
  filter(header_date == run_header_date) %>%
  arrange(type, dataset_type, hb_name, month_ending)

#### 1.8 ####
# =========================
# SAVE OUTPUTS
# =========================
write_parquet(rolling_history, current_rolling_history_parquet_file)

write_qc_workbook_from_template(
  template_path = template_file,
  rolling_history_df = rolling_history,
  current_month_comments_df = current_month_comments,
  output_path = current_qc_workbook_file
)

cat("\nRolling history parquet created:\n")
print(current_rolling_history_parquet_file)

cat("\nQC workbook created from template:\n")
print(current_qc_workbook_file)




##### PART 2 - only use if need to recreate the files from Scratch ####
# takes every excel MMI file in the folder MMI folder path & creates a QC parquet rolling month & xlxs file
# library(readxl) 
# library(dplyr)
# library(lubridate)
# library(arrow)
# library(openxlsx)
# 
# # =========================
# # PATHS
# # =========================
# source_folder <- "//PHI_conf/MentalHealth5/CAPTND/mmi_report"
# qc_folder <- file.path(source_folder, "MMI_QC")
# dir.create(qc_folder, showWarnings = FALSE, recursive = TRUE)
# 
# # =========================
# # HELPERS
# # =========================
# extract_dataset <- function(file_name) {
#   sub("^mmi_data_tables_([A-Za-z]+)_\\d{4}-\\d{2}-\\d{2}\\.xlsx$", "\\1", file_name)
# }
# 
# extract_file_date <- function(file_name) {
#   sub("^mmi_data_tables_[A-Za-z]+_(\\d{4}-\\d{2}-\\d{2})\\.xlsx$", "\\1", file_name)
# }
# 
# parse_mixed_date <- function(x) {
#   if (inherits(x, "Date")) return(x)
#   if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
#   if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
#   
#   x <- trimws(as.character(x))
#   x[x == ""] <- NA
#   
#   out <- as.Date(x, format = "%d/%m/%Y")
#   still_na <- is.na(out) & !is.na(x)
#   out[still_na] <- as.Date(x[still_na], format = "%Y-%m-%d")
#   
#   still_na <- is.na(out) & !is.na(x)
#   suppressWarnings(
#     out[still_na] <- as.Date(as.numeric(x[still_na]), origin = "1899-12-30")
#   )
#   
#   out
# }
# 
# standardise_hb_name <- function(x) {
#   x <- trimws(as.character(x))
#   x[x %in% c("NHSScotland", "NHS Scotland")] <- "NHS Scotland"
#   x
# }
# 
# normalise_empty_strings <- function(df) {
#   df[] <- lapply(df, function(x) {
#     if (is.character(x)) {
#       x <- trimws(x)
#       x[x == ""] <- NA_character_
#     }
#     x
#   })
#   df
# }
# 
# first_non_empty_comment <- function(x) {
#   x <- trimws(as.character(x))
#   x[x == ""] <- NA_character_
#   x <- x[!is.na(x)]
#   if (length(x) == 0) return(NA_character_)
#   x[1]
# }
# 
# ensure_required_cols <- function(df, cols) {
#   for (col in cols) {
#     if (!col %in% names(df)) {
#       df[[col]] <- NA
#     }
#   }
#   df
# }
# 
# empty_history_df <- function() {
#   tibble(
#     header_date = as.Date(character()),
#     previous_header_date = as.Date(character()),
#     group_key = character(),
#     month_ending = as.Date(character()),
#     dataset_type = character(),
#     hb_name = character(),
#     total = numeric(),
#     previous_total = numeric(),
#     total_delta = numeric(),
#     type = character(),
#     analyst_comments = character()
#   )
# }
# 
# standardise_history_schema <- function(df) {
#   df <- as_tibble(df)
#   
#   required_cols <- c(
#     "header_date",
#     "previous_header_date",
#     "group_key",
#     "month_ending",
#     "dataset_type",
#     "hb_name",
#     "total",
#     "previous_total",
#     "total_delta",
#     "type",
#     "analyst_comments"
#   )
#   
#   df <- ensure_required_cols(df, required_cols)
#   
#   df %>%
#     mutate(
#       header_date = parse_mixed_date(header_date),
#       previous_header_date = parse_mixed_date(previous_header_date),
#       month_ending = parse_mixed_date(month_ending),
#       dataset_type = as.character(dataset_type),
#       hb_name = as.character(hb_name),
#       group_key = as.character(group_key),
#       total = suppressWarnings(as.numeric(total)),
#       previous_total = suppressWarnings(as.numeric(previous_total)),
#       total_delta = suppressWarnings(as.numeric(total_delta)),
#       type = as.character(type),
#       analyst_comments = as.character(analyst_comments)
#     ) %>%
#     select(all_of(required_cols))
# }
# 
# summarise_tab1 <- function(df, file_date) {
#   df <- normalise_empty_strings(df)
#   df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
#   
#   if ("month_ending" %in% names(df)) {
#     df$month_ending <- parse_mixed_date(df$month_ending)
#   }
#   
#   if ("hb_name" %in% names(df)) {
#     df$hb_name <- standardise_hb_name(df$hb_name)
#   }
#   
#   if ("dataset_type" %in% names(df)) {
#     df$dataset_type <- trimws(as.character(df$dataset_type))
#   }
#   
#   if ("total" %in% names(df)) {
#     df$total <- suppressWarnings(as.numeric(df$total))
#   } else {
#     stop("Tab 1 Data is missing required column: total")
#   }
#   
#   if (!"analyst_comments" %in% names(df)) {
#     df$analyst_comments <- NA_character_
#   }
#   
#   # HB-level output: deduplicate on HB/month/dataset/header month, do not sum totals
#   df %>%
#     filter(!is.na(month_ending)) %>%
#     mutate(
#       header_date = file_date,
#       previous_header_date = file_date %m-% months(1),
#       type = "referrals",
#       group_key = paste(
#         dataset_type,
#         hb_name,
#         month_ending,
#         header_date,
#         previous_header_date,
#         type,
#         sep = "|"
#       )
#     ) %>%
#     group_by(group_key) %>%
#     summarise(
#       header_date = first(header_date),
#       previous_header_date = first(previous_header_date),
#       month_ending = first(month_ending),
#       dataset_type = first(dataset_type),
#       hb_name = first(hb_name),
#       total = first(total),
#       type = first(type),
#       analyst_comments = first_non_empty_comment(analyst_comments),
#       .groups = "drop"
#     ) %>%
#     select(
#       header_date,
#       previous_header_date,
#       month_ending,
#       dataset_type,
#       hb_name,
#       total,
#       type,
#       analyst_comments
#     )
# }
# 
# summarise_tab7 <- function(df, file_date) {
#   df <- normalise_empty_strings(df)
#   df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
#   
#   if ("month_ending" %in% names(df)) {
#     df$month_ending <- parse_mixed_date(df$month_ending)
#   }
#   
#   if ("hb_name" %in% names(df)) {
#     df$hb_name <- standardise_hb_name(df$hb_name)
#   }
#   
#   if ("dataset_type" %in% names(df)) {
#     df$dataset_type <- trimws(as.character(df$dataset_type))
#   }
#   
#   if ("total_apps" %in% names(df)) {
#     df$total_apps <- suppressWarnings(as.numeric(df$total_apps))
#   } else {
#     stop("Tab 7/8 Data is missing required column: total_apps")
#   }
#   
#   if (!"analyst_comments" %in% names(df)) {
#     df$analyst_comments <- NA_character_
#   }
#   
#   df %>%
#     filter(!is.na(month_ending)) %>%
#     mutate(
#       header_date = file_date,
#       previous_header_date = file_date %m-% months(1),
#       type = "appointments",
#       group_key = paste(
#         dataset_type,
#         hb_name,
#         month_ending,
#         header_date,
#         previous_header_date,
#         type,
#         sep = "|"
#       )
#     ) %>%
#     group_by(group_key) %>%
#     summarise(
#       header_date = first(header_date),
#       previous_header_date = first(previous_header_date),
#       month_ending = first(month_ending),
#       dataset_type = first(dataset_type),
#       hb_name = first(hb_name),
#       total = first(total_apps),
#       type = first(type),
#       analyst_comments = first_non_empty_comment(analyst_comments),
#       .groups = "drop"
#     ) %>%
#     select(
#       header_date,
#       previous_header_date,
#       month_ending,
#       dataset_type,
#       hb_name,
#       total,
#       type,
#       analyst_comments
#     )
# }
# 
# read_single_workbook <- function(file_path, dataset_label, file_date) {
#   if (!file.exists(file_path)) {
#     stop("Source file not found: ", file_path)
#   }
#   
#   sheets <- excel_sheets(file_path)
#   out_list <- list()
#   
#   tab1 <- sheets[trimws(sheets) == "Tab 1 Data"]
#   if (length(tab1) > 0) {
#     df1 <- read_excel(
#       path = file_path,
#       sheet = tab1[1],
#       guess_max = 10000,
#       .name_repair = "unique"
#     )
#     
#     df1 <- as_tibble(df1)
#     
#     if (dataset_label == "CAMHS" && "hb_name" %in% names(df1)) {
#       df1 <- df1[!(tolower(trimws(df1$hb_name)) == "nhs 24"), , drop = FALSE]
#     }
#     
#     out_list[["tab1"]] <- summarise_tab1(df1, file_date)
#   }
#   
#   # Special case: 2025-07-01 uses Tab8 Data instead of Tab 7 Data
#   tab7_name <- if (identical(file_date, as.Date("2025-07-01"))) "Tab 8 Data" else "Tab 7 Data"
#   tab7 <- sheets[trimws(sheets) == tab7_name]
#   
#   if (length(tab7) > 0) {
#     df7 <- read_excel(
#       path = file_path,
#       sheet = tab7[1],
#       guess_max = 10000,
#       .name_repair = "unique"
#     )
#     
#     df7 <- as_tibble(df7)
#     
#     if (dataset_label == "CAMHS" && "hb_name" %in% names(df7)) {
#       df7 <- df7[!(tolower(trimws(df7$hb_name)) == "nhs 24"), , drop = FALSE]
#     }
#     
#     out_list[["tab7"]] <- summarise_tab7(df7, file_date)
#   }
#   
#   if (length(out_list) == 0) {
#     return(empty_history_df()[0, ])
#   }
#   
#   bind_rows(out_list)
# }
# 
# write_qc_workbook <- function(rolling_history_df, current_month_comments_df, xlsx_path) {
#   wb <- createWorkbook()
#   
#   addWorksheet(wb, "rolling_history")
#   writeData(wb, "rolling_history", rolling_history_df, withFilter = TRUE)
#   freezePane(wb, "rolling_history", firstRow = TRUE)
#   
#   addWorksheet(wb, "analyst_comments")
#   writeData(wb, "analyst_comments", current_month_comments_df, withFilter = TRUE)
#   freezePane(wb, "analyst_comments", firstRow = TRUE)
#   
#   saveWorkbook(wb, xlsx_path, overwrite = TRUE)
# }
# 
# # =========================
# # FIND ALL SOURCE FILES
# # =========================
# excel_files <- list.files(
#   path = source_folder,
#   pattern = "^mmi_data_tables_(CAMHS|PT)_\\d{4}-\\d{2}-\\d{2}\\.xlsx$",
#   full.names = TRUE
# )
# 
# if (length(excel_files) == 0) {
#   stop("No source files found.")
# }
# 
# file_info <- tibble(
#   file_path = excel_files,
#   file_name = basename(excel_files),
#   dataset_label = extract_dataset(basename(excel_files)),
#   file_date = as.Date(extract_file_date(basename(excel_files)))
# ) %>%
#   filter(dataset_label %in% c("CAMHS", "PT")) %>%
#   arrange(file_date, dataset_label)
# 
# if (nrow(file_info) == 0) {
#   stop("No valid CAMHS/PT source files found.")
# }
# 
# latest_header_date <- max(file_info$file_date, na.rm = TRUE)
# 
# # =========================
# # READ ALL SOURCE FILES
# # =========================
# all_current_rows <- lapply(seq_len(nrow(file_info)), function(i) {
#   read_single_workbook(
#     file_path = file_info$file_path[i],
#     dataset_label = file_info$dataset_label[i],
#     file_date = file_info$file_date[i]
#   )
# })
# 
# all_current_rows <- bind_rows(all_current_rows)
# 
# if (nrow(all_current_rows) == 0) {
#   stop("No usable rows were found in the source files.")
# }
# 
# all_current_rows <- all_current_rows %>%
#   arrange(header_date, type, dataset_type, hb_name, month_ending)
# 
# # =========================
# # BUILD FULL ROLLING HISTORY
# # =========================
# lookup_history <- all_current_rows %>%
#   transmute(
#     type,
#     header_date,
#     month_ending,
#     dataset_type,
#     hb_name,
#     previous_total_lookup = total
#   ) %>%
#   distinct()
# 
# rolling_history <- all_current_rows %>%
#   left_join(
#     lookup_history,
#     by = c(
#       "type",
#       "previous_header_date" = "header_date",
#       "month_ending",
#       "dataset_type",
#       "hb_name"
#     )
#   ) %>%
#   mutate(
#     previous_total = previous_total_lookup,
#     total_delta = total - previous_total,
#     group_key = paste(
#       dataset_type,
#       hb_name,
#       month_ending,
#       header_date,
#       previous_header_date,
#       type,
#       sep = "|"
#     )
#   ) %>%
#   select(
#     header_date,
#     previous_header_date,
#     group_key,
#     month_ending,
#     dataset_type,
#     hb_name,
#     total,
#     previous_total,
#     total_delta,
#     type,
#     analyst_comments
#   ) %>%
#   distinct(group_key, .keep_all = TRUE) %>%
#   arrange(header_date, type, dataset_type, hb_name, month_ending)
# 
# rolling_history <- standardise_history_schema(rolling_history)
# 
# # =========================
# # BUILD CURRENT MONTH COMMENTS TAB
# # =========================
# current_month_comments <- rolling_history %>%
#   filter(header_date == latest_header_date) %>%
#   arrange(type, dataset_type, hb_name, month_ending)
# 
# # =========================
# # OUTPUT FILE PATHS
# # =========================
# rolling_history_parquet_file <- file.path(
#   qc_folder,
#   paste0("MMI_QC_rolling_history_", format(latest_header_date, "%Y-%m-%d"), ".parquet")
# )
# 
# qc_workbook_file <- file.path(
#   qc_folder,
#   paste0("MMI_QC_", format(latest_header_date, "%Y-%m-%d"), ".xlsx")
# )
# 
# # =========================
# # SAVE OUTPUTS
# # =========================
# write_parquet(rolling_history, rolling_history_parquet_file)
# 
# write_qc_workbook(
#   rolling_history_df = rolling_history,
#   current_month_comments_df = current_month_comments,
#   xlsx_path = qc_workbook_file
# )
# 
# cat("\nRolling history parquet created:\n")
# print(rolling_history_parquet_file)
# 
# cat("\nQC workbook created:\n")
# print(qc_workbook_file)
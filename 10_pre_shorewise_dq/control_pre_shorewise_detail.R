
#########################################################.
### Control pre-shorewise CAPTND processes and ouputs ###
#########################################################.

# Author: Charlie Smith
# Date: 2024-04-01

# Purpose: Refactor original CAPTND data pull, data quality and publication prep,
# and publication analyses using updated coding practices

# NB this is an updated version of the old (pre-shorewise) CAPTND processes and
# are distinct from the Shorewise methodology.



# Outline:
# 1. Pull, check, save data
# 2. Create dq reports


# 0 - Start timer ---------------------------------------------------------

tictoc::tic()

# 1 - Load function -------------------------------------------------------
source('02_setup/save_df_as_parquet.R') #initial data pull lines 27-47
source('02_setup/swift_column_renamer.R')
source('02_setup/null_to_na.R')
source('04_check_modify/correct_hb_names_simple.R')

source('10_pre_shorewise_dq/pull_captnd_from_db.R')
source('10_pre_shorewise_dq/set_preg_perinatal_stage.R')
source('10_pre_shorewise_dq/fix_dob_issue.R')
source('10_pre_shorewise_dq/format_dates.R')
source('10_pre_shorewise_dq/save_captnd_raw.R')
source('10_pre_shorewise_dq/save_captnd_checked.R')

source('10_pre_shorewise_dq/assess_variables_demo.R')
source('10_pre_shorewise_dq/assess_variables_ref.R')
#source('10_pre_shorewise_dq/assess_variables_ref2.R')
source('10_pre_shorewise_dq/assess_variables_apps.R')
source('10_pre_shorewise_dq/assess_variables_unav.R')
source('10_pre_shorewise_dq/assess_variables_diag.R')
source('10_pre_shorewise_dq/assess_variables_dis.R')
source('10_pre_shorewise_dq/assess_variables_global_imps.R')
source('10_pre_shorewise_dq/fix_treat_lead_zeros.R')
source('10_pre_shorewise_dq/create_error_logs.R')

source('./10_pre_shorewise_dq/dq_reporting/load_functions.R') #loads all functions

# 2 - Set constants -------------------------------------------------------

source("10_pre_shorewise_dq/dq_reporting/set_constants.R")            #set_constants - current variables - comment this out if replacing


message('Pulling the data and creating the report takes ~15 mins. Get the kettle on!')

# 3 - Pull, check, save output data ---------------------------------------

if( file.exists(paste0(data_prep_dir, '/captnd_raw.parquet')) != TRUE){

  df_captnd_raw <- pull_captnd_from_db() |>
    null_to_na() |>
    set_preg_perinatal_stage() |>
    fix_dob_issue() |>
    format_dates() |>
    correct_hb_names_simple() |>
    fix_treatment_leadings_zeros() |>
    save_captnd_raw() 
    
  rm(df_captnd_raw)
  gc()

}


# if( file.exists(paste0(data_prep_dir, "/assess_demo.parquet")) != TRUE |
#     file.exists(paste0(data_prep_dir, "/assess_refs.parquet")) != TRUE |
#     file.exists(paste0(data_prep_dir, "/assess_apps.parquet")) != TRUE |
#     file.exists(paste0(data_prep_dir, "/assess_unav.parquet")) != TRUE |
#     file.exists(paste0(data_prep_dir, "/assess_diag.parquet")) != TRUE |
#     file.exists(paste0(data_prep_dir, "/assess_dis.parquet")) != TRUE ){

  # for each stage: split into treatment stages and run checks
  df <- read_parquet(paste0(data_prep_dir, '/captnd_raw.parquet'))

    assess_variables_demo(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_demo'))
    assess_variables_ref(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_refs'))

    gc()
    

    gc()

    assess_variables_apps(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_apps'))
    assess_variables_unav(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_unav'))
    assess_variables_diag(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_diag'))
    assess_variables_global_impressions(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_glob_imps'))
    assess_variables_dis(df) |> save_as_parquet(paste0(data_prep_dir, '/assess_dis'))

    rm(df)
    gc()

  # combine and save
  df_captnd_checked <- rbind.fill(
    # df_checked_demo <-
    read_parquet(paste0(data_prep_dir, '/assess_demo.parquet')),
    # df_checked_ref <-
    read_parquet(paste0(data_prep_dir, '/assess_refs.parquet')),
    # df_checked_apps <-
    read_parquet(paste0(data_prep_dir, '/assess_apps.parquet')),
    # df_checked_unav <-
    read_parquet(paste0(data_prep_dir, '/assess_unav.parquet')),
    # df_checked_diag <-
    read_parquet(paste0(data_prep_dir, '/assess_diag.parquet')),

    read_parquet(paste0(data_prep_dir, '/assess_glob_imps.parquet')),
    # df_checked_dis <-
    read_parquet(paste0(data_prep_dir, '/assess_dis.parquet'))) |>
    save_captnd_checked() # "captnd_checked.parquet"


  rm(df_checked_demo,
    df_checked_ref,
    df_checked_apps,
    df_checked_unav,
    df_checked_diag,
    df_checked_dis,
    df_captnd_checked)

  gc()

#}



# 4 - Create DQ heatmap reports -------------------------------------------

suppressWarnings(source('./10_pre_shorewise_dq/dq_reporting/create_dq_report.R'))

  
# 4 Create Heatmaps by HB For last 15 months
  
  
  # create_heatmap_known_by_hb <- function(df, chart_value){
  #   
  #   df |>
  #     dplyr::filter(value == chart_value) |>
  #     dplyr::mutate(
  #       variable = forcats::fct_rev(variable),
  #       # make sure dates are ordered and display nicely
  #       header_date_month = as.Date(header_date_month),
  #       header_date_month_f = factor(
  #         format(header_date_month, "%b %Y"),
  #         levels = format(sort(unique(header_date_month)), "%b %Y")
  #       )
  #     ) |>
  #     ggplot2::ggplot(ggplot2::aes(x = header_date_month_f, y = variable, fill = prop_group)) +
  #     ggplot2::geom_tile(width = 1, height = 1, linewidth = .25, color = "black") +
  #     ggplot2::geom_text(ggplot2::aes(label = proportion), size = 2) +
  #     ggplot2::scale_fill_manual(values = colors, name = "Known\nRecords (%)", drop = FALSE) +
  #     ggplot2::scale_x_discrete(position = "top") +
  #     ggplot2::theme(
  #       axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 0),
  #       legend.key = ggplot2::element_rect(fill = "white", colour = "black"),
  #       plot.caption = ggplot2::element_text(hjust = 0),
  #       strip.text.y.right = ggplot2::element_text(angle = 0)
  #     ) +
  #     ggplot2::facet_grid(var_cat ~ dataset_type, scales = "free", space = "free") +
  #     ggplot2::labs(
  #       title = paste0("Proportion of 'known' records by month and dataset type (submissions)"),
  #       subtitle = "Known records = valid and meaningful values",
  #       x = NULL,
  #       y = "Variables",
  #       caption = "\n*'Supplementary info' refers to data items that will only apply to a limited number of submitted records."
  #     )
  # }
  # 
  # 
  # 
  # 
  # # Output directory (UNC path)
  # data_quality_report_dir <- "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/alan/alan_stuff/heatmaps_rtt"
  # # 
  # # if (!dir.exists(data_quality_report_dir)) {
  # #   dir.create(data_quality_report_dir, recursive = TRUE)
  # # }
  # 
  # # Get the 16 HB names
  # hb_list <- sort(unique(df_trend2_restructured$hb_name))
  # 
  # # Helper to make safe object/file names
  # sanitize_name <- function(x) {
  #   x <- tolower(x)
  #   x <- gsub("&", "and", x)
  #   x <- gsub("[^a-z0-9]+", "_", x)   # replace non-alnum with underscores
  #   x <- gsub("^_|_$", "", x)         # trim leading/trailing underscores
  #   x
  # }
  # 
  # for (hb in hb_list) {
  #   
  #   hb_chr <- as.character(hb)
  #   hb_safe <- sanitize_name(hb_chr)
  #   chart_obj_name <- paste0("chart_known_", hb_safe)
  #   
  #   p <- create_heatmap_known_by_hb(
  #     df = dplyr::filter(df_trend2_restructured, hb_name == hb_chr),
  #     chart_value = "known"
  #   )
  #   
  #   assign(chart_obj_name, p, envir = .GlobalEnv)
  #   
  #   out_file <- file.path(data_quality_report_dir, paste0(chart_obj_name, ".png"))
  #   
  #   ggplot2::ggsave(
  #     filename = out_file,
  #     plot     = p,
  #     device   = "png",
  #     width    = 17,
  #     height   = 10,
  #     units    = "in",
  #     dpi      = 300
  #   )
  # }

# 5 - Compare against old DQ report ---------------------------------------

# source('./10_pre_shorewise_dq/compare_old_new.R')


# 6 - End timer -----------------------------------------------------------

tictoc::toc()

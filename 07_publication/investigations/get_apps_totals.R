#########################################################.
#### APPOINTMENTs - NEW AND RETURN - for publication ####.
#########################################################.


# Author: Bex Madden
# Date: 2024-07-05


get_apps_totals <- function(){ # , dataset_choice
  
  measure_label <- "apps_total_nr_"
  
  # load data 
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  # get appointments df
  source("./07_publication/update_2024_06/functions/get_appointments_df.R")
  df_app <- get_appointments_df(df) 
  
  df_nr_app <- df_app |>
    filter(app_month %in% date_range) |>
    mutate(att_cat = as.factor(att_cat),
           att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
    Sex = case_when(
      sex_reported == 1 ~ 'Male',
      sex_reported == 2 ~ 'Female',
      sex_reported == 0 ~ 'Not known',
      sex_reported == 9 ~ 'Not specified', 
      TRUE ~ NA_character_)) 
  
 # 1. QUARTERLY ------------------------------------------------------------
  
  # present quarterly with proportions by new and return
  nr_all <- df_nr_app |> 
    group_by(dataset_type, hb_name, app_quarter_ending, att_cat) |>  
    summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(dataset_type, app_quarter_ending, att_cat) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>

    pivot_wider(names_from = att_cat, values_from = appointments) |> 
    adorn_totals("col") |>
    mutate(`Percent New` = round(New/Total*100, 1),
           `Percent Return` = round(Return/Total*100, 1),
           across(`Percent New`:`Percent Return`, ~paste0(., "%")),
           across(New:Total, ~prettyNum(., big.mark = ",")))  #|>       mutate(app_quarter_ending = format(as.Date(app_quarter_ending), "%b '%y"))#|>
    #filter(dataset_type == dataset_choice) |>
    #save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "qt")) #_", dataset_choice
    
  #present latest quarter for pdf
    nr_latest <- nr_all |> 
      filter(app_quarter_ending  == max(app_quarter_ending)) |> 
      select(-c("app_quarter_ending", "New", "Return")) |> 
      save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "last_qt")) # _", dataset_choice
    
    # 2. MONTHLY -----------------------------------------------------------
    
    # Total new vs return apps - for presenting in supplement
    
    nr_all_m <- df_nr_app |> 
      group_by(dataset_type, hb_name, app_month, att_cat) |>  
      summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
      group_by(dataset_type, app_month, att_cat)  %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |>
      pivot_wider(names_from = att_cat, values_from = appointments) |> 
      adorn_totals("col") |>
      mutate(app_month = as.Date(app_month, "%Y-&m-&d"),
             `Percent New` = round(New/Total*100, 1),
             `Percent Return` = round(Return/Total*100, 1),
             across(`Percent New`:`Percent Return`, ~paste0(., "%")),
             across(New:Total, ~prettyNum(., big.mark = ",")),
             hb_name = factor(hb_name, levels = level_order_hb)) |>
      arrange(dataset_type, hb_name, app_month) |>
      #filter(dataset_type == dataset_choice) |>
      save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth")) # _", dataset_choice
    
    
    # SEX - New vs Return Apps: by sex
    
    nr_sex_all_m <- df_nr_app |> 
      group_by(dataset_type, hb_name, app_month, att_cat, Sex) |>  
      summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
      group_by(dataset_type, app_month, att_cat, Sex) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |>
      # filter(dataset_type = dataset_choice) |>
      pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
      unnest(cols = everything()) |>
      adorn_totals("col") |>
      mutate(app_month = as.Date(app_month, format = "%Y-%m-%d"),
             `Percent New` = round(New/Total*100, 1),
             `Percent Return` = round(Return/Total*100, 1),
             across(`Percent New`:`Percent Return`, ~paste0(., "%")),
             across(New:Total, ~prettyNum(., big.mark = ",")),
             hb_name = factor(hb_name, levels = level_order_hb)) |>
      arrange(dataset_type, hb_name, app_month) |>
        # filter(dataset_type = dataset_choice) |>
      save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_sex")) # _", dataset_choice
    
    
    # AGE - New vs Return Apps: by age
    
    nr_age_all_m <- df_nr_app |> 
      group_by(dataset_type, hb_name, app_month, att_cat, age_group) |>  
      summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
      group_by(dataset_type, app_month, att_cat, age_group) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |>
      mutate(app_month = as.Date(app_month, format = "%Y-%m-%d"),
             hb_name = factor(hb_name, levels = level_order_hb)) |>
      arrange(dataset_type, hb_name, app_month, att_cat, readr::parse_number(age_group)) |>
      pivot_wider(names_from = att_cat, values_from = appointments) |> 
      adorn_totals("col") |>
      mutate(app_month = as.Date(app_month, format = "%Y-%m-%d"),
             Return = ifelse(is.na(Return), 0, Return),
             `Percent New` = round(New/Total*100, 1),
             `Percent Return` = round(Return/Total*100, 1),
             across(`Percent New`:`Percent Return`, ~paste0(., "%")),
             across(New:Total, ~prettyNum(., big.mark = ",")),
             hb_name = factor(hb_name, levels = level_order_hb)) |>
      # filter(dataset_type = dataset_choice) |>
      save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_age")) # _", dataset_choice
    
    
    # SIMD - New vs Return Apps: by SIMD 
    
    nr_simd_both_m <- df_nr_app |>
      group_by(dataset_type, hb_name, app_month, att_cat, simd2020_quintile) |>
      summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") |>
      group_by(dataset_type, app_month, att_cat, simd2020_quintile) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |>
      pivot_wider(names_from = att_cat, values_from = appointments) |>
      adorn_totals("col") |>
      mutate(hb_name = factor(hb_name, levels = level_order_hb),
             New = ifelse(is.na(New), 0, New),
             Return = ifelse(is.na(Return), 0, Return),
             `Percent New` = round(New/Total*100, 1),
             `Percent Return` = round(Return/Total*100, 1),
             across(`Percent New`:`Percent Return`, ~paste0(., "%")),
             app_month = as.Date(app_month, "%Y-%m-%d"),
             across(New:Total, ~prettyNum(., big.mark = ","))) |> 
      arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
      # filter(dataset_type = dataset_choice) |>
      save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_simd")) # _", dataset_choice
    
}

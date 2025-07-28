#########################################.
##### For publication - treatment 1 #####
#########################################.

# Author: Luke Taylor
# Date: 2025-07-23

summarise_treat_received <- function(){
  
  treat_dir <- paste0(shorewise_pub_data_dir, "/treatment_received/")
  dir.create(treat_dir)
  measure_label <- "treat_rec_"
  
  # get treatment/intervention received
  lookup_treat_rec <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Treatment_update") %>% 
    rename(treat_1 = Codes,
           treat_name = Values) |>
    select(1:2) |>
    mutate(treat_1 = str_remove(treat_1, "^0+"))
  
  # single row per intervention
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    add_sex_description() |> 
    tidy_age_group_order() |>
    remove_borders_int_refs() |>
    filter(!is.na(!!sym(treat_1_o))) |> 
    mutate(treat_start = case_when(!is.na(!!sym(treat_start_date_o)) ~ treat_start_date,
                                        is.na(!!sym(treat_start_date_o)) & !is.na(!!sym(app_date_o)) ~ app_date,
                                        is.na(!!sym(treat_start_date_o)) & is.na(!!sym(app_date_o)) & !is.na(!!sym(header_date_o)) ~ header_date,
                                        TRUE ~ NA_Date_)) |>
    filter(treat_start %in% date_range) |> 
    select(!!!syms(data_keys), treat_start, !!sym(treat_1_o)) |>
    distinct() |>
    mutate(treat_1 = str_remove(treat_1, "^0+")) |>
    left_join(lookup_treat_rec, by = 'treat_1') |>
    mutate(ref_quarter = ceiling_date(treat_start, unit = "quarter") - 1,
           ref_quarter_ending = floor_date(ref_quarter, unit = "month"))
  
  
  ##Treatment Received##
  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), treat_name) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), treat_name) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), treat_name, count) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), desc(count))
  
  
  #by quarter hb
  df_qr_hb <- df_single_row |> 
    filter(!!sym(dataset_type_o) == 'PT') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o), ref_quarter_ending) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(protection_o), ref_quarter_ending) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                                  !!sym(protection_o) == 2 ~ 'Yes',
                                  !!sym(protection_o) == 99 ~ 'Not known',
                                  is.na(!!sym(protection_o)) ~ 'Data missing')) |>
    select(ref_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           prot_label = factor(prot_label, levels = prot_order)) |> 
    arrange(!!sym(hb_name_o), ref_quarter_ending, prot_label) |> 
    save_as_parquet(path = paste0(ref_prot_dir, measure_label, "adult_qr_hb"))
  
  
  
  ##Child Protection Status##
  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df_single_row |> 
    filter(!!sym(dataset_type_o) == 'CAMHS') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(protection_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                                  !!sym(protection_o) == 2 ~ 'Yes',
                                  !!sym(protection_o) == 99 ~ 'Not known',
                                  is.na(!!sym(protection_o)) ~ 'Data missing')) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           prot_label = factor(prot_label, levels = prot_order)) |> 
    arrange(!!sym(hb_name_o), prot_label) |> 
    save_as_parquet(path = paste0(ref_prot_dir, measure_label, "child_all_hb"))
  
  
  #by quarter hb
  df_qr_hb <- df_single_row |> 
    filter(!!sym(dataset_type_o) == 'CAMHS') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o), ref_quarter_ending) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(protection_o), ref_quarter_ending) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                                  !!sym(protection_o) == 2 ~ 'Yes',
                                  !!sym(protection_o) == 99 ~ 'Not known',
                                  is.na(!!sym(protection_o)) ~ 'Data missing')) |>
    select(ref_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           prot_label = factor(prot_label, levels = prot_order)) |> 
    arrange(!!sym(hb_name_o), ref_quarter_ending, prot_label) |> 
    save_as_parquet(path = paste0(ref_prot_dir, measure_label, "child_qr_hb"))
  
}

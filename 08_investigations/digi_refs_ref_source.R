#############################################.
#### Digital Referrals - Referral Source ####.
#############################################.

# Author: Luke Taylor
# Date: 2024-07-21

 
  dig_refs_dir <- paste0(shorewise_pub_data_dir, "/digital_referrals/")
  dir.create(dig_refs_dir)
  measure_label <- "digi_refs_source"
  
  # get digital referrals source
  
  df_digi_source <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    filter(!!sym(referral_month_o) %in% date_range,
           !is.na(!!sym(act_code_sent_date_o))) |>
    select(all_of(data_keys), !!referral_month_o, !!ref_source_o) |> # need to account for multiples 
    lazy_dt() |> 
    group_by(across(all_of(c(!!hb_name_o, !!dataset_type_o, !!referral_month_o, !!ref_source_o)))) |> 
    summarise(n_digi_ref_by_source = n(), .groups = 'drop') |>
    distinct() |>
    ungroup() |> 
    as.data.frame() |> 
    mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(ref_quarter, unit = "month"))
  
  
  # lookup codes for care contact location
  ref_source_lookup <- read_xlsx("../../../data/captnd_codes_lookup.xlsx",sheet = 'Ref_Source') %>%
    select(ref_source = Code, ref_source_name = Ref_Source) |>
    mutate(ref_source_name = str_to_sentence(ref_source_name),
           ref_source_name = case_when(ref_source_name == 'Self referral (includes self, relations, friends and carers)' ~ 'Self referral',
                                       ref_source_name == 'A&e' ~ 'Accident & emergency',
                                       ref_source_name == 'Gp' ~ 'General practitioner',
                                       ref_source_name == 'Lac' ~ 'LAC',
                                       ref_source_name == 'Camhs team' ~ 'CAMHS team',
                                       ref_source_name == 'Camhs – out of area' ~ 'CAMHS - Out of area',
                                       ref_source_name == 'Nhs24' ~ 'NHS24',
                                       ref_source_name == 'Consultant outwith own hb' ~ 'Consultant outside own health board',
                                       TRUE ~ ref_source_name)) 
  
  df_digi_ref_source <- df_digi_source |> 
    left_join(ref_source_lookup, by = "ref_source") 
  
 
  # get total digi referrals for each time period --------------------------------------
  #all time
  df_digi_refs_all <- df_ref_source |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    summarise(total_digi_refs = sum(n_digi_ref_by_source), .groups = 'drop') |>
    group_by(!!sym(dataset_type_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    ungroup()
  
  #quarterly
  df_digi_refs_qt <- df_ref_source |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
    summarise(total_digi_refs = sum(n_digi_ref_by_source), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup()
  
  #monthly
  df_digi_refs_mth <- df_ref_source |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |>  
    summarise(total_digi_refs = sum(n_digi_ref_by_source), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), referral_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup()
  
  
  # overall ----------------------------------------------------------------
  # by hb 
  digi_refs_all <- df_digi_ref_source |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_source_name) |>
    summarise(count = sum(n_digi_ref_by_source), .groups = 'drop') |>
    ungroup() |> 
    group_by(!!sym(dataset_type_o), ref_source_name) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    left_join(df_digi_refs_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           prop = round(count/total_digi_refs*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    save_as_parquet(paste0(dig_refs_dir, measure_label, "all_hb"))
  
  # quarterly ----------------------------------------------------------------
  # by hb 
  digi_refs_qt <- df_digi_ref_source |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_source_name, app_quarter_ending) |>
    summarise(count = sum(n_digi_ref_by_source), .groups = 'drop') |>
    ungroup() |> 
    group_by(!!sym(dataset_type_o), ref_source_name, app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    left_join(df_digi_refs_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> # join in total appointment count in time period
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           prop = round(count/total_digi_refs*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, app_quarter_ending) |>
    save_as_parquet(paste0(dig_refs_dir, measure_label, "qt_hb"))
  
  # monthly ----------------------------------------------------------------
  # by hb 
  digi_refs_mth <- df_digi_ref_source |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_source_name, referral_month) |>
    summarise(count = sum(n_digi_ref_by_source), .groups = 'drop') |>
    ungroup() |> 
    group_by(!!sym(dataset_type_o), ref_source_name, referral_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    left_join(df_digi_refs_mth, by = c("dataset_type", "hb_name", "referral_month")) |> # join in total appointment count in time period
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           prop = round(count/total_digi_refs*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, referral_month) |>
    save_as_parquet(paste0(dig_refs_dir, measure_label, "mth_hb"))
  
#####################
#### URBAN RURAL ####
#####################
  
# Most common referral sources by urban rural classification
  
  df_digi_source_ur <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    filter(!!sym(referral_month_o) %in% date_range,
           !is.na(!!sym(act_code_sent_date_o))) |>
    select(!!!syms(data_keys), !!sym(referral_month_o), !!sym(ref_source_o), ur8_2022_name) |> # need to account for multiples 
    lazy_dt() |> 
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o), !!sym(referral_month_o), !!sym(ref_source_o), ur8_2022_name) |> 
    summarise(n_digi_ref_by_source = n(), .groups = 'drop') |>
    distinct() |>
    ungroup() |> 
    as.data.frame() |> 
    mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(ref_quarter, unit = "month"))  
  
  df_digi_source_ur <- df_digi_source_ur |> 
    left_join(ref_source_lookup, by = "ref_source") 
  
  #overall
  digi_refs_ur_all <- df_digi_source_ur |>
    group_by(ref_source_name, ur8_2022_name) |>
    summarise(count = sum(n_digi_ref_by_source), .groups = 'drop') |>
    ungroup() |> 
    group_by(ur8_2022_name) |>
    arrange(ur8_2022_name, desc(count)) |>
    filter(!is.na(ref_source_name),
           !is.na(ur8_2022_name)) |>
    mutate(tot_digi_refs = sum(count),
           prop = round(count/tot_digi_refs*100, 1)) |>
    slice_head(n = 5) |>
    ungroup()
    
    
  
  
  
  
  


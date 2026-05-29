
#######################################################################.
### Compare adjusted monthly patients waiting - aggregate vs captnd ###
#######################################################################.

# Author: Luke Taylor 
# Date: 2026-05-29

compare_adj_patients_waiting_monthly <- function() {
  
  
  getAggregatePatientsWaiting <- function(ds_type) {
    ptrn=paste0('PatientsWaiting_',ds_type,'_')
    
    #read all files that have patients seen
    aggregate_files =list.files(path = '../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                                pattern = ptrn,
                                full.names = FALSE)
    
    last_date_agg = gsub(ptrn, '', aggregate_files) %>% 
      gsub('.csv', '', .) %>% 
      as.Date(.) %>% 
      max(.) %>% 
      as.character(.)
    
    aggregate_data=read_csv_arrow(paste0('../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                                         ptrn,
                                         last_date_agg,
                                         '.csv')) %>% 
      filter(variables_mmi %in% c('0 to 18 weeks unadj Patients waiting',
                                  '19 to 35 weeks unadj Patients waiting',
                                  '36 to 52 weeks unadj Patients waiting',
                                  'Over 52 weeks unadj Patients waiting',
                                  'u_NumberOfPatientsWaiting0To18Weeks',
                                  'u_NumberOfPatientsWaiting19To35Weeks',
                                  'u_NumberOfPatientsWaiting36To52Weeks',
                                  'u_NumberOfPatientsWaitingOver52Weeks',
                                  '0 to 18 weeks adj Patients waiting',
                                  '19 to 35 weeks adj Patients waiting',
                                  '36 to 52 weeks adj Patients waiting',
                                  'Over 52 weeks adj Patients waiting',
                                  'a_NumberOfPatientsWaiting0To18Weeks',
                                  'a_NumberOfPatientsWaiting19To35Weeks',
                                  'a_NumberOfPatientsWaiting36To52Weeks',
                                  'a_NumberOfPatientsWaitingOver52Weeks'
                                  
      )) %>% 
      mutate(!!dataset_type_o := ds_type,
             waiting_period = case_when(variables_mmi=='0 to 18 weeks unadj Patients waiting' |
                                          variables_mmi== 'u_NumberOfPatientsWaiting0To18Weeks' ~ '0-18 weeks',
                                        variables_mmi=='19 to 35 weeks unadj Patients waiting' |
                                          variables_mmi== 'u_NumberOfPatientsWaiting19To35Weeks' ~ '19-35 weeks',
                                        variables_mmi=='36 to 52 weeks unadj Patients waiting' |
                                          variables_mmi== 'u_NumberOfPatientsWaiting36To52Weeks' ~ '36-52 weeks',
                                        variables_mmi=='Over 52 weeks unadj Patients waiting' |
                                          variables_mmi== 'u_NumberOfPatientsWaitingOver52Weeks' ~ 'Over 52 weeks',
                                        variables_mmi=='0 to 18 weeks adj Patients waiting' |
                                          variables_mmi== 'a_NumberOfPatientsWaiting0To18Weeks' ~ '0-18 weeks adj',
                                        variables_mmi=='19 to 35 weeks adj Patients waiting'|
                                          variables_mmi== 'a_NumberOfPatientsWaiting19To35Weeks' ~ '19-35 weeks adj',
                                        variables_mmi=='36 to 52 weeks adj Patients waiting' |
                                          variables_mmi== 'a_NumberOfPatientsWaiting36To52Weeks' ~ '36-52 weeks adj',
                                        variables_mmi=='Over 52 weeks adj Patients waiting' |
                                          variables_mmi== 'a_NumberOfPatientsWaitingOver52Weeks' ~ 'Over 52 weeks adj')) %>% 
      pivot_longer(starts_with('2'), names_to = 'month', values_to = 'n_aggregate') 
    
  }
  
  #Adjusted Waits
  aggregate_CAMHS_adj = getAggregatePatientsWaiting('CAMHS') |>
    filter(str_detect(waiting_period, " adj"))
  
  aggregate_PT_adj = getAggregatePatientsWaiting('PT') |>
    filter(str_detect(waiting_period, " adj"))
  
  aggregate_adj = bind_rows(aggregate_CAMHS_adj,aggregate_PT_adj) %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(month = as.Date(month),
           waiting_period = str_remove(waiting_period, " adj")) %>%
    correct_hb_names_simple()
  
  #Unadjusted Waits
  # aggregate_CAMHS_unadj = getAggregatePatientsWaiting('CAMHS') |>
  #   filter(!str_detect(waiting_period, " adj"))
  # 
  # aggregate_PT_unadj = getAggregatePatientsWaiting('PT') |>
  #   filter(!str_detect(waiting_period, " adj"))
  # 
  # aggregate_unadj = bind_rows(aggregate_CAMHS_unadj,aggregate_PT_unadj) %>% 
  #   select(-variables_mmi) %>% 
  #   rename(!!hb_name_o := HB_new,
  #          unadj_n_aggregate = n_aggregate) %>% 
  #   mutate(month = as.Date(month),
  #          waiting_period = str_remove(waiting_period, " unadj")) %>%
  #   correct_hb_names_simple()
  # 
  # 
  # aggregate = aggregate_adj |>
  #   left_join(aggregate_unadj, by = c('hb_name', 'dataset_type', 'waiting_period', 'month')) |>
  #   mutate(n_aggregate = case_when(hb_name == 'NHS Dumfries and Galloway' ~ adj_n_aggregate,
  #                                  TRUE ~ n_aggregate))
  
  
  # #adjust scotland total for D&G unadjusted total
  # aggregate <- aggregate |>
  #   filter(hb_name != 'NHS Scotland',
  #          hb_name != 'NHS24') |>
  #   pivot_longer(cols = c('n_aggregate', 'adj_n_aggregate'),
  #                names_to = 'adj_status',
  #                values_to = 'count') |>
  #   pivot_wider(names_from = 'waiting_period',
  #               values_from = 'count') |>
  #   group_by(month, dataset_type, adj_status) %>%
  #   bind_rows(summarise(.,
  #                       across(where(is.numeric), sum),
  #                       across(!!sym(hb_name_o), ~"NHS Scotland"),
  #                       .groups = "drop"))
  # 
  # #reformat before joining captnd data
  # aggregate <- aggregate |>
  #   pivot_longer(cols = c('0-18 weeks', '19-35 weeks', '36-52 weeks', 'Over 52 weeks'),
  #                names_to = 'waiting_period',
  #                values_to = 'count') |>
  #   pivot_wider(names_from = 'adj_status',
  #               values_from = 'count')
  
  df_waiting <- read_parquet(paste0(pat_waits_dir, "patients_waiting_adj_", month_end, ".parquet")) |>
    filter(sub_month_end %in% df_month_seq_end$sub_month_end) |>
    mutate(sub_month_start = floor_date(sub_month_end, unit = "month")) |>
    select(dataset_type, hb_name, ucpn, patient_id, sub_month_start, monthly_wait_adj)
  
  df_waiting <- df_waiting |>
    mutate(wait_group_adj = case_when(
      monthly_wait_adj >= 0 & monthly_wait_adj <= 18 ~ "wait_0_to_18_weeks",
      monthly_wait_adj > 18 & monthly_wait_adj <= 35 ~ "wait_19_to_35_weeks",
      monthly_wait_adj > 35 & monthly_wait_adj <= 52 ~ "wait_36_to_52_weeks",
      monthly_wait_adj > 52 ~ "over_52_weeks",
      TRUE ~ NA_character_),
      wait_group_adj = factor(wait_group_adj, levels = c("wait_0_to_18_weeks", "wait_19_to_35_weeks", 
                                                         "wait_36_to_52_weeks", "over_52_weeks")))
  
  
  sub_month_start_o <- "sub_month_start"
  wait_group_adj_o <- "wait_group_adj"
  
  #adj count
  all_waiting <- df_waiting |> 
    filter(sub_month_start %in% date_range) |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o, wait_group_adj_o))) |> 
    summarise(count = n()) |> 
    ungroup() |> 
    group_by(!!!syms(c(dataset_type_o, sub_month_start_o, wait_group_adj_o))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o))) |>
    mutate(waiting_total = sum(count)) 
  
  
  all_waiting <- all_waiting |>
    mutate(wait_group_adj = case_when(wait_group_adj == 'wait_0_to_18_weeks' ~ '0-18 weeks',
                                        wait_group_adj == 'wait_19_to_35_weeks' ~ '19-35 weeks',
                                        wait_group_adj == 'wait_36_to_52_weeks' ~ '36-52 weeks',
                                        wait_group_adj == 'over_52_weeks' ~ 'Over 52 weeks')) %>%
    select(-waiting_total) |>
    rename(month := sub_month_start,
           waiting_period := wait_group_adj,
           n_captnd := count)
  
  all_waiting = all_waiting %>%
    full_join(aggregate_adj,by = join_by(!!hb_name_o, !!dataset_type_o, month, waiting_period)) %>%  # full join so it doesn't just drop data... doesn't actually affect plots though
    mutate(captnd_perc_agg = round(n_captnd/n_aggregate*100, 1),
           measure = 'patient_waiting')
  
  save_as_parquet(df = all_waiting,
                  path = paste0(patients_waiting_dir, "/comp_data_adj_patients_waiting_monthly"))
}




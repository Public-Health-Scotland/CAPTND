#################################.
#### PATIENTS SEEN - for mmi ####.
#################################.

# Author: Bex Madden
# Date: 2024-09-19

summarise_patients_seen <- function(){
  
  # create files for saving outputs
  pat_seen_dir <- paste0(shorewise_pub_data_dir, "/patients_seen/")
  dir.create(pat_seen_dir)
  
  # measure labels
  measure_label <- "pat_seen_" # for file names
  
  # source rtt function
  source("./06_calculations/calculate_adjusted_rtt_waits.R")
  
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

  df_pat_seen <- calculate_adjusted_rtt_waits(df) # apply RTT calculation to latest version of df and save output
  save_as_parquet(df = df_pat_seen, path = paste0(pat_seen_dir, "patients_seen_total_df", month_end))
  #df_pat_seen <- read_parquet(paste0(pat_seen_dir, "patients_seen_total_df", month_end, ".parquet"))
  
# get notes and wait groups for adjusted and unadjusted rtt
  pat_seen_notes <- df_pat_seen |> 
    filter(!is.na(rtt_adj),
           !is.na(first_treat_app)) |> # ok to do?
    mutate(has_clock_reset = fcase(ref_rec_date != clock_start, TRUE, default = FALSE),
           has_rtt_adjustment = fcase(!is.na(rtt_adj) & rtt_unadj != rtt_adj, TRUE, default = FALSE),
           has_unavailability = fcase(unav_opti_total >= 1, TRUE, default = FALSE),
           
           adj_rtt_group = fcase(
             rtt_adj >= 0 & rtt_adj <= 126, "0 to 18 weeks",
             rtt_adj > 126 & rtt_adj <= 245, "19 to 35 weeks",
             rtt_adj > 245 & rtt_adj <= 364, "36 to 52 weeks",
             rtt_adj > 364, "Over 52 weeks",
             default = NA_character_
           ),
           unadj_rtt_group = fcase(
             rtt_unadj >= 0 & rtt_unadj <= 126, "0 to 18 weeks",
             rtt_unadj > 126 & rtt_unadj <= 245, "19 to 35 weeks",
             rtt_unadj > 245 & rtt_unadj <= 364, "36 to 52 weeks",
             rtt_unadj > 364, "Over 52 weeks",
             default = NA_character_),
           has_adj_in_diff_rtt_group = fcase(adj_rtt_group != unadj_rtt_group, TRUE, 
                                             default = FALSE),
           
           first_treat_month = floor_date(first_treat_app, unit = "month")) |> 
    filter(first_treat_month %in% date_range) |> 
    append_quarter_ending(date_col = "first_treat_app")
  
  
  
  #### ALL TIME ----------------------------------------------------------------
  
  all_pat_seen_summ_adj <- pat_seen_notes |> 
    group_by(dataset_type, hb_name, adj_rtt_group) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name) |> 
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_all"))
  
  all_pat_seen_summ_unadj <- pat_seen_notes |> 
    group_by(dataset_type, hb_name, unadj_rtt_group) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name) |> 
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_all"))
  
  #### ALL TIME BY DEMOGRAPHICS ------------------------------------------------
  
  #make df of demographics for joining to rtt df
  demo_df <- df |> 
    select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, sex_reported_o,
                     age_at_ref_rec_o, age_group_o, simd_quintile_o))) |> 
    lazy_dt() |> 
    group_by(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o))) |> 
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    add_sex_description()
  
  pat_seen_demo <- pat_seen_notes |> 
    left_join(demo_df, by = c("patient_id", "ucpn", "dataset_type", "hb_name"))
  
  
  # BY SEX
  
  all_pat_seen_summ_adj_sex <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, adj_rtt_group, sex_reported) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, sex_reported) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, sex_reported) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_all_sex"))
  
  all_pat_seen_summ_unadj_sex <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, sex_reported) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, sex_reported) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, sex_reported) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_all_sex"))
  
  
  # BY AGE GROUP
  
  all_pat_seen_summ_adj_age <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, adj_rtt_group, age_group) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, age_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, age_group) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_all_age"))
  
  all_pat_seen_summ_unadj_age <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, age_group) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, age_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, age_group) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_all_age"))
  
  
  # BY SIMD QUINTILE
  
  all_pat_seen_summ_adj_simd <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, adj_rtt_group, !!sym(simd_quintile_o)) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, !!sym(simd_quintile_o)) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_all_simd"))
  
  all_pat_seen_summ_unadj_simd <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, !!sym(simd_quintile_o)) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, !!sym(simd_quintile_o)) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_all_simd"))
  
  
  #### MONTHLY -----------------------------------------------------------------
  
  pat_seen_summ_adj <- pat_seen_notes |> 
    group_by(dataset_type, hb_name, adj_rtt_group, first_treat_month) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, first_treat_month) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month) |> 
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_mth")) # key output for mmi
  
  pat_seen_summ_unadj <- pat_seen_notes |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, first_treat_month) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, first_treat_month) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month) |> 
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_mth")) # key ouput for mmi

  
  #### MONTHLY BY DEMOGRAPHICS -------------------------------------------------
  
  #make df of demographics for joining to rtt df
  demo_df <- df |> 
    select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, sex_reported_o,
                     age_at_ref_rec_o, age_group_o, simd_quintile_o))) |> 
    lazy_dt() |> 
    group_by(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o))) |> 
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    add_sex_description()
  
  pat_seen_demo <- pat_seen_notes |> 
    left_join(demo_df, by = c("patient_id", "ucpn", "dataset_type", "hb_name"))
  
  
  # BY SEX
  
  pat_seen_summ_adj_sex <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, adj_rtt_group, first_treat_month, sex_reported) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, first_treat_month, sex_reported) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month, sex_reported) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_mth_sex"))
  
  pat_seen_summ_unadj_sex <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, first_treat_month, sex_reported) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, first_treat_month, sex_reported) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month, sex_reported) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_mth_sex"))
    
    
# BY AGE GROUP
  
  pat_seen_summ_adj_age <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, adj_rtt_group, first_treat_month, age_group) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, first_treat_month, age_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month, age_group) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
  save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_mth_age"))
  
  pat_seen_summ_unadj_age <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, first_treat_month, age_group) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, first_treat_month, age_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month, age_group) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
  save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_mth_age"))
  
  
  # BY SIMD QUINTILE
  
  pat_seen_summ_adj_simd <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, adj_rtt_group, first_treat_month, !!sym(simd_quintile_o)) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, first_treat_month, !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month, !!sym(simd_quintile_o)) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
  save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_mth_simd"))
  
  pat_seen_summ_unadj_simd <- pat_seen_demo |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, first_treat_month, !!sym(simd_quintile_o)) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, first_treat_month, !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, first_treat_month, !!sym(simd_quintile_o)) |> #adj_rtt_group
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
  save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_mth_simd"))
  
  
  #### QUARTERLY ---------------------------------------------------------------

  
  qt_pat_seen_summ_adj <- pat_seen_notes |> 
    group_by(dataset_type, hb_name, adj_rtt_group, quarter_ending) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, adj_rtt_group, quarter_ending) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, quarter_ending) |> 
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_qt"))
  
  qt_pat_seen_summ_unadj <- pat_seen_notes |> 
    group_by(dataset_type, hb_name, unadj_rtt_group, quarter_ending) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, unadj_rtt_group, quarter_ending) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(dataset_type, hb_name, quarter_ending) |> 
    mutate(total = sum(n),
           perc = round(n/total*100, 1)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_qt"))
  
  #### QUARTERLY BY DEMOGRAPHICS -----------------------------------------------
    
    # BY SEX
    
    qt_pat_seen_summ_adj_sex <- pat_seen_demo |> 
      group_by(dataset_type, hb_name, adj_rtt_group, quarter_ending, sex_reported) |> 
      summarise(n = n(), .groups = "drop") |> 
      group_by(dataset_type, adj_rtt_group, quarter_ending, sex_reported) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |> 
      group_by(dataset_type, hb_name, quarter_ending, sex_reported) |> #adj_rtt_group
      mutate(total = sum(n),
             perc = round(n/total*100, 1)) |> 
      ungroup() |> 
      save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_qt_sex"))
    
    qt_pat_seen_summ_unadj_sex <- pat_seen_demo |> 
      group_by(dataset_type, hb_name, unadj_rtt_group, quarter_ending, sex_reported) |> 
      summarise(n = n(), .groups = "drop") |> 
      group_by(dataset_type, unadj_rtt_group, quarter_ending, sex_reported) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |> 
      group_by(dataset_type, hb_name, quarter_ending, sex_reported) |> #adj_rtt_group
      mutate(total = sum(n),
             perc = round(n/total*100, 1)) |> 
      ungroup() |> 
      save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_qt_sex"))
    
    
    # BY AGE GROUP
    
    qt_pat_seen_summ_adj_age <- pat_seen_demo |> 
      group_by(dataset_type, hb_name, adj_rtt_group, quarter_ending, age_group) |> 
      summarise(n = n(), .groups = "drop") |> 
      group_by(dataset_type, adj_rtt_group, quarter_ending, age_group) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |> 
      group_by(dataset_type, hb_name, quarter_ending, age_group) |> #adj_rtt_group
      mutate(total = sum(n),
             perc = round(n/total*100, 1)) |> 
      ungroup() |> 
      save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_qt_age"))
    
    qt_pat_seen_summ_unadj_age <- pat_seen_demo |> 
      group_by(dataset_type, hb_name, unadj_rtt_group, quarter_ending, age_group) |> 
      summarise(n = n(), .groups = "drop") |> 
      group_by(dataset_type, unadj_rtt_group, quarter_ending, age_group) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |> 
      group_by(dataset_type, hb_name, quarter_ending, age_group) |> #adj_rtt_group
      mutate(total = sum(n),
             perc = round(n/total*100, 1)) |> 
      ungroup() |> 
      save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_qt_age"))
    
    
    # BY SIMD QUINTILE
    
    qt_pat_seen_summ_adj_simd <- pat_seen_demo |> 
      group_by(dataset_type, hb_name, adj_rtt_group, quarter_ending, !!sym(simd_quintile_o)) |> 
      summarise(n = n(), .groups = "drop") |> 
      group_by(dataset_type, adj_rtt_group, quarter_ending, !!sym(simd_quintile_o)) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |> 
      group_by(dataset_type, hb_name, quarter_ending, !!sym(simd_quintile_o)) |> #adj_rtt_group
      mutate(total = sum(n),
             perc = round(n/total*100, 1)) |> 
      ungroup() |> 
      save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_qt_simd"))
    
    qt_pat_seen_summ_unadj_simd <- pat_seen_demo |> 
      group_by(dataset_type, hb_name, unadj_rtt_group, quarter_ending, !!sym(simd_quintile_o)) |> 
      summarise(n = n(), .groups = "drop") |> 
      group_by(dataset_type, unadj_rtt_group, quarter_ending, !!sym(simd_quintile_o)) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |> 
      group_by(dataset_type, hb_name, quarter_ending, !!sym(simd_quintile_o)) |> #adj_rtt_group
      mutate(total = sum(n),
             perc = round(n/total*100, 1)) |> 
      ungroup() |> 
      save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_qt_simd"))
    
    
  }




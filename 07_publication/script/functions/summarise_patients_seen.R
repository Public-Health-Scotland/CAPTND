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
                                             default = FALSE))
  
  pat_seen_summ_adj <- pat_seen_notes |> 
    mutate(first_treat_month = floor_date(first_treat_app, unit = "month")) |> # should be first_treat_app but hadn't included it in rtt function
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
    filter(first_treat_month %in% date_range) |> 
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "adj_wait_grp_mth"))
  
  pat_seen_summ_unadj <- pat_seen_notes |> 
    mutate(first_treat_month = floor_date(first_treat_app, unit = "month")) |> # should be first_treat_app but hadn't included it in rtt function
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
    filter(first_treat_month %in% date_range) |>  
    save_as_parquet(path = paste0(pat_seen_dir, measure_label, "unadj_wait_grp_mth"))

  }




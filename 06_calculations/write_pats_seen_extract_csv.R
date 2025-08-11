###########################################.
### Create extract csv of Patients Seen ###
###########################################.

# Author: Luke Taylor
# Date: 2025-06-16

# Will write a csv file with all patients seen in the most recent month, based on month_end

write_pats_seen_extract <- function(HB, dataset_choice){
  
  pat_seen_dir <- paste0(shorewise_pub_data_dir, "/patients_seen/")
  dir.create(pat_seen_dir)

# measure labels
  measure_label <- "pat_seen_" # for file names

  df_pat_seen <- read_parquet(paste0(pat_seen_dir, "patients_seen_total_df_", month_end, ".parquet"))


  pat_seen_notes <- df_pat_seen |> 
    filter(!is.na(rtt_adj),
           !is.na(wait_end_date)) |> # ok to do?
    mutate(has_clock_reset = fcase(ref_rec_date_opti != clock_start, TRUE, default = FALSE),
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
         
           first_treat_month = floor_date(wait_end_date, unit = "month")) |> 
    filter(first_treat_month %in% date_range) |> 
    append_quarter_ending(date_col = "wait_end_date")


  df_month_hb <- pat_seen_notes |>
    filter(first_treat_month >= month_end,
           hb_name == HB,
           dataset_type == dataset_choice) |>
    select(!!!syms(data_keys), wait_end_date, adj_rtt_group, first_treat_month) |>
    write.xlsx(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/wl_extracts/",
                     HB, "_" , dataset_choice, "_", month_end, "_", "pats_seen_extract.xlsx"))

}

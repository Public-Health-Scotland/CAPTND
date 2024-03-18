
####################################################.
### Calculate monthly waits for patients waiting ###
####################################################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-03-07


calculate_pats_waiting_monthly <- function(df){
  
  # set date sequences
  start <- min(df$referral_month)
  end <- max(df$referral_month) 
  
  month_seq <- seq.Date(from = ymd(start), to = ymd(end), by = "month")
  df_month_seq_end <- data.frame(month_end = ceiling_date(month_seq, unit = "month")-1)
  
  month_range <- seq.Date(from = end-months(14), to = end, by = "month")
  
  # add month seq end to df 
  df_seq <- df %>% 
    select(header_date, file_id, dataset_type, hb_name, ucpn, patient_id, ref_rec_date, 
           ref_rej_date, first_treat_app, rtt_eval, case_closed_date) |> 
    arrange(header_date) |> 
    group_by(ucpn) |> 
    fill(first_treat_app, .direction = "downup") |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_end)
  
  # calculate waiting times ### RAW CODE ###
  df_waits <- df_seq |> 
    mutate(TreatmentDate_og = first_treat_app, 
           TreatmentDate = coalesce(first_treat_app, case_closed_date), #end_date probably a better name
           month_start = floor_date(month_end, unit = "month"),
           treat_month_end = as.Date(ceiling_date(TreatmentDate, unit = "month")-1),
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1),
    
           # add wait status
           wait_status = case_when(
             !is.na(ref_rej_date) & rej_month_end <= month_end ~ "rejected",
             ref_rec_date <= month_end & is.na(TreatmentDate) ~ "on list",
             treat_month_end == month_end ~ "tx Start",
             TreatmentDate > month_end & ref_rec_date < month_end ~ "on list",
             TRUE ~ NA_character_),
           
           # add wait time
           wait_wks = ifelse(wait_status == "on list", round((month_end-ref_rec_date)/7, 1), NA_real_),
           
           # add rtt status
           rtt_status = case_when(
             wait_status == "on list" & wait_wks <= 18 ~ "under_18_wks",
             wait_status == "on list" & wait_wks > 18 & wait_wks <= 52 ~ "19_to_52_wks",
             wait_status == "on list" & wait_wks >= 52 ~ "over_52_wks",
             TRUE ~ NA_character_),
           rtt_status = factor(rtt_status, levels = c("under_18_wks", "19_to_52_wks", "over_52_wks"))) |> 
    filter(!is.na(rtt_status))
  
  # table
  table_wait_summary <- df_waits |> 
    filter(month_start %in% month_range) |> 
    group_by(dataset_type, hb_name, month_start, rtt_status) |> 
    summarise(n = n()) |> 
    ungroup() |> 
    group_by(dataset_type, month_start, rtt_status) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) #|> 
    #filter(hb_name == "NHS Western Isles")
  
  
  
  
}



####################################################.
### Calculate monthly waits for patients waiting ###
####################################################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-03-07
# Edited: Luke Taylor 13/12/2024

#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# NB this is to calculate unadjusted waits for those waiting to start treatment

calculate_pats_waiting_monthly <- function(df){
  
  sub_month_end <- ymd(most_recent_month_in_data)
  sub_month_start <- ymd(most_recent_month_in_data) - months(14)
  
  month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
  df_month_seq_end <- data.frame(sub_month_end = ceiling_date(month_seq, unit = "month")-1) # month_last_day
  
  month_range <- seq.Date(from = sub_month_end - months(14), to = sub_month_end, by = "month")
  
  # add month seq end to df 
  df_single_row <- df |>
    select(!!!syms(c(header_date_o, file_id_o, dataset_type_o, hb_name_o, ucpn_o, 
                     patient_id_o, sex_reported_o,age_group_o, simd_quintile_o, 
                     ref_rec_date_o, ref_rej_date_o, app_date_o, first_treat_app_o, 
                     unav_date_start_o, unav_date_end_o, unav_days_no_o,
                     rtt_eval_o, case_closed_date_o, act_code_sent_date_o))) |>
    arrange(!!sym(header_date_o)) |> 
    group_by(across(all_of(data_keys))) |> 
    fill(!!sym(first_treat_app_o), .direction = "downup") |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_end)
  
  
  df_waits <- df_single_row |> 
    mutate(off_list_date = coalesce(!!sym(first_treat_app_o), !!sym(case_closed_date_o),
                                    !!sym(act_code_sent_date_o)),
           sub_month_start = floor_date(sub_month_end, unit = "month"),
           off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1), 
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1), 
           
           # add wait status
           wait_status = case_when(
             !is.na(ref_rej_date) & rej_month_end <= sub_month_end ~ "rejected",
             ref_rec_date <= sub_month_end & is.na(off_list_date) ~ "on list",
             off_list_month_end == sub_month_end ~ "tx Start",
             off_list_date > sub_month_end & ref_rec_date < sub_month_end ~ "on list",
             TRUE ~ NA_character_),
           
           # add wait time
           wait_days_unadj = ifelse(wait_status == "on list", round((sub_month_end-ref_rec_date), 1), NA_real_),
           wait_wks_unadj = ifelse(wait_status == "on list", round(wait_days_unadj/7, 1), NA_real_),
           
           # add rtt status
           wait_group_unadj = case_when(
             wait_status == "on list" & wait_wks_unadj >= 0 & wait_wks_unadj <= 18 ~ "wait_0_to_18_weeks",
             wait_status == "on list" & wait_wks_unadj > 18 & wait_wks_unadj <= 35 ~ "wait_19_to_35_weeks",
             wait_status == "on list" & wait_wks_unadj > 35 & wait_wks_unadj <= 52 ~ "wait_36_to_52_weeks",
             wait_status == "on list" & wait_wks_unadj > 52 ~ "over_52_weeks",
             TRUE ~ NA_character_),
           wait_group_unadj = factor(wait_group_unadj, levels = c("wait_0_to_18_weeks", "wait_19_to_35_weeks", 
                                                                  "wait_36_to_52_weeks", "over_52_weeks"))) |> 
    filter(!is.na(wait_group_unadj))
  
  
  # table
  
  sub_month_start_o <- "sub_month_start"
  wait_group_unadj_o <- "wait_group_unadj"
  
  # by hb and month
  table_wait_summary <- df_waits |> 
    filter(sub_month_start %in% month_range) |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o, wait_group_unadj_o))) |> 
    summarise(count = n()) |> 
    ungroup() |> 
    group_by(!!!syms(c(dataset_type_o, sub_month_start_o, wait_group_unadj_o))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop"))|> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o))|> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o))) |> 
    mutate(waiting_total = sum(count, na.rm = TRUE),
           waiting_prop = round(count / waiting_total *100, 1))
  
  # save df
  dir.create(paste0(patients_waiting_dir,"/by_month"))
  save_as_parquet(table_wait_summary, paste0(patients_waiting_dir,'/by_month/monthly_waits_patients_waiting_hb'))
  
  write_csv_arrow(table_wait_summary, paste0(patients_waiting_dir,'/nPatients_waiting_subSource_monthly.csv'))
  
  
  # create chart
  plot_patients_waiting_monthly <- function(table_wait_summary, ds_type) {
    
    p2 <- table_wait_summary %>% 
      filter(!!sym(dataset_type_o) == ds_type) %>% 
      ggplot( aes(x = sub_month_start, 
                  y = waiting_prop, 
                  group= wait_group_unadj, 
                  colour= wait_group_unadj,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Month: ", gsub('\n','-', sub_month_start), "<br>",
                    "Wait group: ", wait_group_unadj, "<br>",
                    "Count: ", count, "<br>",
                    "Proportion: ", waiting_prop
                  ))) +
      geom_line()+
      geom_point()+
      theme_minimal()+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#C73918"))+
      ylab("% Patients Waiting")+
      xlab("Month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b-%y",
        expand = c(0,0))+
      ylim(0, 100)+
      labs(title = paste0(ds_type, " Patients Waiting (Unadjusted)"),
           colour = "Wait Group")+
      theme(plot.title = element_text(hjust = 0.5, size = 30))+
      facet_wrap(~factor(hb_name, levels = c(level_order_hb)), scales = "free_y")+
      theme(panel.spacing.x= unit(0, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position = "right",
            axis.text.x = element_text(angle = -90, size = 13, hjust = 0, vjust = 0.5,
                                       margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size = 15),
            axis.title = element_text(size = 17),
            panel.grid.minor = element_blank(),
            legend.title = element_text(size = 17),
            legend.text = element_text(size = 13))
    
    fig2 = ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(patients_waiting_dir,
                    '/by_month/plot_patients_waiting_monthly_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  plot_patients_waiting_monthly(table_wait_summary, "CAMHS")
  plot_patients_waiting_monthly(table_wait_summary, "PT")
  
}




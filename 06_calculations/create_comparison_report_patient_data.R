
###############################################.
### Create comparison report - patient data ###
###############################################.

# Name: Charlie Smith
# Date: 2024-04-09

create_comparison_reports_patient_data <- function(){
  
  # patients data and records to find saved here:
  # comp_report_dir_patient_data
  
  # get records to find
  df_records_to_get <- read_parquet(paste0(comp_report_dir_patient_data, "/records_to_find.parquet")) 
  
  list_records_to_get <- df_records_to_get |> 
    group_split(measure) |> 
    setNames(unique(df_records_to_get$measure))
  
  

  # 1 - Referrals ----------------------------------------------------------
  list_bucket <- list()

  for(i in 1:nrow(list_records_to_get$referrals)){
    
    df_refs_specific <- df |>
      mutate(!!ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                                  !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                                  TRUE ~ 'pending')) |> 
      filter(
        if(list_records_to_get$referrals[[i, 2]] == 'total'){ # deal with lack of 'total' in df ref_acc_last_reported
          referral_month == list_records_to_get$referrals[[i, 5]] &
            hb_name == list_records_to_get$referrals[[i, 4]] &
            dataset_type == list_records_to_get$referrals[[i, 3]] 
        }else{
          referral_month == list_records_to_get$referrals[[i, 5]] &
            hb_name == list_records_to_get$referrals[[i, 4]] &
            dataset_type == list_records_to_get$referrals[[i, 3]] &
            ref_acc_last_reported == list_records_to_get$referrals[[i, 2]]} 
      ) |> 
      select(dataset_type, hb_name, ucpn, patient_id, referral_month, ref_acc_last_reported) |> 
      distinct() |> 
      mutate(label = if(list_records_to_get$referrals[[i, 2]] == 'total'){ # deal with lack of 'total' in df ref_acc_last_reported
        
        paste(list_records_to_get$referrals[[i, 4]], list_records_to_get$referrals[[i, 3]], "total", sep = "_")
        
      } else {
        
        paste(list_records_to_get$referrals[[i, 4]], list_records_to_get$referrals[[i, 3]], list_records_to_get$referrals[[i, 2]], sep = "_")
        
      }
               
    )
    
    list_bucket[[i]] <- df_refs_specific  
      
  }
  
  list_hb <- bind_rows(list_bucket) |> 
    group_split(hb_name) 
  
  library(writexl)
  
  for(i in 1:length(list_hb)){
  
    df_hb <- list_hb[[i]]
    
    df_split <- df_hb |> # split by label
      arrange(label) |> 
      group_by(label) |> 
      group_split() |>
      setNames(sort(unique(df_hb$label)))
    
    hb_name_no_space <- list_hb[[i]][[1,2]] |> # get hb name and format for filenames 
      tolower() |>
      str_replace_all(" ", "_")
    
    writexl::write_xlsx(df_split, paste0(comp_report_dir_patient_data, "/referrals_", hb_name_no_space,".xlsx")) # save each measure to separate tab in excel doc
  
  }
  

  # 2 - Patients Waiting - Seen ---------------------------------------------
  
  df_waits_seen <- read_parquet(paste0(comp_report_dir_patient_data, "/patients_seen.parquet"))
  
  list_bucket <- list()
  
  for(i in 1:nrow(list_records_to_get$waits_patients_seen)){
    
    df_rtt <- df_waits_seen |> 
      mutate(measure_type = case_when(
        waiting_time >= 0 & waiting_time <= 18 ~ "0-18 weeks",
        waiting_time >= 19 & waiting_time <= 35 ~ "19-35 weeks",
        waiting_time >= 36 & waiting_time <= 52 ~ "36-52 weeks",
        waiting_time > 52 ~ "53+ weeks",
        TRUE ~ "check")) |> 
      filter(
        dataset_type == list_records_to_get$waits_patients_seen[[i, 3]],
        hb_name == list_records_to_get$waits_patients_seen[[i, 4]],
        app_month == list_records_to_get$waits_patients_seen[[i, 5]],
        measure_type == list_records_to_get$waits_patients_seen[[i, 2]]) |> 
      distinct()
    
    list_bucket[[i]] <- df_rtt
  
  }
  
  list_hb <- bind_rows(list_bucket) |> 
    group_split(hb_name) 
  
  for(i in 1:length(list_hb)){
    
    df_hb <- list_hb[[i]]
    
    df_split <- df_hb |> # split by measure_type
      arrange(measure_type) |> 
      group_by(measure_type) |> 
      group_split() |>
      setNames(sort(unique(df_hb$measure_type)))
    
    hb_name_no_space <- list_hb[[i]][[1, 4]] |> # get hb name and format for filenames 
      tolower() |>
      str_replace_all(" ", "_")
    
    writexl::write_xlsx(df_split, paste0(comp_report_dir_patient_data, "/rtt_seen_", hb_name_no_space,".xlsx")) # save each measure to separate tab in excel doc
    
  }
  

# 3 - DNAs ----------------------------------------------------------------

  df_att_status <- read_parquet(paste0(comp_report_dir_patient_data, "/attendance_status.parquet")) |> 
    filter(att_status_desc == "DNA")
  
  list_bucket <- list()
  
  for(i in 1:nrow(list_records_to_get$dna)){
    
    df_dna <- df_att_status |> 
      filter(
        dataset_type == list_records_to_get$dna[[i, 3]],
        hb_name == list_records_to_get$dna[[i, 4]],
        app_month == list_records_to_get$dna[[i, 5]],
        att_cat == 1) |> 
      distinct()
    
    list_bucket[[i]] <- df_dna
    

  }
  
  list_hb <- bind_rows(list_bucket) |> 
    group_split(hb_name) 
  
  for(i in 1:length(list_hb)){
    
    df_hb <- list_hb[[i]]
    
    # df_split <- df_hb |> # split by measure_type
    #   arrange(measure_type) |> 
    #   group_by(measure_type) |> 
    #   group_split() |>
    #   setNames(sort(unique(df_hb$measure_type)))
    
    hb_name_no_space <- list_hb[[i]][[1, 4]] |> # get hb name and format for filenames 
      tolower() |>
      str_replace_all(" ", "_")
    
    writexl::write_xlsx(df_hb, paste0(comp_report_dir_patient_data, "/dna_", hb_name_no_space,".xlsx")) # save each measure to separate tab in excel doc
    
  }
  

# 4 - Open Cases ----------------------------------------------------------

  # NOT WORKING #
  
  df_open_cases <- read_parquet(paste0(comp_report_dir_patient_data, "/open_cases.parquet")) 
  
  list_bucket <- list()
  
  for(i in 1:nrow(list_records_to_get$open_cases)){
    
    df_open <- df_open_cases |> 
      filter(
        dataset_type == list_records_to_get$open_cases[[i, 3]],
        hb_name == list_records_to_get$open_cases[[i, 4]]#,
        #app_month == list_records_to_get$open_cases[[i, 5]]
        ) |> 
      distinct()
    
    list_bucket[[i]] <- df_open
    
  }
  
  list_hb <- bind_rows(list_bucket) |> 
    group_split(hb_name) 
  
  for(i in 1:length(list_hb)){
    
    df_hb <- list_hb[[i]]
    
    df_split <- df_hb |> # split by measure_type
      arrange(measure_type) |> 
      group_by(measure_type) |> 
      group_split() |>
      setNames(sort(unique(df_hb$measure_type)))
    
    hb_name_no_space <- list_hb[[i]][[1, 4]] |> # get hb name and format for filenames 
      tolower() |>
      str_replace_all(" ", "_")
    
    writexl::write_xlsx(df_split, paste0(comp_report_dir_patient_data, "/rtt_seen_", hb_name_no_space,".xlsx")) # save each measure to separate tab in excel doc
    
  }
   

# 5 - First Contact -------------------------------------------------------

  
  
   

  
  message(paste0("Reports created and saved to: ", comp_report_dir_patient_data))
  detach(package:writexl, unload = TRUE)
    
}







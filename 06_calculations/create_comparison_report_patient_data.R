
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
  
  
  # REFERRALS
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
      
    #names(list_bucket)[[i]] <- 
      
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
    
    writexl::write_xlsx(df_split, paste0(comp_report_dir_patient_data, "/comp_report_patients_referrals_", hb_name_no_space,".xlsx")) # save each measure to separate tab in excel doc
  
  }
  
  message(paste0("Reports created and saved to: ", comp_report_dir_patient_data))
  detach(package:writexl, unload = TRUE)
    
}







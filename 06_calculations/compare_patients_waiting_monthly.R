
##############################################################.
### Compare monthly patients waiting - aggregate vs captnd ###
##############################################################.

# Author: Charlie Smith 
# Date: 2024-03-26

compare_patients_waiting_monthly <- function(){
  
  # load aggregate data
  get_aggregate_data <- function(ds_type){
  
  ptrn = paste0('PatientsWaiting_',ds_type,'_')
  
  aggregate_files = list.files(path = '../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                               pattern = ptrn,
                               full.names = FALSE) 
  
  last_date_agg = gsub(ptrn, '', aggregate_files) %>% 
    gsub('.csv', '', .) %>% 
    as.Date(.) %>% 
    max(.) %>% 
    as.character(.)
  
  measure_types <- c("Over 52 weeks unadj Patients waiting", "19 to 35 weeks unadj Patients waiting",
                     "36 to 52 weeks unadj Patients waiting", "Total Patients Waiting unadj",
                     "0 to 18 weeks unadj Patients waiting", "Over 52 weeks unadj Patients waiting", 
                     "Over 18 weeks unadj Patients waiting")
  
  aggregate_data = read_csv_arrow(paste0('../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                                         ptrn,
                                         last_date_agg,
                                         '.csv'))
  if(ds_type == "PT"){
    
    aggregate_data <- aggregate_data |> 
      mutate(variables_mmi = case_when(
        variables_mmi == "u_TotalPatientsWaiting" ~ "Total Patients Waiting unadj",
        variables_mmi == "u_NumberOfPatientsWaiting0To18Weeks"  ~ "0 to 18 weeks unadj Patients waiting",
        variables_mmi == "u_NumberOfPatientsWaitingOver18Weeks" ~ "Over 18 weeks unadj Patients waiting",
        variables_mmi == "u_NumberOfPatientsWaiting19To35Weeks" ~ "19 to 35 weeks unadj Patients waiting",
        variables_mmi == "u_NumberOfPatientsWaiting36To52Weeks" ~ "36 to 52 weeks adj Patients waiting",
        variables_mmi == "u_NumberOfPatientsWaitingOver52Weeks" ~ "Over 52 weeks unadj Patients waiting",
        TRUE ~ NA_character_)) |> 
      filter(!is.na(variables_mmi))
    
    }
  
  aggregate_data <- aggregate_data |> 
    rename(hb_name = HB_new) |> 
    correct_hb_names_simple() |> 
    mutate(!!dataset_type_o := ds_type, 
           measure = "patients waiting") %>% 
    pivot_longer(starts_with('2'), names_to = 'app_month', values_to = 'n_aggregate') |> 
    select(measure, measure_type = variables_mmi, dataset_type, hb_name, 
           month = app_month, n_aggregate) |> 
    arrange(dataset_type, hb_name, measure_type) |> 
    filter(measure_type %in% measure_types)
  
  
  return(aggregate_data)
  
  }
  
  df_agg_camhs <- get_aggregate_data(ds_type = "CAMHS")
  df_agg_pt <- get_aggregate_data(ds_type = "PT")
  
  df_agg <- rbind(df_agg_camhs, df_agg_pt)
  
  rm(df_agg_camhs, df_agg_pt)
  
  # load captnd data
  df_captnd <- read_parquet(paste0(data_export_dir, 
                                   "/patients_waiting/by_month/monthly_waits_patients_waiting_hb.parquet")) |> 
    select(-c(6, 7)) |> 
    mutate(measure = "patients waiting", .before = everything()) |> 
    rename(month = month_start, measure_type = wait_group_unadj, n_captnd = waiting_count)
  
  
}

# To do:
# ~ unify measure_type names in both dfs





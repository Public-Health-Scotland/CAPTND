
#####################################.
### Publication - appointments df ###
#####################################.

# Author: Bex Madden
# Date: 2024-06-13
#adapted 2024-08-29 to use dtplyr

get_appointments_df <- function(){
  
  #read in data and create timepoint variables
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 

    mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
           app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(app_quarter, unit = "month")) 
  
  # select demographic vars to bind into data later
  demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group", "location", "prof_group")

  df_vars <- df |>
    select(all_of(data_keys), all_of(demographics), !!ref_acc_o, !!app_date_o,
           !!att_status_o, !!att_cat_o, app_quarter_ending) |> 
    lazy_dt() |> 
    group_by(!!!syms(data_keys), !!sym(app_date_o)) |>
    slice(1) |> 
    ungroup() |> 
    as.data.frame()

  
  # get number of appointments per day per patient
  df_app <- df |>
    select(all_of(data_keys), !!app_date_o, !!app_month_o) |> # need to account for multiples
    filter(!is.na(!!sym(app_date_o))) |> 
    lazy_dt() |> 
    group_by(across(all_of(c(data_keys, app_month_o, app_date_o)))) |> 
    summarise(n_app_patient_same_day = n(), .groups = 'drop') |>
    distinct() |>
    ungroup() |> 
    as.data.frame() |> 
    
    # join in demographics and other vars
    left_join(df_vars, by = c("patient_id", "ucpn", "dataset_type", 
                              "hb_name", "app_date"))

   
  return(df_app)
}

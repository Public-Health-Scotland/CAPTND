
#####################################.
### Publication - appointments df ###
#####################################.

# Author: Bex Madden
# Date: 2024-06-13

get_appointments_df <- function(df){
  
  #read in data and create timepoint variables
  df <- df |> 
    mutate(app_month = floor_date(app_date, unit = "month"),
           app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(app_quarter, unit = "month")) 
  
  # select demographic vars to bind into data later
  demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")
  
  df_vars <- df |>
    select(all_of(data_keys), all_of(demographics), ref_acc, app_date, att_status, 
           att_cat, app_quarter_ending) |> 
    mutate(app_quarter_ending = as.Date(app_quarter_ending)) |>
    group_by(!!!syms(data_keys), app_date) |>
    slice(1)
  
  # get number of appointments per day per patient
  df_app <- df |>
    select(all_of(data_keys), app_date, app_month) |> # need to account for multiples
    filter(!is.na(app_date)) |> 
    group_by(across(all_of(c(data_keys, app_month_o, app_date_o)))) |> 
    summarise(n_app_patient_same_day = n(), .groups = 'drop') |>
    distinct() |>
    
    # join in demographics and other vars
    left_join(df_vars, by = c("patient_id", "ucpn", "dataset_type", 
                              "hb_name", "app_date"))
   
  return(df_app)
}

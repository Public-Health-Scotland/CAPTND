###########################################.
### Calculate Standard Population - Age ###
###########################################.

# Author: Luke Taylor
# Date: 2026-06-30

calculate_total_age_std_pop <- function(df){
  
  #update aggregated age groups
  updated_age_groups_df <- df |>
    mutate(agg_age_groups = case_when(#PT age groups
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) <= 24 ~ 'Under 25',
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 25 & !!sym(age_at_ref_rec_o) <= 39 ~ '25-39',
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 40 & !!sym(age_at_ref_rec_o) <= 64 ~ '40-64',
      !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 65 ~ '65 plus',
      #CAMHS age groups
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) < 6 ~ 'Under 6',
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) >= 6 & !!sym(age_at_ref_rec_o) <= 11 ~ '6-11',
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) >= 12 & !!sym(age_at_ref_rec_o) <= 15 ~ '12-15',
      !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) > 15 ~ 'Over 15',
      #NAs with invalid CHI
      is.na(!!sym(age_at_ref_rec_o)) ~ 'Data missing'))
  
  
  standard_pop <- updated_age_groups_df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), age_group) |> 
    summarise(appts = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), age_group) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>  
    filter(hb_name == 'NHS Scotland',
           age_group != 'Data missing') |>
    group_by(!!sym(dataset_type_o)) |>
    mutate(tot_appts = sum(appts),
           weight = appts/tot_appts) |>
    select(dataset_type, hb_name, age_group, weight) |>
    save_as_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_std_pop_weights"))
  
}




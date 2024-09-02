###############################################################.
### Get age and sex summary statistics to be used in report ###
###############################################################.

get_forpub_refs_agesex <- function(){
  
  # create folder for saving output files in
  age_sex_sumstats_dir <- paste0(shorewise_pub_data_dir, "/referrals/age_sex_sumstats/")
  dir.create(age_sex_sumstats_dir)
  
  df_agesex <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/referrals_sex_age_hb.parquet")) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland")
  
  # demographics for referrals all time
  agesex_total <- df_agesex |> 
    mutate(!!sym(sex_reported_o) := replace_na(!!sym(sex_reported_o), "Not known")) |> # to group NA and 'not known' together to give single value
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o)) |> 
    summarise(count = sum(count), .groups = "drop") |>
    mutate(prop = round(count/sum(count, na.rm = TRUE)*100, 1),
           count = prettyNum(count, big.mark = ",")) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals/age_sex_sumstats/agesex_total"))
  
  age_peak <- df_agesex |> 
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o)) |> 
    mutate(total = sum(count)) |>  
    filter(!!sym(sex_reported_o) == "Male" |
             !!sym(sex_reported_o) == "Female",
           count == max(count)) |> 
    mutate(prop = round(count/total*100, 1),
           count = prettyNum(count, big.mark = ",")) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals/age_sex_sumstats/age_peak"))
  

  # latest quarter summary stats
  df_agesex_last_qt <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/referrals_sex_age_hb_qt.parquet")) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland",
           quarter_ending == month_end) |> 
    select(-c("total", "prop")) |> 
    mutate(!!sym(sex_reported_o) := replace_na(!!sym(sex_reported_o), "Not known")) |> # to group NA and 'not known' together to give single value
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o)) |> 
    summarise(count = sum(count)) |>
    mutate(prop = round(count/sum(count, na.rm = TRUE)*100, 1),
           count = prettyNum(count, big.mark = ",")) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals/age_sex_sumstats/agesex_total_last_qt"))


}

# age_groups_PT <- df_agesex |> 
#   filter(dataset_type == "PT") |> 
#   mutate(age_group = case_when(age_at_ref_rec <= 18 ~ "under 18",
#                                age_at_ref_rec >= 19 &
#                                  age_at_ref_rec <= 40 ~ "19-40",
#                                age_at_ref_rec >= 41 &
#                                  age_at_ref_rec <= 64 ~ "41-64",
#                                age_at_ref_rec >= 65 ~ "over 65",
#                               TRUE ~ NA_character_)) |> 
#   group_by(age_group) |> 
#   summarise(count = sum(count)) |> 
#   mutate(prop = round(count/sum(count, na.rm = TRUE)*100, 1),
#          count = prettyNum(count, big.mark = ","),
#          age_group = factor(age_group, levels = c("19-40", 
#                                                   "41-64", 
#                                                   "over 65",
#                                                   "under 18"))) |> 
#   arrange(age_group)  |> 
#   save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals/age_sex_sumstats/age_groups_PT"))
#   
#   
# age_groups_CAMHS <- df_agesex |> 
#   filter(dataset_type == "CAMHS") |> 
#   mutate(age_group = case_when(age_at_ref_rec <= 4 ~ "0-4",
#                                age_at_ref_rec >= 5 &
#                                  age_at_ref_rec <= 12 ~ "5-12",
#                                age_at_ref_rec >= 13 &
#                                  age_at_ref_rec <= 19 ~ "13-19",
#                                age_at_ref_rec >= 20 ~ "over 20",
#                                TRUE ~ NA_character_)) |> 
#   group_by(age_group) |> 
#   summarise(count = sum(count)) |> 
#   mutate(prop = round(count/sum(count, na.rm = TRUE)*100, 1),
#          count = prettyNum(count, big.mark = ","),
#          age_group = factor(age_group, levels = c("0-4", 
#                                                   "5-12", 
#                                                   "13-19", 
#                                                   "over 20"))) |> 
#   arrange(age_group) |> 
#   save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals/age_sex_sumstats/age_groups_CAMHS"))
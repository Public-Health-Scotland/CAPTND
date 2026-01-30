
#NHS Lothian count by age group/ethnic group
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  filter(hb_name == 'NHS Lothian',
         dataset_type == 'PT',
         ref_rec_date >= '2025-01-01' & ref_rec_date <= '2025-11-30',
         record_type_label == 'Referral')

updated_age_groups_df <- df_single_row |>
  mutate(agg_age_group = case_when(#PT age groups
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
    is.na(!!sym(dob_o)) & is.na(!!sym(age_at_ref_rec_o)) ~ 'Data missing'))

lothian_count_df <- updated_age_groups_df |>
  group_by(agg_age_group, ethnicity_last_reported) |>
  summarise(n_referrals = n()) |>
  filter(!is.na(ethnicity_last_reported)) |>
  mutate(hb_name = 'NHS Lothian')


#HB populations by ethnic group and age group
eth_census_figs <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Ethnicity/hb_ethnicity_census_2022.rds')

eth_census_figs <- eth_census_figs |>
  filter(age_group != 'Total',
         sex == 'All people') |>
  mutate(agg_age_group = case_when(age_group == '25 - 29' | age_group == '30 - 34'| age_group == '35 - 39' ~ '25-39',
                                   age_group == '40 - 44' | age_group == '45 - 49' | age_group == '50 - 54' |
                                     age_group == '55 - 59' | age_group == '60 - 64' ~ '40-64',
                                   age_group == '65 - 69' | age_group == '70 - 74' | age_group == '75 - 79' |
                                     age_group == '80 - 84' | age_group == '85 and over' ~ '65 plus',
                                   TRUE ~ 'Under 25')) |> 
  group_by(area, ethnic_group, agg_age_group) |>
  mutate(agg_pop_tot = sum(population),
         area = paste0("NHS ", area)) |>
  select(hb_name = area, ethnic_group, ethnic_group_code, agg_age_group, agg_pop_tot) |>
  distinct()

#ESP population
esp_2013_pop <- read_csv('/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/esp_2013.csv') |>
  mutate(agg_age_group = case_when(age == '25-29' | age == '30-34'| age == '35-39' ~ '25-39',
                                   age == '40-44' | age == '45-49' | age == '50-54' |
                                     age == '55-59' | age == '60-64' ~ '40-64',
                                   age == '65-69' | age == '70-74' | age == '75-79' |
                                     age == '80-84' | age == '85-89' | age == '90+' ~ '65 plus',
                                   TRUE ~ 'Under 25')) |>
  group_by(agg_age_group) |>
  mutate(esp_agg_pop_tot = sum(esp_2013),
         hb_name = 'NHS Lothian') |>
  select(agg_age_group, esp_agg_pop_tot) |>
  distinct()
  
  
#calculate age specific referral rates by ethnic group
esp_pop_rates <- lothian_count_df |>
  left_join(esp_2013_pop, by = c('agg_age_group')) |> 
  left_join(eth_census_figs, by = c("hb_name", "ethnicity_last_reported" = "ethnic_group_code", "agg_age_group")) |>
  mutate(age_specific_rate = n_referrals/agg_pop_tot*100000,
         esp_x_asr = age_specific_rate*esp_agg_pop_tot)





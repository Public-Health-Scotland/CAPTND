####################################################
##### Missingness of ethnicity data in CAPTND  #####
####################################################

#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

#ethnicity missing by age group and sex
ethnicity_age_sex_missing <- df |>
  filter(record_type_label == 'Referral',
         ethnicity_last_reported == '99' | is.na(ethnicity_last_reported)) |>
  group_by(dataset_type, sex_reported, age_group, ethnicity_last_reported) |>
  summarise(count = n()) |> ungroup()

pat_by_age_group <- df |>
  filter(record_type_label == 'Referral') |>
  group_by(dataset_type, sex_reported, age_group) |>
  summarise(tot_pats = n()) |> ungroup()

ethnicity_age_sex_count <- ethnicity_age_sex_missing |>
  left_join(pat_by_age_group, by = c("dataset_type", "age_group", "sex_reported")) |>
  mutate(prop_missing = round(count/tot_pats*100, 1),
         sex_reported = case_when(sex_reported == 1 ~ 'Male',
                                  sex_reported == 2 ~ 'Female',
                                  TRUE ~ 'Not known'))

#plot ethnicity missingness by age group and sex
ds <- 'CAMHS'

eth_age_sex_plot <- ethnicity_age_sex_count |>
  filter(sex_reported != 'Not known',
         !is.na(age_group),
         dataset_type == ds) |>
  ggplot(aes(x = age_group, y = prop_missing, fill = sex_reported)) +
  geom_col(position = "dodge", width = bar_width) +
  scale_fill_manual(values = c("Male" = "#3F3685", "Female" = "#9B4393")) +
  labs(
    y = "Prop. of missing ethnicity values",
    x = "Age group",
    fill = 'Sex reported',
    title = paste0("Proportion of ethnicity data that is missing in ", ds, " by age group and sex reported")) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        axis.text.x = element_text(hjust = 0.25, vjust = 0))

chart_width <- 32
chart_height <- 16

ggsave(plot = eth_age_sex_plot, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = paste0(ds, "_missing_ethnicity_data_by_age_sex.png"),
       path = paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/"))
  

#ethnicity missing or unknown by referral source
ethnicity_missing_ref_source <- df |>
  filter(record_type_label == 'Referral',
         ethnicity_last_reported == '99' | is.na(ethnicity_last_reported)) |>
  group_by(dataset_type, ref_source, ethnicity_last_reported) |>
  summarise(count = n()) |> ungroup()

pat_by_ref_source <- df |>
  filter(record_type_label == 'Referral') |>
  group_by(dataset_type, ref_source) |>
  summarise(tot_pats = n()) |> ungroup()

#referral source lookup
ref_source_lookup <- read_xlsx("../../../data/captnd_codes_lookup.xlsx",sheet = 'Ref_Source') %>%
  select(code = Code, ref_source_name = Ref_Source) |>
  mutate(ref_source_name = str_to_sentence(ref_source_name),
         ref_source_name = case_when(ref_source_name == 'Self referral (includes self, relations, friends and carers)' ~ 'Self referral',
                                     ref_source_name == 'A&e' ~ 'Accident & emergency',
                                     ref_source_name == 'Gp' ~ 'General practitioner',
                                     ref_source_name == 'Lac' ~ 'LAC',
                                     ref_source_name == 'Camhs team' ~ 'CAMHS team',
                                     ref_source_name == 'Camhs – out of area' ~ 'CAMHS - Out of area',
                                     ref_source_name == 'Nhs24' ~ 'NHS24',
                                     ref_source_name == 'Consultant outwith own hb' ~ 'Consultant outside own health board',
                                     TRUE ~ ref_source_name))

ethnicity_count_ref_source <- ethnicity_missing_ref_source |>
  left_join(pat_by_ref_source, by = c("dataset_type", "ref_source")) |>
  mutate(prop_missing = round(count/tot_pats*100, 1)) |>
  left_join(ref_source_lookup, by = c('ref_source' = 'code'))


#plot ethnicity missingness by referral source
ds <- 'CAMHS'

eth_ref_source_plot <- ethnicity_count_ref_source |>
  filter(!is.na(ref_source),
         dataset_type == ds) |>
  arrange(desc(count)) |>
  mutate(ref_source_name = factor(ref_source_name, levels = rev(ref_source_name))) |>
  ggplot(aes(x = ref_source_name, y = prop_missing)) +
  geom_col(width = bar_width, fill = "#9B4393") +
  geom_text(aes(label = count),
            hjust = -0.2, vjust = 0.5,
            size = 3) +
  labs(y = "Prop. of missing ethnicity values",
       x = "Referral source",
       title = paste0("Proportion of ethnicity data that is missing in ", ds, " by referral source")) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = 'none',
        axis.text.x = element_text(hjust = 0.25, vjust = 0)) +
  coord_flip()

chart_width <- 28
chart_height <- 25

ggsave(plot = eth_ref_source_plot, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = paste0(ds, "_missing_ethnicity_data_by_ref_source.png"),
       path = paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/"))


#ethnicity missing by hscp locality
ethnicity_hscp_loc_missing <- df |>
  filter(record_type_label == 'Referral',
         ethnicity_last_reported == '99' | is.na(ethnicity_last_reported)) |>
  group_by(dataset_type, hscp2019name, hscp_locality, ethnicity_last_reported) |>
  summarise(count = n()) |> ungroup()

pat_by_hscp_loc <- df |>
  filter(record_type_label == 'Referral') |>
  group_by(dataset_type, hscp2019name, hscp_locality) |>
  summarise(tot_pats = n()) |> ungroup()

#hscp lookup file
hb_hscp_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.rds') |>
  select(hscp2019name, hb2019name) |>
  distinct()

ethnicity_hscp_loc_count <- ethnicity_hscp_loc_missing |>
  left_join(pat_by_hscp_loc, by = c("dataset_type", "hscp2019name", "hscp_locality")) |>
  mutate(prop_missing = round(count/tot_pats*100, 1)) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |>
  rename("hb_name" = "hb2019name") |>
  add_hb_region()


#plot ethnicity missingness by hscp locality
ds <- 'CAMHS'
region <- 'West'

eth_hscp_loc_plot <- ethnicity_hscp_loc_count |>
  filter(!is.na(hscp_locality),
         hb_region == region,
         dataset_type == ds,
         !(hscp2019name == 'North Lanarkshire' & hscp_locality == 'Glasgow North East')) |>
  mutate(hscp_locality = factor(hscp_locality, levels = rev(unique(hscp_locality)))) |>
  ggplot(aes(x = hscp_locality, y = prop_missing)) +
  geom_col(width = bar_width, fill = "#9B4393") +
  geom_text(aes(label = hscp2019name),
            hjust = -0.2, vjust = 0.5,
            size = 3) +
  labs(y = "Prop. of missing ethnicity values",
       x = "HSCP locality",
       title = paste0("Proportion of ethnicity data that is missing in ", ds, " by HSCP locality of residence")) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = 'none',
        axis.text.x = element_text(hjust = 0.25, vjust = 0)) +
  coord_flip()

chart_width <- 40
chart_height <- 30

ggsave(plot = eth_hscp_loc_plot, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = paste0(ds, region, "_missing_ethnicity_data_by_hscp_locality.png"),
       path = paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/"))


#ethnicity missing by simd and hb name
ethnicity_simd_hb_missing <- df |>
  filter(record_type_label == 'Referral',
         ethnicity_last_reported == '99' | is.na(ethnicity_last_reported)) |>
  group_by(dataset_type, simd2020_quintile, hb_name, ethnicity_last_reported) |>
  summarise(count = n()) |> ungroup()

pat_by_simd_hb <- df |>
  filter(record_type_label == 'Referral') |>
  group_by(dataset_type, simd2020_quintile, hb_name) |>
  summarise(tot_pats = n()) |> ungroup()

ethnicity_simd_hb_count <- ethnicity_simd_hb_missing |>
  left_join(pat_by_simd_hb, by = c("dataset_type", "simd2020_quintile", "hb_name")) |>
  mutate(prop_missing = round(count/tot_pats*100, 1)) |>
  add_hb_region()


#plot ethnicity missingness by simd and hb
ds <- 'PT'
region <- 'West'

eth_simd_hb_plot <- ethnicity_simd_hb_count |>
  filter(!is.na(simd2020_quintile),
         hb_region == region,
         dataset_type == ds) |>
  ggplot(aes(x = hb_name, y = prop_missing, fill = factor(simd2020_quintile),
             group = factor(simd2020_quintile))) +
  geom_col(position = position_dodge2(width = 0.9, padding = 0.2), width = bar_width) +

  scale_fill_manual(values = c("1" = "#1E7F84", "2" = "#83BB26", "3" = "#0078D4", "4" = "#9B4393", "5" = "#3F3685"),
                    labels = c("1" = "1 - Most deprived", 
                               "2" = "2", 
                               "3" = "3", 
                               "4" = "4",
                               "5" = "5 - Least deprived")) +
  labs(y = "Prop. of missing ethnicity values",
       x = "Health board",
       fill = 'SIMD quintile',
       title = paste0("Proportion of ethnicity data that is missing in ", ds, " by SIMD quintile and health board")) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line())

chart_width <- 40
chart_height <- 25

ggsave(plot = eth_simd_hb_plot, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = paste0(ds, region, "_missing_ethnicity_data_by_simd_hb.png"),
       path = paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/"))



















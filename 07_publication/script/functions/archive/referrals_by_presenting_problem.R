#########################################################.
### For publication - referrals by presenting problem ###
#########################################################.

# Author: Bex Madden
# Date: 2024-05-28


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

#### SETUP ######
# set DS choice

#dataset_choice <- "PT"

# load data
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(ref_month = floor_date(ref_rec_date, unit = "month"),
         ref_quarter = ceiling_date(ref_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) 

# set constants
most_recent_month_in_data <- get_lastest_month_end(df) 

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# select vars
demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")

df_refs <- df |>
  remove_borders_int_refs() |>
  filter(ref_month %in% date_range) |>
  select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, ref_quarter_ending, ref_reason, ref_month) |>
  group_by(!!!syms(data_keys)) |>
  slice(1) |>
  ungroup() |>
  mutate(ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                              !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                              TRUE ~ 'pending'),
         ref_reason = as.factor(ref_reason)) 



####### ALL TIME #######

##### get 'valid' vs 'invalid' ref reason counts #####
reasons_both_all <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(hb_name) 


reasons_totals_all <- reasons_both_all |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(reason_given, hb_name, dataset_type) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = reason_given, values_from = c("totals", "perc_refs")) |>
  mutate(across(all_refs:totals_valid_reason, ~prettyNum(., big.mark = ","))) |>
  arrange(hb_name, dataset_type) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_alltime")) #_", dataset_choice


#### Get full breakdown of presenting problem ######
# Lookup codes for presenting problem

reason_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Ref_Reason") %>% 
  select(-Further.Information) |>
  mutate(Code = as.factor(Code)) |>
  rename(reason = Ref_Reason,
         ref_reason = Code)

#set order of reason codes
reason_order <- reason_lookup$reason

reasons_all_all <- left_join(reasons_both_all, reason_lookup, by = "ref_reason") |> # could group reasons together using icd10 https://icd.who.int/browse10/2019/en#/V
  mutate(reason = factor(reason, levels = reason_order)) |>
  #filter(dataset_type == dataset_choice) |>
  select(-c(reason_given, ref_reason)) |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  adorn_totals("col") |>
  mutate(across(.fns = ~replace(., is.na(.), ".."))) |>
  arrange(hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_alltime")) #_", dataset_choice

rm(reasons_both_all)

#### refs by presenting problem - by sex #####

reasons_sex_both_all <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, sex_reported) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, ref_reason, sex_reported) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup() |>
  left_join(reason_lookup, by = "ref_reason") |> # label reason codes
  mutate(reason = factor(reason, levels = reason_order),
         hb_name = factor(hb_name, levels = level_order_hb),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |>
  select(-ref_reason, -sex_reported) |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_sex_alltime")) #_", dataset_choice


#### refs by presenting problem (valid/invalid) - by age group #####

reasons_age_totals_all <- df_refs |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, reason_given, hb_name, age_group) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, reason_given, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup() |>  
  #filter(dataset_type == dataset_choice) |>
  #arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = n) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, reason_given) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_age_alltime")) #_", dataset_choice


#### refs by presenting problem (valid/invalid) - by SIMD #####

reasons_simd_totals_all <- df_refs |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, reason_given, hb_name, simd2020_quintile) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, reason_given, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup() |>  
  #filter(dataset_type == dataset_choice) |>
  pivot_wider(names_from = simd2020_quintile, values_from = n) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, reason_given) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_simd_alltime")) #_", dataset_choice



####### QUARTERLY ########

##### get 'valid' vs 'invalid' ref reason counts #####
reasons_both <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
            ref_reason == "99" |
            ref_reason == "98" ~ "invalid_reason",
          TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, ref_quarter_ending, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) 


reasons_totals <- reasons_both |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(reason_given, hb_name, ref_quarter_ending, dataset_type) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, ref_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  ungroup() |>
  pivot_wider(names_from = reason_given, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_quarterly")) #_", dataset_choice


# Lookup codes for presenting problem

reasons_all <- left_join(reasons_both, reason_lookup, by = "ref_reason") |>
  mutate(reason = factor(reason, levels = reason_order)) |>
  #filter(dataset_type == dataset_choice) |>
  select(-c(reason_given, ref_reason)) |>
  ungroup() |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, ref_quarter_ending) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_quarterly")) #_", dataset_choice

rm(reasons_both)


#### refs by presenting problem - by sex #####

reasons_sex_both <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_quarter_ending, sex_reported) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, ref_reason, ref_quarter_ending, sex_reported) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) 


# label reason codes
reasons_sex_all <- left_join(reasons_sex_both, reason_lookup, by = "ref_reason") |>
  mutate(reason = factor(reason, levels = reason_order)) |>
  # filter(dataset_type = dataset_choice) |>
  mutate(Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_),
         ref_quarter_ending = as.Date(ref_quarter_ending, format = "%Y-%m-%d")) |>
  select(-ref_reason, -sex_reported) |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  arrange(dataset_type, hb_name, ref_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_sex")) #_", dataset_choice

rm(reasons_sex_both)


#### refs by presenting problem (valid/invalid) - by age group #####

reasons_age_both <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_quarter_ending, age_group) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, ref_quarter_ending, age_group, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending, "%Y-%m_%d"))


# present

reasons_age_totals <- reasons_age_both |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(dataset_type, hb_name, ref_quarter_ending, reason_given, age_group) |>
  summarise(totals = sum(n), .groups = "drop") |>
  #arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = totals) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, ref_quarter_ending, reason_given) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_age")) #_", dataset_choice

rm(reasons_age_both)


#### refs by presenting problem (valid/invalid) - by SIMD #####

reasons_simd_both <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_quarter_ending, simd2020_quintile) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, ref_quarter_ending, simd2020_quintile, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         simd2020_quintile = as.factor(simd2020_quintile),
         ref_quarter_ending = as.Date(ref_quarter_ending, "%Y-%m-%d")) 


# present 

reasons_simd_totals <- reasons_simd_both |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(dataset_type, hb_name, ref_quarter_ending, reason_given, simd2020_quintile) |>
  summarise(totals = sum(n)) |>
  ungroup() |>
  pivot_wider(names_from = simd2020_quintile, values_from = totals) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, ref_quarter_ending, reason_given) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_simd")) #_", dataset_choice

rm(reasons_simd_both)



####### MONTHLY ########

##### get 'valid' vs 'invalid' ref reason counts #####
reasons_both_m <- df_refs |>
  group_by(dataset_type, hb_name, ref_reason, ref_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, ref_month, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  ungroup()


reasons_totals_m <- reasons_both_m |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(reason_given, hb_name, ref_month, dataset_type) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, ref_month) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2),
         ref_month = as.Date(ref_month, "%Y-%m_%d")) |>
  ungroup() |>
  pivot_wider(names_from = reason_given, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type, ref_month) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_monthly")) #_", dataset_choice


# Lookup codes for presenting problem

reasons_all_m <- left_join(reasons_both_m, reason_lookup, by = "ref_reason") |>
  mutate(reason = factor(reason, levels = reason_order)) |>
  #filter(dataset_type == dataset_choice) |>
  select(-c(reason_given, ref_reason)) |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, ref_month) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_monthly")) #_", dataset_choice

rm(reasons_both_m)


#### refs by presenting problem - by sex #####

reasons_sex_both_m <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_month, sex_reported) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, ref_reason, ref_month, sex_reported) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  ungroup()


# label reason codes
reasons_sex_all_m <- left_join(reasons_sex_both_m, reason_lookup, by = "ref_reason") |>
  mutate(reason = factor(reason, levels = reason_order)) |>
  # filter(dataset_type = dataset_choice) |>
  mutate(Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_),
    ref_month = as.Date(ref_month, format = "%Y-%m-%d")) |>
  select(-ref_reason, -sex_reported) |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  arrange(dataset_type, hb_name, ref_month, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_sex_monthly")) #_", dataset_choice

rm(reasons_sex_both_m)

#### refs by presenting problem (valid/invalid) - by age group #####

reasons_age_both_m <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_month, age_group) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, ref_month, age_group, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  mutate(ref_month = as.Date(ref_month, "%Y-%m_%d")) |>
  ungroup()

# present

reasons_age_totals_m <- reasons_age_both_m |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(dataset_type, hb_name, ref_month, reason_given, age_group) |>
  summarise(totals = sum(n), .groups = "drop") |>
  arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = totals) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, ref_month, reason_given) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_age_monthly")) #_", dataset_choice

rm(reasons_age_both_m)


#### refs by presenting problem (valid/invalid) - by SIMD #####

reasons_simd_both_m <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_month, simd2020_quintile) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, ref_reason, ref_month, simd2020_quintile, reason_given) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         ref_month = as.Date(ref_month, "%Y-%m-%d")) |>
  ungroup()

# present 

reasons_simd_totals_m <- reasons_simd_both_m |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(dataset_type, hb_name, ref_month, reason_given, simd2020_quintile) |>
  summarise(totals = sum(n), .groups = "drop") |>
  arrange(simd2020_quintile) |>
  pivot_wider(names_from = simd2020_quintile, values_from = totals) |>
  adorn_totals("col") |>
  arrange(dataset_type, hb_name, ref_month, reason_given) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_simd_monthly")) #_", dataset_choice

rm(reasons_simd_both_m)



###### CREATE EXCEL DOC ########

# Create a blank workbook
refs_pp <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(refs_pp, "Reason Given - All Time")
addWorksheet(refs_pp, "Reason Given - Quarterly")
addWorksheet(refs_pp, "Reason Given - Monthly")

addWorksheet(refs_pp, "Reasons - All Time")
addWorksheet(refs_pp, "Reasons - Quarterly")
addWorksheet(refs_pp, "Reasons - Monthly")

addWorksheet(refs_pp, "Reasons - All by Sex")
addWorksheet(refs_pp, "Reasons - Qt by Sex")
addWorksheet(refs_pp, "Reasons - Monthly by Sex")

addWorksheet(refs_pp, "Reason Given - All by Age")
addWorksheet(refs_pp, "Reason Given - Qt by Age")
addWorksheet(refs_pp, "Reason Given - Mth by Age")

addWorksheet(refs_pp, "Reason Given - All by SIMD")
addWorksheet(refs_pp, "Reason Given - Qt by SIMD")
addWorksheet(refs_pp, "Reason Given - Mth by SIMD")

#addWorksheet(refs_pp, "Sheet 2 Name")

# Write the data to the sheets
writeData(refs_pp, sheet = "Reason Given - All Time", x = reasons_totals_all)
writeData(refs_pp, sheet = "Reason Given - Quarterly", x = reasons_totals)
writeData(refs_pp, sheet = "Reason Given - Monthly", x = reasons_totals_m)

writeData(refs_pp, sheet = "Reasons - All Time", x = reasons_all_all)
writeData(refs_pp, sheet = "Reasons - Quarterly", x = reasons_all)
writeData(refs_pp, sheet = "Reasons - Monthly", x = reasons_all_m)

writeData(refs_pp, sheet = "Reasons - All by Sex", x = reasons_sex_both_all)
writeData(refs_pp, sheet = "Reasons - Qt by Sex", x = reasons_sex_all)
writeData(refs_pp, sheet = "Reasons - Monthly by Sex", x = reasons_sex_all_m)

writeData(refs_pp, sheet = "Reason Given - All by Age", x = reasons_age_totals_all)
writeData(refs_pp, sheet = "Reason Given - Qt by Age", x = reasons_age_totals)
writeData(refs_pp, sheet = "Reason Given - Mth by Age", x = reasons_age_totals_m)

writeData(refs_pp, sheet = "Reason Given - All by SIMD", x = reasons_simd_totals_all)
writeData(refs_pp, sheet = "Reason Given - Qt by SIMD", x = reasons_simd_totals)
writeData(refs_pp, sheet = "Reason Given - Mth by SIMD", x = reasons_simd_totals_m)


# Export the file
saveWorkbook(refs_pp, paste0(shorewise_pub_dir, "/measure_summaries/refs_reason_forpub.xlsx"), overwrite = TRUE)


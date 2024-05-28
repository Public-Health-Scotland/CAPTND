#########################################################.
### For publication - referrals by presenting problem ###
#########################################################.

# Author: Bex Madden
# Date: 2024-05-28


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

most_recent_month_in_data <- get_lastest_month_end(df)

demographics <- c("sex_from_chi", "age_at_ref_rec", "simd2020_quintile")


df_refs <- df |>
  select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, referral_month, ref_reason) |>
  distinct() |>
  mutate(ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                              !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                              TRUE ~ 'pending'),
         ref_reason = as.factor(ref_reason)) 

# PT
reasons_pt <- df_refs |>
  filter(dataset_type == "PT") |>
  group_by(ref_reason, hb_name) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
            ref_reason == "99" |
            ref_reason == "98" ~ FALSE,
          TRUE ~ TRUE))

reasons_pt_sco <- df_refs |>
  filter(dataset_type == "PT") |>
  group_by(ref_reason) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ FALSE,
    TRUE ~ TRUE),
    hb_name = "NHS Scotland")

reasons_pt_both <- bind_rows(reasons, reasons_sco) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(hb_name) 
  #save_as_parquet() # needed?

reasons_pt_totals <- reasons_both |>  
group_by(reason_given, hb_name) |>
  summarise(totals = sum(n)) |>
  ungroup() |>
  group_by(hb_name) |>
  mutate(all_refs = sum(totals),
         perc_refs = totals/all_refs*100) |>
  arrange(hb_name) |>
  save_as_parquet(paste0(xx_dir, "referrals_reason_given_pt"))

rm(reasons_pt, reasons_pt_sco)
         
# CAMHS
reasons_camhs <- df_refs |>
  filter(dataset_type == "CAMHS") |>
  group_by(ref_reason, hb_name) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ FALSE,
    TRUE ~ TRUE))

reasons_camhs_sco <- df_refs |>
  filter(dataset_type == "CAMHS") |>
  group_by(ref_reason) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ FALSE,
    TRUE ~ TRUE),
    hb_name = "NHS Scotland")

reasons_camhs_both <- bind_rows(reasons, reasons_sco) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(hb_name) |>
  # save_as_parquet()  # needed?

reasons_camhs_totals <- reasons_both |>  
  group_by(reason_given, hb_name) |>
  summarise(totals = sum(n)) |>
  ungroup() |>
  group_by(hb_name) |>
  mutate(all_refs = sum(totals),
         perc_refs = totals/all_refs*100) |>
  arrange(hb_name) |>
  save_as_parquet(paste0(xx_dir, "referrals_reason_given_camhs"))

rm(reasons_camhs, reasons_camhs_sco)

#############################################.
#### Digital Referrals - Referral Reason ####.
#############################################.

# Author: Luke Taylor
# Date: 2024-07-21


dig_refs_dir <- paste0(shorewise_pub_data_dir, "/digital_referrals/")
dir.create(dig_refs_dir)
measure_label <- "digi_refs_reason"

# get digital referrals source

df_digi_reason <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  filter(!!sym(referral_month_o) %in% date_range,
         !is.na(!!sym(act_code_sent_date_o))) |>
  select(all_of(data_keys), !!referral_month_o, !!ref_reason_o) |> # need to account for multiples 
  lazy_dt() |> 
  group_by(across(all_of(c(!!hb_name_o, !!dataset_type_o, !!referral_month_o, !!ref_reason_o)))) |> 
  summarise(n_digi_ref_by_reason = n(), .groups = 'drop') |>
  distinct() |>
  ungroup() |> 
  as.data.frame() |> 
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(ref_quarter, unit = "month"))


# lookup codes for care contact location
ref_reason_lookup <- read_xlsx("../../../data/captnd_codes_lookup.xlsx",sheet = 'Ref_Reason') %>%
  select(ref_reason = Code, ref_reason_name = Ref_Reason) |>
  mutate(ref_reason_name = str_to_sentence(ref_reason_name),
         ref_reason_name = case_when(ref_reason_name == 'Adhd' ~ 'ADHD',
                                     TRUE ~ ref_reason_name)) 

df_digi_ref_reason <- df_digi_reason |> 
  left_join(ref_reason_lookup, by = "ref_reason") 


# get total digi referrals for each time period --------------------------------------
#all time
df_digi_refs_all <- df_digi_reason |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  summarise(total_digi_refs = sum(n_digi_ref_by_reason), .groups = 'drop') |>
  group_by(!!sym(dataset_type_o)) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup()

#quarterly
df_digi_refs_qt <- df_digi_reason |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
  summarise(total_digi_refs = sum(n_digi_ref_by_reason), .groups = 'drop') |> 
  group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup()

#monthly
df_digi_refs_mth <- df_digi_reason |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |>  
  summarise(total_digi_refs = sum(n_digi_ref_by_reason), .groups = 'drop') |> 
  group_by(!!sym(dataset_type_o), referral_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup()


# overall ----------------------------------------------------------------
# by hb 
digi_refs_all <- df_digi_ref_reason |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_reason_name) |>
  summarise(count = sum(n_digi_ref_by_reason), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), ref_reason_name) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_digi_refs_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_digi_refs*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  save_as_parquet(paste0(dig_refs_dir, measure_label, "all_hb"))

# quarterly ----------------------------------------------------------------
# by hb 
digi_refs_qt <- df_digi_ref_reason |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_reason_name, app_quarter_ending) |>
  summarise(count = sum(n_digi_ref_by_reason), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), ref_reason_name, app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_digi_refs_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_digi_refs*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o, app_quarter_ending) |>
  save_as_parquet(paste0(dig_refs_dir, measure_label, "qt_hb"))

# monthly ----------------------------------------------------------------
# by hb 
digi_refs_mth <- df_digi_ref_reason |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_reason_name, referral_month) |>
  summarise(count = sum(n_digi_ref_by_reason), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), ref_reason_name, referral_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_digi_refs_mth, by = c("dataset_type", "hb_name", "referral_month")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_digi_refs*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o, referral_month) |>
  save_as_parquet(paste0(dig_refs_dir, measure_label, "mth_hb"))



digi_reason_tot <- df_digi_ref_reason |>
  group_by(ref_reason_name) |>
  summarise(count = sum(n_digi_ref_by_reason), .groups = 'drop') |>
  ungroup() |>
  arrange(desc(count)) |>
  filter(!is.na(ref_reason_name)) |>
  mutate(tot_digi_refs = sum(count),
         prop = round(count/tot_digi_refs*100, 1)) |>
  ungroup()










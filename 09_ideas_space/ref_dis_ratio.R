#######################################
### Ratio of referrals to discharges ##
#######################################

#Author: Luke Taylor
#Date: 2026-04-14

#referrals calculation code
df_single_row_ref <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range) |> # date_range is the last 15 months
  lazy_dt() |> #lazy_dt() is just used to speed up the code
  group_by(!!!syms(data_keys)) |> #group by pathway data keys (dataset_type, hb_name, ucpn, patient_id)
  slice(1) |> #slice ensures one row per pathway
  ungroup() |> 
  as.data.frame() #this is required to transform the data back into a dataframe after using lazy_dt()

df_ref_month_hb <- df_single_row_ref |> 
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> #calculate NHS Scotland figures
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb"))


#discharges calculation code
df_single_row_dis <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(case_closed_month_o) %in% date_range & # apply date range filter
           record_type_label == 'Discharge') # can filter using new date 

df_ref_month_hb <- df_single_row_dis |> 
  group_by(!!sym(case_closed_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(case_closed_month_o), !!sym(dataset_type_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> #calculate NHS Scotland figure
  right_join(df_month_ds_hb, by = c("case_closed_month" = "month", "dataset_type", "hb_name")) |> #skeleton dataset joined so that months without a ref are included in the df
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |> #convert NAs to 0 for months were no discharges were received 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(case_closed_month_o)) |> 
  save_as_parquet(path = paste0(dis_dir, measure_label, "month_hb"))




#load referrals by hb/month
ref_df <- read_parquet(paste0(ref_dir, "referrals_month_hb.parquet")) |>
  ungroup() |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |>
  rename(ref_n = count)

#load discharges by hb/month
dis_df <- read_parquet(paste0(dis_dir, "discharges_month_hb.parquet")) |>
  ungroup() |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |>
  filter(hb_name != 'NHS 24') |>
  rename(dis_n = count)

#join datasets
ratio_df <- ref_df |>
  left_join(dis_df, by = c("referral_month" = "case_closed_month", "dataset_type", "hb_name")) |>
  mutate(ratio = round(ref_n/dis_n, 1)) |>
  filter(referral_month == "2026-03-01")
  



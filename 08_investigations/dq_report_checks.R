



# dq report update checks

df_check_all <- read_parquet("../../../output/analysis_2024-10-07/pre_shorewise_outputs/01_data_prep/captnd_checked.parquet")


# NHS A&A CAMHS: 1 postocode invalid
month_latest <- ymd(c("2024-08-01"))

# test1 <- df_check_all |> 
#   mutate(header_month = floor_date(header_date, unit = "months")) |> 
#   distinct() |> 
#   filter(hb_name == "NHS Ayrshire and Arran" &
#            dataset_type == "CAMHS" &
#            header_month == month_latest)
# 
# test1.2 <- df_check_all |>
#   mutate(header_month = floor_date(header_date, unit = "months")) |> 
#   filter(header_month == month_latest) |> 
#   distinct() |> 
#   group_by(dataset_type, hb_name) |> 
#   summarise(n = n())

df_check_records <- read_parquet("../../../output/analysis_2024-10-07/pre_shorewise_outputs/01_data_prep/captnd_raw.parquet")

test2 <- df_check_records |>
  mutate(header_month = floor_date(header_date, unit = "months")) |> 
  filter(header_month %in% month_latest) |> 
  select(-1, -2) |> 
  distinct() |> 
  group_by(dataset_type, hb_name) |> 
  summarise(n = n())

df_report_latest <- read_parquet(paste0(data_quality_report_dir, "/captnd_dq_clean_all.parquet")) |> 
  filter(header_date_month %in% month_latest &
    hb_name != "NHS Scotland" &
           value == "known" &
           case_when(dataset_type == "CAMHS" ~ variable == "chi",
                     TRUE ~ variable == "upi"))

df_comp <- left_join(df_report_latest, test2, by = c("dataset_type", "hb_name")) |> 
  mutate(diff = total - n) |> 
  select(header_date_month, dataset_type, hb_name, variable, value, proportion, total, n, diff) |> 
  rename(number_raw_recs = n) |> 
  filter(diff != 0) |> 
  arrange(-diff)


# check each stage manually

test3 <- df_dis_checked |>
  mutate(header_month = floor_date(header_date, unit = "months")) |> 
  filter(header_month == month_latest) |> 
  group_by(dataset_type, hb_name) |> 
  summarise(n = n())





# dq report update checks

df_check_all <- read_parquet("../../../output/analysis_2024-10-07/pre_shorewise_outputs/01_data_prep/captnd_checked.parquet")


# NHS A&A CAMHS: 1 postocode invalid
month_latest <- "2024-08-01"

test1 <- df_check_all |> 
  mutate(header_month = floor_date(header_date, unit = "months")) |> 
  distinct() |> 
  filter(hb_name == "NHS Ayrshire and Arran" &
           dataset_type == "CAMHS" &
           header_month == month_latest)



df_check_records <- read_parquet("../../../output/analysis_2024-10-07/pre_shorewise_outputs/01_data_prep/captnd_raw.parquet")

test2 <- df_check_records |>
  mutate(header_month = floor_date(header_date, unit = "months")) |> 
  filter(header_month == month_latest) |> 
  select(-1, -2) |> 
  distinct() |> 
  group_by(dataset_type, hb_name) |> 
  summarise(n = n())

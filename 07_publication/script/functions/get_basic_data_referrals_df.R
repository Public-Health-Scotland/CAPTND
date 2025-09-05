###############################################.
#### Publication - basic data referrals df ####.
###############################################.

# Author: Bex Madden
# Date: 2024-06-13

get_basic_data_referrals_df <- function(dataset_choice){

# 1 - Load latest CAPTND data ---------------------------------------------------------

df <- read_parquet(file = paste0(data_prep_dir, "/assess_refs.parquet")) %>%
  filter(header_date %in% date_range) %>%
  distinct(.)

df_all <- df |>
  filter(dataset_type == dataset_choice)


# 2 - referral trend -----------------------------------------------------------

# HB quarterly trend table 
table_hb_q_trend <- df_all %>%
  filter(!is.na(ref_rec_date) | !is.na(ref_date)) %>%
    mutate(quarter = ceiling_date(ref_rec_date, unit = "quarter") - 1,
         quarter_ending = floor_date(quarter, unit = "month")) %>% 
  group_by(chi, ucpn) %>% 
  slice(1) %>% 
  ungroup() %>%
  group_by(hb_name, quarter_ending) %>% 
  summarise(referrals = n(), .groups = "drop") %>% 
  group_by(quarter_ending)  %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) %>%
  select(c("hb_name", everything())) %>%
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) %>% 
  filter(!is.na(hb_name)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_", dataset_choice))

# monthly
table_hb_m_trend <- df_all %>%
  filter(!is.na(ref_rec_date) | !is.na(ref_date)) %>%
  group_by(chi, ucpn) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(ref_rec_month = floor_date(ref_rec_date, unit = "month")) %>% 
  group_by(hb_name, ref_rec_month) %>% 
  summarise(referrals = n(), .groups = "drop") %>% 
  group_by(ref_rec_month)  %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) %>%
  select(c("hb_name", everything())) %>%
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) %>% 
  filter(!is.na(hb_name)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_monthly_", dataset_choice))

}
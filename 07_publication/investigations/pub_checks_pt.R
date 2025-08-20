########################################
#####  Compare published  PT data  #####
########################################

month_end <- "2025-06-01"
source("./07_publication/script/chapters/2_load_functions.R")

#latest pub month
latest_pub_month <- as.Date('2025-09-01')
latest_month_end <- as.Date('2025-06-01')

date_range <- seq.Date(from = ymd(latest_pub_month) - months(14), to = latest_pub_month, by = "month")

df_time <- data.frame(month = date_range) |> 
  append_quarter_ending(date_col = "month")

df_quarts <- df_time |> select(quarter_ending) |> distinct()

#prev pub month
prev_pub_month <- as.Date('2025-03-01')
prev_month_end <- as.Date('2024-12-01')

date_range2 <- seq.Date(from = ymd(prev_month_end) - months(14), to = prev_month_end, by = "month")

df_time2 <- data.frame(month = date_range2) |> 
  append_quarter_ending(date_col = "month")

df_quarts2 <- df_time2 |> select(quarter_ending) |> distinct()


#overlapping quarters to filter on
df_quarts <- df_quarts |>
  inner_join(df_quarts2)

#referrals by sex
prev_tab_1 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                sheet = 'Tab 1 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

latest_tab_1 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                    sheet = 'Tab 1 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#referrals by age
prev_tab_2 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 2 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

latest_tab_2 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 2 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#referrals by SIMD
prev_tab_3 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 3 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

latest_tab_3 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 3 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#referrals by acceptance status
prev_tab_4 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 4 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

latest_tab_4 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 4 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#referrals by source
prev_tab_5 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 5 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:6, 8)

latest_tab_7 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 7 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#first con attendance
prev_tab_6 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 6 Data') |>
  mutate(quarter_ending = as.Date(app_quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7, 9)

latest_tab_8 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 8 Data') |>
  mutate(quarter_ending = as.Date(app_quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#dna rate
prev_tab_7 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 7 Data') |>
  mutate(quarter_ending = as.Date(app_quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

latest_tab_9 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 9 Data') |>
  mutate(quarter_ending = as.Date(app_quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#appts by location
prev_tab_8 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 8 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:6, 8)

latest_tab_10 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 10 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)

#appts by prof group
prev_tab_9 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", prev_pub_month, ".xlsx"),
                    sheet = 'Tab 9 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:6, 8)

latest_tab_11 <- read_excel(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/publication/captnd_data_tables_pt_", latest_pub_month, ".xlsx"),
                           sheet = 'Tab 11 Data') |>
  mutate(quarter_ending = as.Date(quarter_ending)) |>
  filter(!is.na(hb_name),
         quarter_ending %in% df_quarts$quarter_ending) |>
  select(2:7)



df_cgi_s <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) |>
  filter(!is.na(ucpn)) %>%
  # only keep if one of the outcome variables is populated
  filter(!is.na(cgi_s)) %>%
  # remove missing HB
  filter(!is.na(hb_name)) %>%
  # get date range
  filter(header_date >= as.Date("2025-01-01")) %>% 
  select(dataset_type,
         hb_name,
         ucpn,
         chi,
         header_date,
         cgi_s)

#cgi_s summary
# To be taken at all sessions
cgi_s_summary <- df_cgi_s |> 
  filter(!is.na(cgi_s)) |> 
  select(dataset_type,
         hb_name,
         header_date,
         cgi_s) %>% 
  mutate(cgi_s = if_else(cgi_s == "00",
                         "00 Not Assessed",
                         "Assessed")) |>
  group_by(hb_name, dataset_type, header_date, cgi_s) |>
  summarise(count = n())



df_cgi_i <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) |>
  filter(!is.na(ucpn)) %>%
  # only keep if one of the outcome variables is populated
  filter(!is.na(cgi_i)) %>%
  #identify records where only cgi_s is complete - only proxy to suggest this was first treatment appt?
  #cgi_i should only be recorded from second treatment appt onwards
  mutate(first_treat = case_when(cgi_i == '00' & cgi_s != '00' ~ 'first_treat',
                                 TRUE ~ 'follow_up')) |>
  filter(first_treat == 'follow_up') |>
  # remove missing HB
  filter(!is.na(hb_name)) %>%
  # get date range
  filter(header_date >= as.Date("2025-01-01")) %>% 
  select(dataset_type,
         hb_name,
         ucpn,
         chi,
         header_date,
         cgi_i)
    
#cgi_i summary
# To be taken from second session onwards 
cgi_i_summary <- df_cgi_i |> 
  filter(!is.na(cgi_i)) |> 
  select(dataset_type,
         hb_name,
         header_date,
         cgi_i) %>% 
  mutate(cgi_i = if_else(cgi_i == "00",
                         "00 Not Assessed",
                         "Assessed")) |>
  group_by(hb_name, dataset_type, header_date, cgi_i) |>
  summarise(count = n())


df_pgi_i <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) |>
  filter(!is.na(ucpn)) %>%
  # only keep if one of the outcome variables is populated
  filter(!is.na(pgi_i)) %>%
  #identify records where only cgi_s is complete - only proxy to suggest this was first treatment appt?
  #cgi_i should only be recorded from second treatment appt onwards
  mutate(first_treat = case_when(pgi_i == '00' & cgi_s != '00' ~ 'first_treat',
                                 TRUE ~ 'follow_up')) |>
  filter(first_treat == 'follow_up') |>
  # remove missing HB
  filter(!is.na(hb_name)) %>%
  # get date range
  filter(header_date >= as.Date("2025-01-01")) %>% 
  select(dataset_type,
         hb_name,
         ucpn,
         chi,
         header_date,
         pgi_i)

#pgi_i summary
# To be taken from second session onwards 
pgi_i_summary <- df_pgi_i |> 
  filter(!is.na(pgi_i)) |> 
  select(dataset_type,
         hb_name,
         header_date,
         pgi_i) %>% 
  mutate(pgi_i = if_else(pgi_i == "00",
                         "00 Not Assessed",
                         "Assessed")) |>
  group_by(hb_name, dataset_type, header_date, pgi_i) |>
  summarise(count = n())



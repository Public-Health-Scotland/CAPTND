######################################
### Check unavailability validity  ###
######################################

# month_start <- as.Date('2025-06-01')
# month_end <- as.Date('2025-06-30')
#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#Checks that at least two of the three unavailability fields are complete
#If not, they're marked as invalid, as we cannot place the unavailability in time correctly

invalid_unav_period <- function(){

unav_validity_df <- df |>
  filter(!is.na(unav_date_start) | !is.na(unav_date_end) | !is.na(unav_days_no),
         unav_days_no != 0,
         header_date == month_start) |>
  mutate(unav_valid = case_when(!is.na(unav_date_start) & is.na(unav_date_end) & is.na(unav_days_no) ~ 'Invalid',
                                !is.na(unav_date_end) & is.na(unav_date_start) & is.na(unav_days_no) ~ 'Invalid',
                                !is.na(unav_days_no) & is.na(unav_date_start) & is.na(unav_date_end) ~ 'Invalid',
                                TRUE ~ 'Valid')) |>
  filter(unav_valid == 'Invalid') |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o), !!sym(app_date_o),
         !!sym(unav_date_start_o), !!sym(unav_date_end_o), !!sym(unav_days_no_o), !!sym(header_date_o)) |>
  write_parquet(paste0(stats_checked_dir, "/invalid_unav", month_start, ".parquet"))

}
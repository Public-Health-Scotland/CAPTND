##################################
###### Clean Unavailability ######
##################################

#Author: Luke Taylor
#Date: 02/04/2024

# function to clean unavailability dates for inclusion within patient waiting calculation

unav_periods <- function(){
  
unav_df <- df |>
  #filter for rows with unavailability
  filter(!is.na(unav_date_start) | !is.na(unav_date_end),
         ref_acc_opti != "2") |>
  select(!!!syms(data_keys), unav_date_start, unav_date_end, unav_days_no) |>
  #check validity of unavailability period
  mutate(unav_valid = case_when(is.na(unav_date_start) & !is.na(unav_date_end) & is.na(unav_days_no) ~ 'invalid',
                                !is.na(unav_date_start) & is.na(unav_date_end) & is.na(unav_days_no) ~ 'invalid',
                                TRUE ~ 'valid')) |>
  filter(unav_valid == 'valid') |>
  #complete start and end of unavailability if missing
  mutate(unav_date_end = case_when(is.na(unav_date_end) ~ as.Date(unav_date_start + unav_days_no),
                                   TRUE ~ unav_date_end),
         unav_date_start = case_when(is.na(unav_date_start) ~ as.Date(unav_date_end - unav_days_no),
                                   TRUE ~ unav_date_start)) |>
  #create sub month start and end based on start of unav period
  mutate(sub_month_start = floor_date(unav_date_start, unit = 'month'),
         sub_month_end = ceiling_date(sub_month_start, unit = "month") - days(1),
         unav_days_no = as.integer(unav_date_end - unav_date_start) + 1) |>
  #keep distinct rows
  distinct() |>
  #keep maximum no of days of unav for periods with the same unav start date
  group_by(!!!syms(data_keys), unav_date_start) |>
  slice_max(unav_days_no) |>
  #keep maximum no of days of unav for periods with the same unav end date
  group_by(!!!syms(data_keys), unav_date_end) |>
  slice_max(unav_days_no) |>
  group_by(!!!syms(data_keys)) |>
  #number each unique period of unav for patient pathway 
  mutate(unav_period_no = row_number()) |>
  select(-unav_valid)


multi_mth_unav <- unav_df |>
  #filter out unav periods that overlap into new month
  filter(unav_date_end > sub_month_end) |>
  #set sub month start and end based on unav date end
  mutate(sub_month_start = floor_date(unav_date_end, unit = 'month'),
         sub_month_end = ceiling_date(unav_date_end, unit = "month") - days(1),
         #set unav start date based on start of month
         unav_date_start = sub_month_start,
         #recalculate unav days for that period
         unav_days_no = as.integer(unav_date_end - unav_date_start) + 1)

if (nrow(unav_df) == 0) {
  
  unav_df
  
} else {
  
  unav_df <- unav_df |>
    mutate(unav_date_end = case_when(unav_date_end > sub_month_end ~ sub_month_end,
                                     TRUE ~ unav_date_end)) |>
    mutate(unav_days_no = as.integer(unav_date_end - unav_date_start)) |>
    rbind(multi_mth_unav) |>
    group_by(!!!syms(data_keys), unav_period_no) |>
    #add in complete months of unavailability 
    complete(sub_month_start = seq(min(sub_month_start), max(sub_month_start), by = "1 month")) |>
    arrange(!!!syms(data_keys), sub_month_start) |>
    mutate(sub_month_end = case_when(is.na(sub_month_end) ~ ceiling_date(sub_month_start, unit = 'month') - days(1),
                                     TRUE ~ sub_month_end),
           unav_date_start = case_when(is.na(unav_date_start) ~ sub_month_start,
                                       TRUE ~ unav_date_start),
           unav_date_end = case_when(is.na(unav_date_end) ~ sub_month_end,
                                     TRUE ~ unav_date_end),
           unav_days_no = case_when(is.na(unav_days_no) ~ as.integer(unav_date_end - unav_date_start) + 1,
                                    TRUE ~ unav_days_no)) |>
    ungroup()
  
  
}

  return(unav_df)

}
create_error_logs <- function(df){

month_cols <- grep("-", names(df), value = TRUE)

monthly_error_logs_df <- df %>%
  rowwise() %>%
  mutate(
    # basic stats for the 15 months
    mn  = mean(c_across(all_of(month_cols)), na.rm = TRUE),
    sdv = sd(c_across(all_of(month_cols)), na.rm = TRUE),
    
    # ---- COUNT limits: 1SD, 2SD, 3SD ----
    sd1_low  = mn - 1*sdv,
    sd1_high = mn + 1*sdv,
    sd2_low  = mn - 2*sdv,
    sd2_high = mn + 2*sdv,
    sd3_low  = mn - 3*sdv,
    sd3_high = mn + 3*sdv,
  ) %>%
  ungroup() %>%
  pivot_longer(all_of(month_cols), names_to = "Month", values_to = "Value") %>%
  mutate(
    MonthDate = as.Date(paste0("01-", Month), "%d-%b-%Y"),
    
    # ---- Determine breach category ----
    Breach = case_when(
      
      # ========== COUNT: SD band classification ==========
      Measurement == "Count" & Value < sd1_low  ~ "Below 1–2 SD",
      Measurement == "Count" & Value < sd2_low  ~ "Below 2–3 SD",
      Measurement == "Count" & Value < sd3_low  ~ "Below 3+ SD",
      
      Measurement == "Count" & Value > sd1_high & Value <= sd2_high ~ "Above 1–2 SD",
      Measurement == "Count" & Value > sd2_high & Value <= sd3_high ~ "Above 2–3 SD",
      Measurement == "Count" & Value > sd3_high                     ~ "Above 3+ SD",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Breach)) %>%
  group_by(Measurement) %>%
  mutate(latest_month = max(MonthDate, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(MonthDate == latest_month) %>%
  select(
    Dataset, `Health Board`, Variable, `Submission Status`,`DQ Assessment`, Measurement, Month, Value, Breach
  )


outfile <- paste0(
  "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/alan/",
  "dq_monthly_error_logs_",
  month_latest,
  ".csv")

# write CSV
write.csv(
  monthly_error_logs_df,
  file = outfile,
  row.names = FALSE)

}


#Rereferral query

#1.Number of children and young people referred to CAMHS more than once within a 12-month period.
#2.Number of repeat referrals following an earlier referral that was not accepted.
#3.Number of repeat referrals subsequently accepted for assessment or intervention.
#4.Where recorded, number of children and young people re-referred following previous 
#involvement with third-sector or community-based mental health services.

#Query 1
#Number of children and young people referred to CAMHS more than once within a 12-month period.
re_ref_df_12mth <- df |>
  filter(dataset_type == 'CAMHS') |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti) |>
  group_by(hb_name, chi) |>
  mutate(count = n_distinct(ucpn)) |>
  filter(count >= 2) |>
  mutate(last_ref_date = lag(ref_rec_date_opti)) |>
  filter(!is.na(last_ref_date),
         last_ref_date >= ref_rec_date_opti %m-% months(12)) |>
  ungroup() |> select(-count) |>
  append_quarter_ending(date_col = "ref_rec_date_opti") |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  summarise(count = n())

#In the above analysis the quarter in which a patient is counted is based on the
#referral received date of the second or subsequent referral rather than the date of the 
#initial referral. A patient may be counted in more than quarter for a given period,
#if they have a further referral submitted that is within 12 months of the last.


ds <- "CAMHS"
hb <- 'NHS Western Isles'

re_ref_df_12mth_plot <- re_ref_df_12mth |>
  filter(dataset_type == ds,
         hb_name == hb,
         quarter_ending >= month_start) |>
  ggplot(aes(x = quarter_ending, y = count)) +
  geom_line(colour = "#9B4393", linewidth = 1) +
  geom_point(colour = "#9B4393", size = 2) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(breaks = sort(unique(re_ref_df_12mth$quarter_ending)),
               date_labels = "%b %Y") +
  labs(y = "Number of patients re-referred within 12mths",
       x = "Quarter ending",
       title = paste0("Number of ", ds, " patients in ", hb ," re-referred within 12 months")) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = 'none',
        axis.text.x = element_text(hjust = 0.25, vjust = 0))

re_ref_df_12mth_plot

  
#Query 2
#Number of repeat referrals following an earlier referral that was not accepted.

source("07_publication/script/functions/append_quarter_ending.R")

re_ref_df_12mth_rej_acc <- df |>
  filter(dataset_type == 'CAMHS') |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti) |>
  group_by(hb_name, chi) |>
  mutate(count = n_distinct(ucpn)) |>
  filter(count >= 2) |>
  mutate(last_ref_date = lag(ref_rec_date_opti),
         last_acc_status = lag(ref_acc_last_reported)) |>
  filter(!is.na(last_ref_date),
         ref_acc_last_reported == 1 & last_acc_status == 2,
         last_ref_date >= ref_rec_date_opti %m-% months(12)) |>
  ungroup() |> select(-count) |>
  append_quarter_ending(date_col = "ref_rec_date_opti") |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  summarise(count = n())

#In the above analysis the quarter in which a patient is counted is based on the
#referral received date of the second or subsequent referral rather than the date of the 
#initial referral. Only accepted referrals after a rejected referral are inlcuded.
#A patient may be counted in more than once.

ds <- "CAMHS"
hb <- 'NHS Western Isles'

re_ref_df_12mth_rej_acc_plot <- re_ref_df_12mth_rej_acc |>
  filter(dataset_type == ds,
         hb_name == hb,
         quarter_ending >= month_start) |>
  ggplot(aes(x = quarter_ending, y = count)) +
  geom_line(colour = "#9B4393", linewidth = 1) +
  geom_point(colour = "#9B4393", size = 2) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(breaks = sort(unique(re_ref_df_12mth_rej_acc$quarter_ending)),
               date_labels = "%b %Y") +
  labs(y = "Number of patients re-referred within 12mths",
       x = "Quarter ending",
       title = paste0("Number of ", ds, " patients in ", hb ," re-referred within 12 months after referral non-acceptance")) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = 'none',
        axis.text.x = element_text(hjust = 0.25, vjust = 0))

re_ref_df_12mth_rej_acc_plot

#Query 3
# Number of repeat referrals subsequently accepted for assessment or intervention.
# For this I've only looked at cases where a referral was rejected and then a subsequent accepted 
# within 12 months, rather than all re-referrals

re_ref_df_12mth_rej_assess <- df |>
  filter(dataset_type == 'CAMHS') |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported, rtt_eval) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti) |>
  group_by(hb_name, chi) |>
  mutate(count = n_distinct(ucpn)) |>
  filter(count >= 2) |>
  mutate(last_ref_date = lag(ref_rec_date_opti),
         last_acc_status = lag(ref_acc_last_reported)) |>
  filter(!is.na(last_ref_date),
         ref_acc_last_reported == 1 & last_acc_status == 2,
         last_ref_date >= ref_rec_date_opti %m-% months(12)) |>
  ungroup() |> select(-count) |>
  filter(rtt_eval %in% c("case closed after assessment",
                         "rtt not possible - attended app but no purpose",
                         "seen - active",
                         "seen - closed",
                         "waiting - after assessment")) |> #rtt eval labels that suggest patient was seen 
  append_quarter_ending(date_col = "ref_rec_date_opti") |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  summarise(count = n())


####################################.
### Patient Turnover line graphs ###
####################################.

#Creates a plot showing referrals, appointments + new starts, and discharges

#author: Bex Madden
#date: 04/04/2024


# Load data
#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

calculate_patient_turnover <- function(df){

#calculate discharges
df_disc <- df |>
  mutate(discharged = case_when(
    !!sym(case_closed_month_o) == !!sym(app_month_o) ~ "1",
    TRUE ~ "0"),
    discharged = as.numeric(discharged)) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o))) |>
  summarise(discharged = sum(discharged), .groups = "drop") |>
  ungroup() |>
  filter(!is.na(app_month))

  # make scotland-wide count for discharges
scot_disc <- df |>
  mutate(discharged = case_when(
    !!sym(case_closed_month_o) == !!sym(app_month_o) ~ "1",
    TRUE ~ "0"),
    discharged = as.numeric(discharged)) |>
  group_by(!!!syms(c(dataset_type_o, app_month_o))) |>
  summarise(discharged = sum(discharged), .groups = "drop") |> ## have to sum not n() to counts up 1 and 0
  ungroup() |>
  mutate(hb_name = "NHS Scotland") |>
  filter(!is.na(app_month))

df_disc <- bind_rows(df_disc, scot_disc)
df_disc <- df_disc |>
  rename(month = app_month)


# calculate new starts (new apps)
df_new <- df |>
  filter(!!sym(att_cat_o) == 1) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o))) |>
  summarise(new_start = n(), .groups = "drop") |> ## here can use n() as have done a filter
  ungroup()

# scotland-wide counts for new starts
scot_new <- df |>
  filter(!!sym(att_cat_o) == 1) |>
  group_by(!!!syms(c(dataset_type_o, app_month_o))) |>
  summarise(new_start = n(), .groups = "drop") |>
  ungroup() |>
  mutate(hb_name = "NHS Scotland") 

df_new <- bind_rows(df_new, scot_new)
df_new <- df_new |>
  rename(month = app_month)


# calculate accepted referrals
df_ref = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  mutate(!!ref_acc_last_reported_o := case_when(!!sym(ref_acc_last_reported_o) == 1 ~ 'accepted',
                                              !!sym(ref_acc_last_reported_o) == 2 ~ 'not accepted',
                                              TRUE ~ 'pending')) |>                                        ### pending doesn't give info on when ref was made
  group_by(!!!syms(c(referral_month_o, hb_name_o, dataset_type_o, ref_acc_last_reported_o))) |>             #### if exclude this gives total refs? just by counting referral month?
  summarise(n = n(), .groups = 'drop') |>
  ungroup()

# scotland-wide count of acccepted referrals
scot_ref = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  mutate(!!ref_acc_last_reported_o := case_when(!!sym(ref_acc_last_reported_o) == 1 ~ 'accepted',
                                                !!sym(ref_acc_last_reported_o) == 2 ~ 'not accepted',
                                                TRUE ~ 'pending')) |>                                       
  group_by(!!!syms(c(referral_month_o, dataset_type_o, ref_acc_last_reported_o))) |>             
  summarise(n = n(), .groups = 'drop') |>
  ungroup() |>
  mutate(hb_name = "NHS Scotland") 

df_ref <- bind_rows(df_ref, scot_ref)
df_ref <- df_ref |>
  spread(ref_acc_last_reported, n) |>
  rename(month = referral_month)



# calculate total referrals
df_refall = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  group_by(!!!syms(c(referral_month_o, hb_name_o, dataset_type_o))) |>           
  summarise(n = n(), .groups = 'drop') |>
  ungroup()

# scotland-wide count of total referrals
scot_refall = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  group_by(!!!syms(c(referral_month_o, dataset_type_o))) |> 
  summarise(n = n(), .groups = 'drop') |>
  ungroup() |>
  mutate(hb_name = "NHS Scotland") 

df_refall <- bind_rows(df_refall, scot_refall)
df_refall <- df_refall |>
  rename(month = referral_month,
         all_refs = n)


# join together by hb dataset and month
df_turnover <- list(df_disc, df_new, df_ref, df_refall) |>
  reduce(full_join, by = c('month', 'dataset_type', 'hb_name')) 

rm(df_disc,
     scot_disc,
     df_new,
     scot_new,
     df_ref,
     scot_ref,
     df_refall,
     scot_refall)

# Make plots

plot_turnover <- function(df_turnover, ds_type){
  
df_plot <- df_turnover |>
  filter(dataset_type == ds_type,
         month > (most_recent_month_in_data  %m-% months(15))) |>
  select(-('not accepted':pending)) |>
  pivot_longer(c('discharged', 'new_start', 'accepted', 'all_refs'), 
               names_to = 'measure', values_to = 'n') |>
  mutate(measure = recode(measure, 
                          'discharged' = "Patients Discharged",
                          'new_start' = "New Starts",
                         'accepted' = "Accepted Referrals", 
                         'all_refs' = "Total Referrals")) |>
 
   ggplot(aes(x = month, y = n,
             group = measure, colour = measure,
             text = paste0("Health Board: ", hb_name, "<br>",
                           "Appointment month: ", month, "<br>",
                           "Measure: ", measure, "<br>",
                           "Value: ", n))) +
  geom_line() +
  geom_point() +
  theme_minimal()+
  scale_colour_manual(values=c("#3F3685", "#9B4393", "#0078D4", "#83BB26"))+
  ylab("Number")+
  xlab("Appointment month")+
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %y")+
  labs(title = paste0("Patient turnover by month - ",
                    ds_type),
       colour= "")+
  theme(plot.title = element_text(hjust = 0.5, size = 25))+
  facet_wrap(~factor(hb_name, levels = c(level_order)), scales = "free_y")+
  theme(panel.spacing.x = unit(-1, "lines"),
        panel.spacing.y = unit(0, "lines"))+
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        legend.position ="bottom",
        axis.text.x = element_text(size = 12, margin = margin(t = 0, r = 0, b = 40, l = 0), 
                                   angle = 90, hjust = 1, vjust = 0), 
        axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 12))


fig1 = ggplotly(df_plot, tooltip = "text") 

htmlwidgets::saveWidget(
  widget = fig1, #the plotly object
  file = paste0(patient_turnover_dir,
                '/plot_patient_turnover_',
                ds_type,
                ".html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)
}


plot_turnover(df_turnover, "PT")
plot_turnover(df_turnover, "CAMHS")


}

# check weird future submissions
#test <- filter(df, referral_month == "2024-03-01")
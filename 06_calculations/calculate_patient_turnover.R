
####################################.
### Patient Turnover line graphs ###
####################################.

#Creates a plot showing referrals, appointments + new starts, and discharges

#author: Bex Madden
#date: 04/04/2024


# Load data

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 


#### DERIVE METRICS ####

#discharges
df_disc <- df |>
  mutate(discharged = case_when(
    !!sym(case_closed_month_o) == !!sym(app_month_o) ~ "1",
    TRUE ~ "0"),
    discharged = as.numeric(discharged)) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o))) |>
  summarise(discharged = sum(discharged), .groups = "drop") |>
  ungroup() |>
  filter(!is.na(app_month))

  # make scotland-wide count
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


#new starts (new apps)
df_new <- df |>
  filter(!!sym(att_cat_o) == 1) |>                      ### att cat 1 should only happen once per refferral?
  group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o))) |>
  summarise(new_start = n(), .groups = "drop") |> ## here can use n() as have done a filter
  ungroup()

  #scotland-wide
scot_new <- df |>
  filter(!!sym(att_cat_o) == 1) |>
  group_by(!!!syms(c(dataset_type_o, app_month_o))) |>
  summarise(new_start = n(), .groups = "drop") |>
  ungroup() |>
  mutate(hb_name = "NHS Scotland") 

df_new <- bind_rows(df_new, scot_new)
df_new <- df_new |>
  rename(month = app_month)


# accepted referrals

# from calculate referrals 
df_ref = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  mutate(!!ref_acc_last_reported_o := case_when(!!sym(ref_acc_last_reported_o) == 1 ~ 'accepted',
                                              !!sym(ref_acc_last_reported_o) == 2 ~ 'not accepted',
                                              TRUE ~ 'pending')) |>                                        ### pending doesn't give info on when ref was made
  group_by(!!!syms(c(referral_month_o, hb_name_o, dataset_type_o, ref_acc_last_reported_o))) |>             #### if exclude this gives total refs? just by counting referral month?
  summarise(n = n(), .groups = 'drop') |>
  ungroup()

# scotland-wide
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



# total refs per month (??)
df_refall = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  group_by(!!!syms(c(referral_month_o, hb_name_o, dataset_type_o))) |>           
  summarise(n = n(), .groups = 'drop') |>
  ungroup()

#scotland-wide
scot_refall = df |>
  select(all_of(data_keys),!!ref_acc_last_reported_o, !!referral_month_o) |>
  distinct() |>
  group_by(!!!syms(c(referral_month_o, dataset_type_o))) |>            
  ungroup() |>
  mutate(hb_name = "NHS Scotland") 

df_refall <- bind_rows(df_refall, scot_refall)
df_refall <- df_refall |>
  rename(month = referral_month,
         all_refs = n)



#### COMBINE DATA ####

# join together by hb dataset and month
df_turnover <- list(df_disc, df_new, df_ref, df_refall) |>
  reduce(full_join, by = c('month', 'dataset_type', 'hb_name')) |>
  filter(month > (most_recent_month_in_data  %m-% months(15)))


ds_type = "CAMHS"
df_plot <- filter(df_turnover, dataset_type == ds_type)


#### GENERATE PLOTS ####

# plotly


#plot <- df_turnover %>% 

plot_to <- ggplot(df_plot) +
  geom_line(aes(x = month, y = discharged, colour = "Patients Discharged"))+
  geom_point(aes(x = month, y = discharged, colour = "Patients Discharged"))+
  geom_line(aes(x = month, y = new_start, colour = "New Patients"))+
  geom_point(aes(x = month, y = new_start, colour = "New Patients"))+
  geom_line(aes(x = month, y = accepted, colour = "Accepted Referrals"))+
  geom_point(aes(x = month, y = accepted, colour = "Accepted Referrals"))+
  geom_line(aes(x = month, y = all_refs, colour = "Total Referrals"))+
  geom_point(aes(x = month, y = all_refs, colour = "Total Referrals"))+
  theme_minimal()+
  scale_colour_manual(values=c("#3F3685",
                               "#9B4393",
                               "#0078D4",
                               "#83BB26"))+
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
        legend.text = element_text(size = 15))


fig1 = ggplotly(plot_to, tooltip = "text") 

htmlwidgets::saveWidget(
  widget = fig1, #the plotly object
  file = paste0(patient_turnover_dir,
                '/plot_patient_turnover_',
                ds_type,
                ".html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)

# check weird future submissions
#test <- filter(df, referral_month == "2024-03-01")


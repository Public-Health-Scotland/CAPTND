
#appts missing referral
appts_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  group_by(dataset_type, hb_name, ucpn) |>
  mutate(is_ref_record = !is.na(ref_date) | !is.na(ref_rec_date),
         has_ref_record_ucpn = any(is_ref_record),
         chi_missing_on_ref = any(is_ref_record & is.na(chi)),
         n_chi_per_ucpn = n_distinct(chi, na.rm = TRUE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         !is.na(app_purpose)) |>
  select(dataset_type, hb_name, ucpn, chi, app_date, app_purpose, 
         has_ref_record_ucpn, chi_missing_on_ref, n_chi_per_ucpn, header_date) |>
  distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  arrange(dataset_type, hb_name, ucpn, app_date)
  
#has_ref_record_ucpn
#will be TRUE if a referral record exists for the UCPN but the CHI on the original referral record does not match the CHI on
#the flagged appointment record below.
has_ref_record_ucpn <- appts_missing_refs |>
  filter(has_ref_record_ucpn == TRUE) |>
  select(dataset_type, hb_name, ucpn, chi) |>
  distinct()
  
#chi_missing_on_ref
#will be TRUE if a referral record exists for the UCPN bu the CHI was submitted as NA
chi_missing_on_ref <- appts_missing_refs |>
  filter(chi_missing_on_ref == TRUE) |>
  select(dataset_type, hb_name, ucpn, chi) |>
  distinct()

#n_chi_per_ucpn
#n_chi_ucpn provides a count of the number of CHIs provided for the same UCPN
n_chi_per_ucpn <- appts_missing_refs |>
  filter(n_chi_per_ucpn >= 2) |>
  select(dataset_type, hb_name, ucpn, chi) |>
  distinct()

#first month of submission
first_contact <- appts_missing_refs |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  slice_head(n = 1) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

ds <- 'CAMHS'
hb <- 'NHS Western Isles'

first_contact_plot <- first_contact |>
  filter(dataset_type == ds,
         hb_name == hb,
         header_date >= '2024-01-01')

dates <- first_contact_plot |> # make date labels for quarters
  select(header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  unique() |>
  pull()

lims = round_any(max(first_contact_plot$count), 5, f = ceiling) # set upper limit of y axis

#plot timetrend
ggplot(first_contact_plot, aes(x = header_date, y = count)) +
  geom_line(linewidth = 1, colour = "#9B4393") +
  geom_point(size = 1.5, colour = "#9B4393") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0, lims)) +
  labs(x = "Month",
       y = "Count",
       title = paste0(ds, " appointments without referral record in ", hb)) +
  theme_captnd() +
  theme(panel.grid.major.y = element_line(),
        axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))



#number of pathways - raw dataset
raw_pathway_count <- df |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  select(dataset_type, hb_name, ucpn, chi, header_date) |> 
  group_by(dataset_type, hb_name, ucpn, chi) |>
  arrange(dataset_type, hb_name, ucpn, header_date) |>
  slice_head(n = 1) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(Raw = n()) |> ungroup()
  
#number of pathways - opti dataset
df_opti <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

opti_pathway_count <- df_opti |>
  select(dataset_type, hb_name, ucpn, patient_id, header_date) |> 
  group_by(dataset_type, hb_name, ucpn, patient_id) |>
  arrange(dataset_type, hb_name, ucpn, header_date) |>
  slice_head(n = 1) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(Opti = n()) |> ungroup()

pathway_comparison <- raw_pathway_count |>
  left_join(opti_pathway_count, by = c("dataset_type", "hb_name", "header_date")) |>
  filter(header_date >= '2023-01-01') |>
  pivot_longer(cols = c('Raw', 'Opti'), names_to = "raw_vs_opti", values_to = "count")
  

ds <- 'CAMHS'
hb <- 'NHS Western Isles'

comparison_plot <- pathway_comparison |>
  filter(dataset_type == ds,
         hb_name == hb)

dates <- comparison_plot |> # make date labels for quarters
  select(header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  unique() |>
  pull()

lims = round_any(max(comparison_plot$count), 5, f = ceiling) # set upper limit of y axis

#plot timetrend
ggplot(comparison_plot, aes(x = header_date, y = count,
                            colour = raw_vs_opti)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0, lims)) +
  scale_colour_manual(values = phs_colors(c("phs-purple", "phs-magenta"))) +
  labs(x = "Month",
       y = "Count",
       title = paste0(ds, " raw vs opti pathway count in ", hb),
       colour = "Dataset type") +
  theme_captnd() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "top",
        legend.title = element_text(size = 12, face = "bold"),
        legend.box.spacing = unit(0, "cm"),
        legend.key.height = unit(10, "pt"),
        legend.key.width = unit(30, "pt"),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))


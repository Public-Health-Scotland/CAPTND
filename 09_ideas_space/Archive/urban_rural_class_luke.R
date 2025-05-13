
##############################################################
###  Investigate Attendance by Urban_Rural Classification  ###
##############################################################

source('07_publication/update_2024_06/functions/append_quarter_ending.R')


apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
dir.create(apps_att_dir)
measure_label <- "dnas_"

# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  select(!!!syms(c(data_keys, postcode_last_reported_o, app_date_o, app_month_o, att_status_o, 
                   sex_reported_o, age_group_o)), ur8_2020_name) |>
  filter(!!sym(app_month_o) %in% date_range) |>
  append_quarter_ending(date_col = 'app_month')

# 2 - calculate attendance rate status-----------------------------------
df_att_status <- df |> 
  filter(!is.na(!!sym(app_date_o))) |>  # must have app_date
  # add attendance status description      
  mutate(!!sym(att_status_desc_o) := case_when(
    !!sym(att_status_o) == 1 ~ "seen",
    !!sym(att_status_o) == 2 ~ "cancelled by NHS",
    !!sym(att_status_o) == 3 ~ "CNA",
    !!sym(att_status_o) == 8 ~ "DNA",
    is.na(!!sym(att_status_o)) ~ "no info",
    TRUE ~ "other")) 

# 4 - DNA counts by urban_rural-------------------------------------------
df_dna_count <- df_att_status |> 
  filter(!!sym(att_status_desc_o) == 'DNA') |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, att_status_desc_o)), quarter_ending, ur8_2020_name) |> 
  summarise(dna_count = n(),.groups = "drop") |> 
  ungroup() |> 
  group_by(!!!syms(c(dataset_type_o, att_status_desc_o)), quarter_ending, ur8_2020_name) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  group_by(!!!syms(c(dataset_type_o, hb_name_o)), quarter_ending) |> 
  mutate(tot = sum(dna_count, na.rm = TRUE),
         perc = round(dna_count/tot*100, 1)) 

# 5 - Appt counts by urban_rural-----------------------------------------
df_appts <- df %>% 
  filter(!is.na(!!sym(app_date_o))) %>% 
  group_by(across(all_of(c(hb_name_o, dataset_type_o))), quarter_ending, ur8_2020_name) %>% 
  summarise(app_count = n(),
            .groups = 'drop') %>% 
  group_by(across(all_of(c(dataset_type_o))), quarter_ending, ur8_2020_name) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop"))

# 6 - DNA Rates by Urban/Rural Class----------------------------
df_dna_rates <- df_appts %>%
  left_join(df_dna_count, by = c(dataset_type_o, hb_name_o, 'quarter_ending', 'ur8_2020_name')) %>%
  mutate(count = ifelse(is.na(dna_count), 0, dna_count)) %>%
  select(!!!syms(c(hb_name_o, dataset_type_o)), quarter_ending, ur8_2020_name, app_count, dna_count) %>%
  mutate(dna_rate = round(dna_count / app_count *100, 1)) |>
  mutate(ur8_2020_name = as.factor(ur8_2020_name)) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "urban_rural_hb_qr"))

  
# 6 - Scottish DNA Rates by Urban/Rural Class----------------------------


create_bar_chart_dna_ur <- function(hb, dataset_choice){
  
  df_dna_ur <- read_parquet(paste0(apps_att_dir, measure_label, "urban_rural_hb_qr.parquet")) |> 
    ungroup() |>
    filter(!!sym(hb_name_o) == hb,
           !!sym(dataset_type_o) == ds,
           !is.na(ur8_2020_name),
           quarter_ending == max(quarter_ending))
  
  pal <- phsstyles::phs_colours(c("phs-blue", "phs-teal-30", "phs-blue-80", "phs-teal-50", 
                                  "phs-blue-50","phs-teal-80", "phs-blue-30", "phs-teal"))
  
  lims = round_any(max(df_dna_ur$dna_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(df_dna_ur, aes(x = ur8_2020_name, y = dna_rate, fill = ur8_2020_name)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(dna_rate, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_manual(values = pal) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Urban Rural Classification",
      y = "DNA Rate (%)",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none",
          axis.text.x = element_text(angle=45, vjust=1, hjust=1)) #+
    #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
}

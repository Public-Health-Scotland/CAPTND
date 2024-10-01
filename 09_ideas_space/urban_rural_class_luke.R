
##############################################################
###  Investigate Attendance by Urban_Rural Classification  ###
##############################################################

source('07_publication/update_2024_06/functions/append_quarter_ending.R')

# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

df <- append_quarter_ending(df, app_month_o)

# 2 - load postcode lookup files-----------------------------------------
postcode_dir <- ("/conf/linkage/output/lookups/Unicode/Geography")

postcode_lookup <- read_parquet(paste0(postcode_dir,
                                       '/Scottish Postcode Directory/Scottish_Postcode_Directory_2024_2.parquet'))

postcode_lookup <- postcode_lookup %>%
  select(pc8, ur8_2020_name)

df <- df %>%
  mutate(!!postcode_last_reported_o := format_postcode(!!sym(postcode_last_reported_o), format = 'pc8')) %>%
  left_join(postcode_lookup, by = join_by(!!sym(postcode_last_reported_o) == pc8), multiple = "all")

# 3 - calculate attendance rate status-----------------------------------
df_att_status <- df |> 
  filter(!is.na(!!sym(app_date_o))) |>  # must have app_date
  # add attendance status description      
  mutate(!!sym(att_status_desc_o) := case_when(
    !!sym(att_status_o) == 1 ~ "seen",
    !!sym(att_status_o) == 2 ~ "cancelled by NHS",
    !!sym(att_status_o) == 3 ~ "CNA",
    !!sym(att_status_o) == 8 ~ "DNA",
    is.na(!!sym(att_status_o)) ~ "no info",
    TRUE ~ "other")) |>
  select(!!!syms(c(data_keys, postcode_last_reported_o, app_month_o, att_cat_o, att_status_desc_o, 
                   sex_reported_o, age_group_o)), ur8_2020_name, quarter_ending) 

# 4 - DNA counts by urban_rural-------------------------------------------
df_dna_count <- df_att_status |> 
  filter(!!sym(att_status_desc_o) == 'DNA') |>
  # count by hb
  group_by(!!!syms(c(dataset_type_o, hb_name_o, att_status_desc_o)), quarter_ending, ur8_2020_name) |> 
  summarise(n_dnas = n(),.groups = "drop") |> 
  ungroup() |> 
  # count for all HBs combined (NHS Scotland)
  group_by(!!!syms(c(dataset_type_o, att_status_desc_o)), quarter_ending, ur8_2020_name) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  # add totals and calculate percentages
  group_by(!!!syms(c(dataset_type_o, hb_name_o)), quarter_ending) |> 
  mutate(tot_dnas = sum(n_dnas, na.rm = TRUE)) 

# 5 - Appt counts by urban_rural-----------------------------------------

df_app_number <- df %>% 
  filter(!is.na(!!sym(app_date_o))) %>% 
  group_by(across(all_of(c(hb_name_o, dataset_type_o))), quarter_ending, ur8_2020_name) %>% 
  summarise(n_app = n(),
            .groups = 'drop') %>% 
  group_by(across(all_of(c(dataset_type_o))), quarter_ending, ur8_2020_name) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop"))

# 6 - Scottish DNA Rates by Urban/Rural Class----------------------------
df_dna_rates <- df_app_number %>%
  left_join(df_dna_count, by = c(dataset_type_o, hb_name_o, 'quarter_ending', 'ur8_2020_name')) %>%
  mutate(n_dnas = ifelse(is.na(n_dnas), 0, n_dnas)) %>%
  select(!!!syms(c(hb_name_o, dataset_type_o)), quarter_ending, ur8_2020_name, n_app, n_dnas) %>%
  mutate(dna_rate = round(n_dnas / n_app *100, 1))

  
# 6 - Scottish DNA Rates by Urban/Rural Class----------------------------
df_dna_rates <- df_dna_rates %>% 
  filter(!!sym(hb_name_o) == 'NHS Scotland' & 
           !!sym(dataset_type_o) == 'PT' & 
           quarter_ending == '2024-06-01' & 
           ur8_2020_name != 'NA')
  

  pal <- phsstyles::phs_colours(c("phs-blue", "phs-blue-80", "phs-blue-50",
                                  "phs-blue-30", "phs-teal", "phs-teal",
                                  "phs-teal-80", "phs-teal-50", "phs-teal-30"))
  
  lims = round_any(max(df_dna_rates$dna_rate) + 3, 2.5) # set upper limit of y axis
  
  chart_urru_dna <- df_dna_rates %>% 
    ggplot(aes(x = ur8_2020_name, y = dna_rate, fill = ur8_2020_name)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(dna_rate, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_manual(values = pal) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Urban Rural Classification",
      y = "DNA rate (%)",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  chart_urru_dna





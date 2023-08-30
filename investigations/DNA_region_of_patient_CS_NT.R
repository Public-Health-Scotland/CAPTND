
####################################################################.
### DNA rate for those living within and outwith HB of treatment ###
####################################################################.

# Author: Charlie Smith
# Date: 2023-08-30

# Plan: establish DNA rate for those living in HB of treatment vs. those living outwith HB of treatment

# load CAPTND data (for testing only)
df <- read_parquet("../../../output/df_glob_swift_completed_2023-08-25.parquet") %>% 
  ungroup()

# create function 
dna_rate_within_outwith_hb <- function(df){
  
  # load postcode hb lookup
  df_postcode_hb_lookup <- import("../../../../R script/CAPTND Data Prep/Data/Basefiles/Postcode_2020_HB_LA.xlsx") %>% 
    rename(!!sym(hb_name_o) := HB, 
           !!sym(postcode_o) := Postcode) %>% 
    mutate(hb_residence := case_when(
      str_detect(!!sym(hb_name_o), regex('ayr|aa|a&a', ignore_case = TRUE)) ~ 'NHS Ayrshire and Arran',
      str_detect(!!sym(hb_name_o), regex('bor', ignore_case = TRUE)) ~ 'NHS Borders',
      str_detect(!!sym(hb_name_o), regex('dumf|d&g', ignore_case = TRUE)) ~ 'NHS Dumfries and Galloway',
      str_detect(!!sym(hb_name_o), regex('fife', ignore_case = TRUE)) ~ 'NHS Fife',
      str_detect(!!sym(hb_name_o), regex('forth|fv', ignore_case = TRUE)) ~ 'NHS Forth Valley',
      str_detect(!!sym(hb_name_o), regex('gram', ignore_case = TRUE)) ~ 'NHS Grampian',
      str_detect(!!sym(hb_name_o), regex('glas|gg', ignore_case = TRUE)) ~ 'NHS Greater Glasgow and Clyde',
      str_detect(!!sym(hb_name_o), regex('high', ignore_case = TRUE)) ~ 'NHS Highland',
      str_detect(!!sym(hb_name_o), regex('lanar', ignore_case = TRUE)) ~ 'NHS Lanarkshire',
      str_detect(!!sym(hb_name_o), regex('loth', ignore_case = TRUE)) ~ 'NHS Lothian',
      str_detect(!!sym(hb_name_o), regex('ork', ignore_case = TRUE)) ~ 'NHS Orkney',
      str_detect(!!sym(hb_name_o), regex('shet|z', ignore_case = TRUE)) ~ 'NHS Shetland',
      str_detect(!!sym(hb_name_o), regex('tay', ignore_case = TRUE)) ~ 'NHS Tayside',
      str_detect(!!sym(hb_name_o), regex('west|wi', ignore_case = TRUE)) ~ 'NHS Western Isles',
      str_detect(!!sym(hb_name_o), regex('2', ignore_case = TRUE)) ~ 'NHS24',
      str_detect(!!sym(hb_name_o), regex('scot', ignore_case = TRUE)) ~ 'NHS Scotland',
      TRUE ~ !!sym(hb_name_o)), 
      !!sym(postcode_o) := str_remove_all(!!sym(postcode_o), " ")) %>%
    select(-c("DZ", "LA", sym(hb_name_o)))
  
  # get DNA rate by dataset, HB # (and month/quarter?)
  df_dna_rate_sco <- df %>%
    select(!!!syms(c(
      sub_source_o,
      app_date_o, 
      dataset_type_o,
      hb_name_o,
      att_status_o,
      postcode_last_reported_o))) %>% 
    filter(!is.na(!!sym(app_date_o)) & # must have app date
             !is.na(!!sym(postcode_last_reported_o)) & # must have completed postcode
             !!sym(att_status_o) != 99) %>% # att status must be known
    left_join(., df_postcode_hb_lookup, by = c("postcode_last_reported" = "postcode")) %>% 
    mutate(hb_match = case_when(
      !!sym(hb_name_o) == hb_residence ~ "hb_match",
      !!sym(hb_name_o) != hb_residence ~ "no_hb_match",
      TRUE ~ "check"), 
      dna_status = case_when(
        !!sym(att_status_o) == 8 ~ "dna",
        TRUE ~ "no dna")) %>% 
    filter(hb_match != "check") %>% # removed for simplicity for now
    group_by(!!sym(dataset_type_o), hb_match, dna_status) %>% 
    summarise(app_count = n()) %>% 
    ungroup() %>% 
    arrange(!!sym(dataset_type_o), hb_match) %>% 
    group_by(!!sym(dataset_type_o), hb_match) %>% 
    mutate(app_total = sum(app_count),
      rate = round( app_count / app_total * 100, 2)) %>% 
    filter(dna_status == "dna") %>% 
    arrange(!!sym(dataset_type_o), hb_match) %>% 
    ungroup() %>% 
    mutate(hb_name = "NHS Scotland")
  
  df_dna_rate_hb <- df %>%
    select(!!!syms(c(
      sub_source_o,
      app_date_o, 
      dataset_type_o,
      hb_name_o,
      att_status_o,
      postcode_last_reported_o))) %>% 
    filter(!is.na(!!sym(app_date_o)) & # must have app date
             !is.na(!!sym(postcode_last_reported_o)) & # must have completed postcode
             !!sym(att_status_o) != 99) %>% # att status must be known
    left_join(., df_postcode_hb_lookup, by = c("postcode_last_reported" = "postcode")) %>% 
    mutate(hb_match = case_when(
      !!sym(hb_name_o) == hb_residence ~ "hb_match",
      !!sym(hb_name_o) != hb_residence ~ "no_hb_match",
      TRUE ~ "check"), 
      dna_status = case_when(
        !!sym(att_status_o) == 8 ~ "dna",
        TRUE ~ "no dna")) %>% 
    filter(hb_match != "check") %>% # removed for simplicity for now
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), hb_match, dna_status) %>% 
    summarise(app_count = n()) %>% 
    ungroup() %>% 
    arrange(!!sym(dataset_type_o), hb_match) %>% 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), hb_match) %>% 
    mutate(app_total = sum(app_count),
           rate = round( app_count / app_total * 100, 2)) %>% 
    filter(dna_status == "dna") %>% 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), hb_match) %>% 
    ungroup()
  
  # join
  df_dna_rate <- rbind(df_dna_rate_hb, df_dna_rate_sco)
  
  
  # direct HB comparison
  df_dna_comp <- df_dna_rate %>%
    select(-c(app_count, app_total, dna_status)) %>%
    pivot_wider(names_from = "hb_match", values_from = "rate")
  
  return(df_dna_comp)
  
}




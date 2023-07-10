###############################################.
###  Percentage of unusable records report  ###
###############################################.

# 1 Load libraries and packages ---------------------------------------------
library(dplyr)
library(lubridate)
library(readr)
source('setup/new_colnames.R')


# 2 Function ----------------------------------------------------------------

report_unusable_records <- function(df_raw, saveName) {
  
  
  df_stats <- df_raw %>%
    # null_to_na() %>% 
    # correct_hb_names() %>% 
    mutate(dateSub=ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
    select(!!hb_name_o,!!dataset_type_o,!!ucpn_o,!!patient_id_o,dateSub) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),dateSub) %>% 
    summarise(totalRows=n(),
              patient_id_na=sum(is.na(!!sym(patient_id_o))),
              ucpn_na=sum(is.na(!!sym(ucpn_o))),
              ucpn_and_patient_id_na=sum(is.na(!!sym(ucpn_o)) & is.na(!!sym(patient_id_o))),
              ucpn_or_patient_id_na=sum(is.na(!!sym(ucpn_o)) | is.na(!!sym(patient_id_o)))) %>% 
    ungroup() %>% 
    group_by(dateSub, !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"NHS Scotland"))) %>% 
    group_by(!!sym(hb_name_o)) %>%
    mutate(patient_id_na_perc= round((patient_id_na * 100)/totalRows, 3),
              ucpn_na_perc= round((ucpn_na * 100)/totalRows, 3),
              ucpn_and_patient_id_na_perc= round((ucpn_and_patient_id_na * 100)/totalRows, 3),
              ucpn_or_patient_id_na_perc= round((ucpn_or_patient_id_na * 100)/totalRows, 3),
           .after=dateSub) %>% 
    ungroup() 
  
  df_stats %>% filter(dateSub>(max(dateSub)- years(1))) %>% 
    mutate(dateSub=ym(format(dateSub, "%Y-%m"))) %>% 
    ggplot( aes(x=dateSub, y=ucpn_or_patient_id_na_perc, group=dataset_type, colour=dataset_type)) +
    geom_line()+
    geom_point()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_date(
      minor_breaks = NULL,
      breaks = seq.Date(
        from = min(df_stats$dateSub),
        to = max(df_stats$dateSub),
        by = "month"))+
    facet_wrap(~ hb_name)
  
  savingLocation <- paste0("../../../output/removed/stats_on_unusable_records_", 
                           saveName,
                           "_")
  
  ggsave(paste0(savingLocation,
                'plot_',
                as.character(today()),
                ".png"),
         width = 20,
         height = 16,
         units = c("cm"),
         dpi = 300,
         bg='white')
  
  
  write_csv(df_stats, paste0(savingLocation,
                             "table_",
                             as.character(today()),
                             ".csv"))
  
  message(paste0('Stats on removed records due to lack of one of the key variables
                 Patient ID and/or UCPN was saved to\n',
                 savingLocation, 
                 "{table/plot}",
                 as.character(today()),
                  "{.csv/.png}"))

}

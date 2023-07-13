###############################################.
###  Percentage of unusable records report  ###
###############################################.

# 1 Load libraries and packages ---------------------------------------------
library(dplyr)
library(lubridate)
library(readr)
source('config/new_colnames.R')


# 2 Function ----------------------------------------------------------------

report_unusable_records <- function(df_raw, saveName) {
  
  timePeriod=1 #time in years that the report will report on. 
  level_order <- c('NHS Ayrshire and Arran',
                   'NHS Borders',
                   'NHS Dumfries and Galloway',
                   'NHS Fife',
                   'NHS Forth Valley',
                   'NHS Grampian',
                   'NHS Greater Glasgow and Clyde',
                   'NHS Highland',
                   'NHS Lanarkshire',
                   'NHS Lothian',
                   'NHS Orkney',
                   'NHS Shetland',
                   'NHS Tayside',
                   'NHS Western Isles',
                   'NHS24',
                   'NHS Scotland')
  #calculates number and percentages of missing ucpns and patient ids. 
  #records are considered unusable if missing ucpn or patient id
  
  df_stats <- df_raw %>%
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
    select(!!hb_name_o,!!dataset_type_o,!!ucpn_o,!!patient_id_o,!!submission_date_o) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
    summarise(!!total_rows_o:=n(),
              #patient_id_na=sum(is.na(!!sym(patient_id_o))),
              #ucpn_na=sum(is.na(!!sym(ucpn_o))),
              #ucpn_and_patient_id_na=sum(is.na(!!sym(ucpn_o)) & is.na(!!sym(patient_id_o))),
              ucpn_or_patient_id_na=sum(is.na(!!sym(ucpn_o)) | is.na(!!sym(patient_id_o))),
              .groups = "drop") %>% 
    group_by(!!sym(submission_date_o), !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"NHS Scotland"),
                        .groups = "keep")) %>% 
    group_by(!!sym(hb_name_o)) %>%
    mutate(#patient_id_na_perc= round((patient_id_na * 100)/!!sym(total_rows_o), 3),
              #ucpn_na_perc= round((ucpn_na * 100)/!!sym(total_rows_o), 3),
              #ucpn_and_patient_id_na_perc= round((ucpn_and_patient_id_na * 100)/!!sym(total_rows_o), 3),
              ucpn_or_patient_id_na_perc= round((ucpn_or_patient_id_na * 100)/!!sym(total_rows_o), 3),
           .after=!!submission_date_o) %>% 
    ungroup() 
  
  #plot removed records
  df_stats %>% filter(!!sym(submission_date_o)>(max(!!sym(submission_date_o))- years(timePeriod))) %>% 
    mutate(!!submission_date_o:=ym(format(!!sym(submission_date_o), "%Y-%m"))) %>% 
    ggplot( aes(x=submission_date, y=ucpn_or_patient_id_na_perc, group=dataset_type, colour=dataset_type)) +
    geom_line()+
    geom_point()+
    theme_minimal()+
    ylab("Removed records due to lack of patient id and ucpn")+
    xlab("Submission date")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_date(
      minor_breaks = NULL,
      breaks = seq.Date(
        from = min(df_stats$submission_date),
        to = max(df_stats$submission_date),
        by = "month"))+
    labs(colour= "Dataset type")+
    facet_wrap(~factor(hb_name, levels=c(level_order)))+
    theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
    theme(legend.position="bottom")
  
  savingLocation <- paste0("../../../output/removed/", 
                           saveName,
                           "_removed_missing_pat_id_ucpn_")
  
  ggsave(paste0(savingLocation,
                'plot_',
                as.character(today()),
                ".png"),
         width = 27,
         height = 20,
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
                  "{.csv/.png}\n"))

}

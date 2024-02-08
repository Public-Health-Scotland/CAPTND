#######################################################################.
###  Percentage of removed rows due to UPI not being unique report  ###
#######################################################################.

# 1 Load libraries and packages ---------------------------------------------
# library(dplyr)
# library(lubridate)
# library(readr)


# 2 Function ----------------------------------------------------------------

report_upi_mult_chi <- function(df, chis_per_upi_with_chis_to_remove, saveName) {
  
  timePeriod=1 #time in years that the report will plotted. 
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
  
  df_total_rows = df %>% 
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m")),
           .after=!!file_id_o) %>% 
    group_by(!!sym(dataset_type_o),!!sym(submission_date_o),!!sym(hb_name_o)) %>% 
    summarise(!!total_rows_o:=n(),
              .groups = "drop") %>% 
    group_by(!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"NHS Scotland"),
                        .groups = "keep")) 
    

  df_stats <- chis_per_upi_with_chis_to_remove %>%
    inner_join(df, by=c(hb_name_o, dataset_type_o, upi_o, chi_o)) %>% 
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m")),
           .after=!!file_id_o) %>% 
    group_by(!!sym(submission_date_o), !!sym(dataset_type_o), !!sym(hb_name_o)) %>% 
    summarise(removed_rows=n(), .groups = 'keep') %>% 
    group_by(!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"NHS Scotland"),
                        .groups = "drop")) %>% 
    inner_join(df_total_rows, by=c(hb_name_o,dataset_type_o,submission_date_o)) %>% 
    mutate(perc_removed=(removed_rows*100)/!!sym(total_rows_o),
           issue='removed_upi_multiple_chi')
  
  
  #plot removed records
  # p=df_stats %>% filter(!!sym(submission_date_o)>(max(!!sym(submission_date_o))- years(timePeriod))) %>% 
  #   mutate(!!submission_date_o:=ym(format(!!sym(submission_date_o), "%Y-%m"))) %>% 
  #   ggplot( aes(x=submission_date, 
  #               y=perc_removed, 
  #               group=dataset_type, 
  #               colour=dataset_type,
  #               text = paste0(
  #                 "Health Board: ", hb_name, "<br>",
  #                 "Sumbmission date: ", submission_date, "<br>",
  #                 "% rows removed: ", perc_removed, "<br>",
  #                 "n rows removed: ", removed_rows,"<br>")
  #               )) +
  #   geom_line()+
  #   geom_point()+
  #   theme_minimal()+
  #   ylab("Removed rows with multiple UPI and NA as CHI")+
  #   xlab("Submission date")+
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #   scale_x_date(
  #     minor_breaks = NULL,
  #     breaks = seq.Date(
  #       from = min(df_stats$submission_date),
  #       to = max(df_stats$submission_date),
  #       by = "month"))+
  #   labs(colour= "Dataset type")+
  #   facet_wrap(~factor(hb_name, levels=c(level_order)))+
  #   theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
  #   theme(legend.position="bottom")
  # 
  savingLocation <- paste0(stats_removed_dir,'/',
                           saveName,
                           "_removed_upi_multiple_chi_stats")
  # 
  # pl=ggplotly(p,tooltip = "text")
  # 
  # htmlwidgets::saveWidget(
  #   widget = pl, #the plotly object
  #   file = paste0(savingLocation,
  #                 as.character(DATA_FOLDER_LATEST),
  #                 ".html"), #the path & file name
  #   selfcontained = TRUE #creates a single html file
  # )
  
  # suppressWarnings( ggsave(paste0(savingLocation,
  #               'plot_',
  #               as.character(DATA_FOLDER_LATEST),
  #               ".png"),
  #        width = 27,
  #        height = 20,
  #        units = c("cm"),
  #        dpi = 300,
  #        bg='white'))
  # 
  
  write_csv(df_stats, paste0(savingLocation,
                             ".csv"))
  
  message(paste0('Stats on removed rows with UPIs associated with multiple patients and no CHI was saved in\n',
                 data_removed_dir))
  
}

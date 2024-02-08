#######################################################################.
### Produce report for removed rows in data quality phase of CAPTND ###
#######################################################################

#Author: Joaa Bittencourt Silvestre
#Date: 17/08/2023


# 1-Load libraries --------------------------------------------------------
# library(dplyr)
# library(readr)
# library(purrr)
# library(ggplot2)
# library(lubridate)
# library(plotly)
# library(htmlwidgets)
# library(phsmethods)
# library(stringr)



report_removed_rows <- function() {
  
  
  # 2-Functions and variables for use in this script ------------------------
  
  # last_date_on_file <- list.files(path = "../../../output/removed/", pattern ="swift.*\\.csv$", full.names = FALSE) %>% 
  #   map_chr(~str_extract(.,'\\d{4}.+\\d{2}.+\\d{2}')) %>% max(.)
  
  
  # 2.1-Variables -----------------------------------------------------------
  
  # df <- list.files(path = "../../../output/removed/", pattern ="swift.*\\.csv$", full.names = TRUE) %>% 
  #   map(~.[str_detect(.,last_date_on_file)]) %>% 
  #   map_df(~read_csv(.,show_col_types = FALSE)) 
  
  df <- list.files(path=stats_removed_dir,pattern ="swift.*\\.csv$", full.names = TRUE) %>% 
    map_df(~read_csv(.,show_col_types = FALSE))
  
  df_month <- df %>%
    #mutate(across(where(is.numeric), round,2))
    mutate(across(where(is.numeric),\(x) round(x,2)))
    # %>% 
    # group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(submission_date_o)) %>%
    # bind_rows(summarise(.,
    #                     across(all_of(c('perc_removed','removed_rows')), sum),
    #                     across(all_of(c('total_rows')), max),
    #                     across(where(is.character), ~"total removed"),
    #                     .groups = "drop"))
   
  
  df_year= df %>% 
    mutate(fin_year=extract_fin_year(!!sym(submission_date_o))) %>%  
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o), fin_year,issue) %>% 
    summarise(across(all_of(c('removed_rows','total_rows')), sum),
              across(all_of(c('perc_removed')), ~(removed_rows*100)/total_rows),
              .groups = "drop") %>%
    mutate(across(where(is.numeric), round,2))
  
  
  last15months = max(df$submission_date, na.rm = T)- months(15)
  
  df_quarter= df %>% 
    filter(!!sym(submission_date_o)>last15months) %>% 
    mutate(submission_quarter=phsmethods::qtr(!!sym(submission_date_o), format=c('short'))) %>%
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),issue,submission_quarter) %>% 
    summarise(across(all_of(c('removed_rows','total_rows')), sum),
              across(all_of(c('perc_removed')), ~(removed_rows*100)/total_rows),
              .groups = "drop") %>%
    mutate(across(where(is.numeric), round,2))
    
    
  quarterOrder=df %>% 
    select(!!submission_date_o) %>% 
    mutate(submission_quarter=phsmethods::qtr(!!sym(submission_date_o), format=c('short'))) %>%
    arrange(!!sym(submission_date_o)) %>% 
    select(-!!submission_date_o) %>% 
    distinct() %>% 
    pull(submission_quarter)
  
  month_order= df %>% 
    select(!!submission_date_o) %>% 
    arrange(!!sym(submission_date_o)) %>% 
    distinct() %>% 
    mutate(!!submission_date_o := format(!!sym(submission_date_o), "%b\n%y")) %>% 
    pull(!!submission_date_o)
  
  
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
  
  # 2.2-Plot maker ----------------------------------------------------------
  
  make_trend_month <- function(df,ds){
    
    df1=df %>% filter(dataset_type==ds) %>% 
      mutate(issue=gsub('_removed','',issue),
             issue=gsub('_',' ',issue))
    
    savingLocation <- paste0(stats_removed_dir,"/", ds,"_removed_rows_breakdown")
    
    
    timePeriod <- 1 #time in years that the report will report on.
    
    minDate <- max(df$submission_date, na.rm = T)- years(timePeriod)
    
    p1 <- df1 %>% filter(!!sym(submission_date_o)>ymd(minDate)) %>% 
      mutate(!!submission_date_o := format(!!sym(submission_date_o), "%b\n%y")) %>% 
          ggplot( aes(x=factor(submission_date, levels=c(month_order)), 
                  y=perc_removed, 
                  group=issue, 
                  colour=issue,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Submission date: ", gsub('\n','-',submission_date), "<br>",
                    "Removal reason: ", issue, "<br>",
                    "% of rows removed: ", perc_removed, "<br>",
                    "n rows removed: ",removed_rows
                  ))) +
      geom_line()+
      geom_point()+
      theme_minimal()+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("percentage of rows removed")+
      xlab("Submission date")+
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      # scale_x_date(
      #   minor_breaks = NULL,
      #   breaks = seq.Date(
      #     from = min(df1$submission_date, na.rm = T),
      #     to = max(df1$submission_date, na.rm = T),
      #     by = "month"))+
      labs(title=paste0("Percentage of removed rows in ",ds," data cleaning by submission month"),
           colour= "Reason for removal")+
      theme(plot.title = element_text(hjust = 0.5, size = 25))+
      facet_wrap(~factor(hb_name, levels=c(level_order)))+
      theme(panel.spacing.x= unit(1, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=11, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))
    
   
    fig1=ggplotly(p1, tooltip = "text") 
    
    
    
    htmlwidgets::saveWidget(
      widget = fig1, #the plotly object
      file = paste0(savingLocation,
                    'plot_',
                    'month.html'), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    
    write_csv(df1, paste0(savingLocation,
                          "table_month.csv"))
  }
  
  
  make_bar_plot_yearly <- function(df, ds){
    
    df1=df %>% filter(dataset_type==ds) 
    
    savingLocation <- paste0(stats_removed_dir,"/", ds,"_removed_rows_breakdown")
    
    barsPlt_prep = df1 %>% 
      mutate(issue=gsub('removed_','',issue)) %>% 
      ggplot(aes(factor(hb_name, level = level_order), 
                 perc_removed, 
                 fill=issue, 
                 group=issue,
                 text = paste0(
                   "Health Board: ", hb_name, "<br>",
                   "Financial Year: ", fin_year, "<br>",
                   "Removal reason: ", gsub('_',' ',issue), "<br>",
                   "% of rows removed: ", perc_removed, "<br>",
                   "n rows removed: ",removed_rows)
      )) 
    
    
    
    p2 <- barsPlt_prep +
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
      scale_fill_manual(values=c("#3F3685",
                                 "#9B4393",
                                 "#0078D4",
                                 "#83BB26"))+
      labs(title=paste0("Percentage of removed rows in ",ds," data cleaning by fiscal year"),
           fill='Reason for removal', 
           x='health board',
           y='percentage of rows removed') +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      theme(legend.position="bottom")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~fin_year)+
      theme(panel.spacing = unit(1, "lines"))
    
    fig2=ggplotly(p2,tooltip = "text")
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(savingLocation,
                    'plot_',
                    'year.html'), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    write_csv(df1, paste0(savingLocation,
                          "table_year.csv"))
    
  }
  
  
  make_bar_plot_quarterly <- function(df, ds){
    
    df1=df %>% filter(dataset_type==ds) 
    
    savingLocation <- paste0(stats_removed_dir,"/", ds,"_removed_rows_breakdown")
    
    barsPlt_prep = df1 %>% 
      mutate(issue=gsub('removed_','',issue)) %>% 
      ggplot(aes(factor(submission_quarter, level = quarterOrder), 
                 perc_removed, 
                 fill=issue, 
                 group=issue,
                 text = paste0(
                   "Health Board: ", hb_name, "<br>",
                   "Quarter: ", submission_quarter, "<br>",
                   "Removal reason: ", gsub('_',' ',issue), "<br>",
                   "% of rows removed: ", perc_removed, "<br>",
                   "n rows removed: ",removed_rows)
      )) 
    
    
    
    p2 <- barsPlt_prep +
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
      scale_fill_manual(values=c("#3F3685",
                                 "#9B4393",
                                 "#0078D4",
                                 "#83BB26"))+
      labs(title=paste0("Percentage of removed rows in ",ds," data cleaning by quarter"),
           fill='Reason for removal', 
           x='health board',
           y='percentage of rows removed') +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      theme(legend.position="bottom")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales = 'free_y')+
      theme(panel.spacing.x= unit(0, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=13, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    fig2=ggplotly(p2,tooltip = "text")
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(savingLocation,
                    'plot_',
                    'quarter.html'), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    write_csv(df1, paste0(savingLocation,
                          "table_quarter.csv"))
    
  }
  
  
  
  # 3-Making plots ----------------------------------------------------------
  
  #make_bar_plot_quarterly(df_quarter,'CAMHS')
  # make_bar_plot_yearly(df_year,'CAMHS')
  make_trend_month(df_month,'CAMHS')
  # 
  # make_bar_plot_quarterly(df_quarter,'PT')
  # make_bar_plot_yearly(df_year,'PT')
  make_trend_month(df_month,'PT')

  message(paste0('Plots and tables with stats on removed rows saved in\n',
                 stats_removed_dir))

}




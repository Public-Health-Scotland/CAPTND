source('setup/set_col_data_types.R')

early_file <- read_parquet(paste0(root_dir,'/swift_glob_merged.parquet')) %>% 
  set_col_data_types() 




flag_data_after_subm_date <- function(early_file) {
  
  
  col_names_date <- c("ref_date" ,
                    "ref_rec_date", 
                    "ref_rej_date" ,
                    "app_date", 
                    "treat_start_date",
                    "case_closed_date",
                    "header_date",
                    "act_code_sent_date")  
  
  col_name_o <- 'col_name'
  date_o <-'date_col'
  sub_month_end_o <- 'sub_month_end'
  
  df_sub_month_end <- early_file %>%
    mutate(sub_month_end=ceiling_date(!!sym(header_date_o), unit = "month")-days(1)) %>% 
    filter(if_any(all_of(col_names_date), ~ .x > sub_month_end)) %>% 
    pivot_longer(cols=all_of(col_names_date), 
                 names_to = col_name_o, values_to = date_o) %>% 
    filter(date_col > sub_month_end) %>% 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, col_name_o, sub_month_end_o))) %>% 
    summarise(n=n(), .groups = 'drop') %>% 
    group_by(!!!syms(c(dataset_type_o, col_name_o, sub_month_end_o))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) 
  
  
  plot_data_after_sub_date <- function(df_sub_month_end,ds_type) {
    
    max_month = max(df_sub_month_end$sub_month_end) %m-% months(15)
    
    
    p2 <- df_sub_month_end %>% 
          mutate(sub_month_end=floor_date(sub_month_end,unit = "month")) %>% 
          filter(!!sym(dataset_type_o)==ds_type,
                 sub_month_end > max_month) %>% 
          ggplot( aes(x=sub_month_end, 
                      y=n, 
                      group=col_name, 
                      colour=col_name,
                      text = paste0(
                        "Health Board: ", hb_name, "<br>",
                        "Submission month : ", format(sub_month_end,"%b-%y"), "<br>",
                        "Col name: ", col_name, "<br>",
                        "N: ", n
                      ))) +
          geom_line()+
          geom_point()+
          theme_minimal()+
          scale_colour_manual(values=c("#3F3685",
                                       "#9B4393",
                                       "#0078D4",
                                       "#83BB26",
                                       "#1E7F84",
                                       "#C73918"))+
          ylab("Number of records submitted after submission month end")+
          xlab("Submission month")+
          scale_x_date(
            date_breaks = "1 month",
            date_labels = "%b\n%y")+
          labs(title=paste0("Records submitted after submission month end - ",
                            ds_type),
               colour= "Dataset")+
          theme(plot.title = element_text(hjust = 0.5, 
                                          size = 30
                                          )
                )+
          facet_wrap(~factor(hb_name, levels=c(level_order)), scales="free_y")+
          theme(plot.margin = unit(c(0.5,0.5,2,2), "cm"),
                legend.position="bottom",
                axis.text.x = element_text(size=11, 
                                           margin = margin(t = 0, r = 0, b = 40, l = 0)
                                           ),
                axis.text.y = element_text(size = 14, 
                                           margin = margin(t = 0, r = 0, b = 0, l = 40)
                                           ),
                strip.text = element_text(size=14),
                axis.title=element_text(size=16),
                legend.text=element_text(size=14),
                legend.title = element_text(size=16),
                panel.spacing.x = unit(-1, "lines"),
                )
        
        
        fig2=ggplotly(p2, tooltip = "text") 
        
        htmlwidgets::saveWidget(
          widget = fig2, #the plotly object
          file = paste0(stats_checked_dir,
                        '/plot_subs_after_sub_month_end_',
                        ds_type,
                        ".html"), #the path & file name
          selfcontained = TRUE #creates a single html file
        )
  
  }
  
  plot_data_after_sub_date(df_sub_month_end, 'CAMHS')
  plot_data_after_sub_date(df_sub_month_end, 'PT')

}

flag_data_after_subm_date(early_file)

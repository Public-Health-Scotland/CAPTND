early_file <- read_parquet(paste0(root_dir,'/swift_glob_merged.parquet')) %>% 
  set_col_data_types() 




col_names_date <- c("ref_date" ,
                  "ref_rec_date", 
                  "ref_rej_date" ,
                  "app_date", 
                  "treat_start_date",
                  "case_closed_date",
                  "header_date",
                  "act_code_sent_date")  

col_name_o <- 'col_name'
date_o <-'date'
sub_month_end_o <- 'sub_month_end'

df_sub_month_end <- early_file %>%
  mutate(sub_month_end=ceiling_date(!!sym(header_date_o), unit = "month")-days(1)) %>% 
  filter(if_any(all_of(col_names_date), ~ .x > sub_month_end)) %>% 
  pivot_longer(cols=all_of(col_names_date), 
               names_to = col_name_o, values_to = date_o) %>% 
  filter(date > sub_month_end) %>% 
  group_by(!!!syms(c(dataset_type_o, hb_name_o, col_name_o, sub_month_end_o))) %>% 
  summarise(n=n(), .groups = 'drop') %>% 
  group_by(!!!syms(c(dataset_type_o, col_name_o, sub_month_end_o))) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop")) 
  


p2 <- df_sub_month_end %>% 
  #mutate() format to ex Jul-22
  #filter by ds_type
      ggplot( aes(x=sub_month_end, 
                  y=n, 
                  group=ds_type, 
                  colour=ds_type,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Submission month end: ", gsub('\n','-',sub_month_end), "<br>",
                    "Col name: ", col_name, "<br>",
                    "N: ", n
                  ))) +
      geom_line()+
      geom_point()+
      geom_ribbon(aes(ymin = 90, ymax = 110), fill = "grey70", alpha = .3, colour = NA)+
      theme_minimal()+
      geom_hline(yintercept=100, linetype='dashed', color="grey35")+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26",))+
      ylab("% similarity with aggregate")+
      xlab("First contact month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%y")+
      labs(title=paste0("First contact - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           colour= "")+
      theme(plot.title = element_text(hjust = 0.5, size = 25))+
      facet_wrap(~factor(hb_name, levels=c(level_order)))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            panel.spacing = unit(1, "lines"),
            axis.text.x = element_text(size=15, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    
    fig2=ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(first_contact_dir,
                    '/plot_comp_aggreg_captnd_first_cont_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )


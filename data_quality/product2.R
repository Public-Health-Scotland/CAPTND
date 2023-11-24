##################################.
### Product 2 - data wrangling ###
##################################.

#author: Charlie Smith & JBS
#date: 23/11/23


# 1 Load libraries --------------------------------------------------------

#library()


# 2 Function --------------------------------------------------------------

calculate_product2_df <- function(df){

  
  df_rtt2 <- df %>% 
    filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>% 
    group_by(!!!syms(data_keys)) %>% 
    
    mutate(
      ref_acc_last_reported := last(!!sym(ref_acc_o),order_by=!!sym(header_date_o), na_rm = TRUE),
      .after=rtt_eval
    ) %>% 
    
    mutate(
      has_any_app_date = case_when(any(!is.na(app_date)) ~ TRUE,
                                TRUE ~ FALSE),
      has_ref_rec_date_opti = case_when(any(!is.na(ref_rec_date_opti)) ~ TRUE,
                                         TRUE ~ FALSE),
      has_act_code_sent_date = case_when(any(!is.na(act_code_sent_date)) ~ TRUE,
                                          TRUE ~ FALSE),
      is_case_closed = case_when(any(!is.na(case_closed_date)) ~ TRUE,
                                        TRUE ~ FALSE),
      .after=rtt_eval
    ) %>% 
    
    mutate(rtt_possible=case_when(
      
      #first case is patients seen whose treatment is ongoing
      has_ref_rec_date_opti == TRUE &
        has_any_app_date == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(2,3,5)
        ) ~ 'seen - active',
      
      #next case is patients seen whose treatment is finished
      has_ref_rec_date_opti == TRUE &
        has_any_app_date == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(2,3,5)
      ) ~ 'seen - closed',
      
      #second case is patients who had online treatment which is still ongoing
      has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        has_any_app_date == FALSE &
        ref_acc_last_reported == 1 &
      any(
          has_act_code_sent_date == TRUE
        ) ~ 'seen - online - active',
      
      #another case is patients who had online treatment that has finished
      has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        has_any_app_date == FALSE &
        ref_acc_last_reported == 1 &
      any(
          has_act_code_sent_date == TRUE 
      ) ~ 'seen - online - closed',
      
      # patients assessed and then discharged
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(1,4,6)
        
      ) &
        !any(
          !is.na(!!sym(app_date_o)) &
            !!sym(app_purpose_o) %in% c(2,3,5) 
        ) ~ 'case closed after assessment',
      
      #patients waiting (assessed but no treatment)
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) == 1 &
          !!sym(app_purpose_o) %in% c(1,4,6)
      ) ~ 'waiting - after assessment',
      
      #rtt not possible - no app purpose information
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        (is.na(!!sym(app_purpose_o)) | !!sym(app_purpose_o) == 99) &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) == 1 
      ) ~ 'rtt not possible - attended app but no purpose',
      
      #rtt not possible - no app attendance information
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
        (!!sym(att_status_o) == 99 | is.na(!!sym(att_status_o))) 
      ~ 'rtt not possible - app date but no attendance status',
      
      
      #case closed due to no attendance
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) %in% c(2,3,5,8) 
      ) ~ 'case closed due to non attendance',
      
      #patients waiting no attendance
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1 &
      any(
          !is.na(!!sym(app_date_o)) &
          !!sym(att_status_o) %in% c(2,3,5,8) 
      ) ~ 'waiting - not attended',
      
      #case closed prior to app
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == TRUE &
        ref_acc_last_reported == 1 
       ~ 'case closed with no app',
      
      #patients waiting prior to app
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
        ref_acc_last_reported == 1
       ~ 'waiting - no app',
      
      
      #referral pending
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
        is_case_closed == FALSE &
      (ref_acc_last_reported == 3 | is.na(ref_acc_last_reported))
       ~ 'referral pending',
      
      #referral rejected
      has_any_app_date == FALSE &
        has_ref_rec_date_opti == TRUE &
      ref_acc_last_reported == 2 
       ~ 'referral not accepted',
      
      #no ref acc but app date
      has_any_app_date == TRUE &
        has_ref_rec_date_opti == TRUE &
        is.na(!!sym(ref_acc_o)) 
      ~ 'rtt_not possible - app with no referral acc',
      
      TRUE ~ 'rtt_not possible - unknown'),
      .after=rtt_eval) %>% 
    ungroup()
  
  df_rtt_stats <- df_rtt2 %>% 
    filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>% 
    select(all_of(data_keys),rtt_possible) %>% 
    distinct() %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o)),rtt_possible) %>% 
    summarise(n=n(),
              .groups='drop') 
    
  
  df_rtt_plot_prep <- df_rtt_stats %>% 
    mutate(rtt=case_when(str_detect(rtt_possible, 'unknown|not possible') ~ 'not possible',
                         TRUE ~ 'possible')) %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o)),rtt) %>% 
    summarise(n=sum(n),
              .groups='drop') %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o))) %>% 
    inner_join(summarise(.,
                         across(where(is.numeric), sum),
                         .groups = "drop"),
               by=c(hb_name_o,dataset_type_o)) %>% 
    rename(count=`n.x`,
           total=`n.y`) %>% 
    mutate(percentage = round(count/total * 100, 1)) %>% 
    filter(rtt == 'possible' ) %>% 
    mutate(traffic_light=case_when(percentage > 89.9 ~ '90 to 100%',
                                   percentage <= 89.9 & 
                                     percentage >= 70 ~ '70 to 89.9%',
                                   percentage <70 ~ '0 to 69.9%')) %>% 
    inner_join(df_rtt_stats, by=c(hb_name_o,dataset_type_o)) %>% 
    mutate(percentage_subdivision = round(n/total * 100, 1))
    
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%"="#B3D7F2",#blue
                             "0 to 69.9%"="#D26146") #rust 80%
  
  
  
  product2_plot <- df_rtt_plot_prep %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order),
           a='') %>%  
    ggplot(aes_string(y = hb_name_o, x = 'a', fill = 'traffic_light')) + 
    geom_tile(width = 0.5, height = 0.9, size = .25, color = "black")+ 
    geom_text(aes(label = percentage), size = 3)+
    scale_fill_manual(values = traffic_light_colours, name = '% of pathways where rtt is possible', drop = FALSE)+
    theme_minimal()+
    theme(
      legend.key = element_rect(fill = "white", colour = "black"),
      plot.caption = element_text(hjust = 0))+
    facet_wrap(~ dataset_type)+
    labs(title = paste0("CAPTND: Percentage of pathways where RTT is possible by healthboard"),
         #subtitle = "Rows not containing data keys are excluded from further analysis",
         caption=paste0("Source: CAPTND - Date: ", Sys.Date()),
         x = NULL,
         y = NULL)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(strip.background = element_rect(
       fill="white", size=1, linetype="solid"),
      plot.caption = element_text(hjust = 1, size = 6)
    )
  
  
  ggsave(paste0(product2_dir,'/product2.png'),
         width=22,
         height=13.5,
         units='cm',
         dpi = 300,
         bg='white')
  
  
  
    
   barsPlt_prep = df_rtt_plot_prep %>%
      mutate(hb_name=factor(hb_name, level = level_order),
             rtt_possible=factor(rtt_possible, level = c('seen - active',#1
                                                          'seen - closed', #2
                                                          'seen - online - active', #3
                                                          'seen - online - closed', #
                                                          'waiting - after assessment',
                                                          'waiting - not attended',
                                                          'waiting - no app',
                                                          'referral pending',
                                                          'referral not accepted',
                                                          'case closed after assessment',
                                                          'case closed with no app',
                                                          'case closed due to non attendance',
                                                          'rtt_not possible - app with no referral acc',
                                                          'rtt not possible - app date but no attendance status',
                                                          'rtt not possible - attended app but no purpose',
                                                          'rtt_not possible - unknown'
                                                               ))) %>% #15
     mutate(rtt_ev=case_when(str_detect(rtt_possible,'rtt not possible') ~ 'rtt not possible',
                          TRUE ~ 'rtt possible')) 
      
    x_axis_colours=barsPlt_prep %>% 
     select(hb_name,dataset_type,traffic_light) %>% 
     distinct() %>% 
     mutate(colour_axis=case_when(
       traffic_light=="90 to 100%" ~ "#9CC951", # green 80%
       traffic_light==  "70 to 89.9%"~"#B3D7F2",#blue
       TRUE ~"#D26146" 
     )) %>% pull(colour_axis)

    p <- barsPlt_prep %>% 
     ggplot(aes(hb_name, 
                percentage_subdivision, 
                fill=rtt_possible, 
                group=rtt_possible,
                text = paste0(
                  "Health Board: ", hb_name, "<br>",
                  "RTT eval: ", rtt_possible, "<br>",
                  "% pathways: ", percentage_subdivision, "<br>",
                  "n pathways: ", n,"<br>"#,
                  #'PMS: ',sub_system
                )
     )) +
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
      #scale_fill_discrete_phs()+
      scale_fill_manual(values=c(
        #purples
        "#3F3685",
        "#3F3685",
        "#3F3685",
        "#3F3685",
        
        "#655E9D",
        "#655E9D",
        "#655E9D",
        
        "#9F9BC2",
        "#9F9BC2",
        
        "#C5C3DA",
        "#C5C3DA",
        "#C5C3DA",
        #"#ECEBF3",
        #blue
        #"#0078D4",
        #magenta
         "#9B4393",
        # "#AF69A9",
        # "#CDA1C9",
        #reds
        #'#751A04',
        # '#751A04',
        # '#751A04',
        # '#751A04',
         '#902004',
        '#902004',
        '#902004'
        # "#C73918",
        # "#D26146",
        # "#E39C8C",
        # "#EEC4BA",
        # '#FFE8E2',
        # "#F9EBE8"
      ))+
      labs(title=paste0("Percentage of pathways where RTT is possible "),
           fill='RTT evaluation', 
           x='health board',
           y='% pathways') +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      theme(legend.position="bottom")+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    p2=p+
      facet_wrap(~dataset_type)+
      theme(panel.spacing = unit(1, "lines"))
    
    
    fig2=ggplotly(p2,tooltip = "text")
    
    pname=paste0(product2_dir,'/product2_details',
                 '.html')
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = pname, #the path & file name
      selfcontained = TRUE #creates a single html file
    )
  
}



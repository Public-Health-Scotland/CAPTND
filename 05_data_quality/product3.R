##################################.
### Product 3 - data wrangling ###
##################################.

#author: JBS
#date: 20/02/24


# 1 Load libraries --------------------------------------------------------


# 2 Make product 2 --------------------------------------------------------

make_product_3 <- function(df, most_recent_month_in_data) {
  
  #I only removed 98 as the option for ethnicity, since for other columns 98 offered information
  
  prep_df_for_plot <- function(df_filtered) {
    df_prep1 <- df_filtered %>%
      filter(!!sym(ref_rec_date_opti_o) >= ymd(210801) &
               !!sym(rtt_eval_o) == 'seen - closed') %>%
      mutate(across(where(is.character), ~na_if(., "99")),
             across(ethnicity, ~na_if(., "98")),
             across(where(is.numeric), ~na_if(., 99))
             ) %>% 
      group_by(across(all_of(data_keys))) %>% 
      mutate(!!postcode_o := first(!!sym(postcode_o), na_rm = TRUE),
             !!sex_o := first(!!sym(sex_o), na_rm = TRUE),
             !!dob_o := first(!!sym(dob_o), na_rm = TRUE),
             !!ethnicity_o := first(!!sym(ethnicity_o), na_rm = TRUE),
             !!vet_o := first(!!sym(vet_o), na_rm = TRUE),
             !!looked_after_c_o := first(!!sym(looked_after_c_o), na_rm = TRUE),
             !!preg_perinatal_o := first(!!sym(preg_perinatal_o), na_rm = TRUE),
             !!act_code_sent_date_o := first(!!sym(act_code_sent_date_o), na_rm = TRUE),
             !!diag_1_o := first(!!sym(diag_1_o), na_rm = TRUE),
             !!diag_2_o := first(!!sym(diag_2_o), na_rm = TRUE),
             !!diag_3_o := first(!!sym(diag_3_o), na_rm = TRUE),
             !!treat_1_o := first(!!sym(treat_1_o), na_rm = TRUE),
             !!treat_2_o := first(!!sym(treat_2_o), na_rm = TRUE),
             !!treat_3_o := first(!!sym(treat_3_o), na_rm = TRUE),
             !!treat_group_or_ind_1_o := first(!!sym(treat_group_or_ind_1_o), na_rm = TRUE),
             !!treat_group_or_ind_2_o := first(!!sym(treat_group_or_ind_2_o), na_rm = TRUE),
             !!treat_group_or_ind_3_o := first(!!sym(treat_group_or_ind_3_o), na_rm = TRUE),
             !!treat_start_date_o := first(!!sym(treat_start_date_o), na_rm = TRUE),
             !!unav_date_start_o := first(!!sym(unav_date_start_o), na_rm = TRUE),
             !!unav_date_end_o := first(!!sym(unav_date_end_o), na_rm = TRUE),
             !!unav_days_no_o := first(!!sym(unav_days_no_o), na_rm = TRUE),
             !!unav_reason_o := first(!!sym(unav_reason_o), na_rm = TRUE),
             !!measure_1_o := first(!!sym(measure_1_o), na_rm = TRUE),
             !!measure_2_o := first(!!sym(measure_2_o), na_rm = TRUE),
             !!measure_3_o := first(!!sym(measure_3_o), na_rm = TRUE)) %>% 
      fill(!!sym(ref_rec_date_o),
           !!sym(ref_source_o),
           !!sym(ref_reason_o),
           !!sym(case_closed_date_o),
           .direction = c("downup")) 
   
  
   df_prep2 = df_prep1 %>% 
      select(all_of(data_keys), 
             !!ref_rec_date_o, 
             !!postcode_o,
             !!sex_o,
             !!ethnicity_o,
             !!dob_o,
             !!vet_o,
             !!looked_after_c_o,
             !!preg_perinatal_o,
             !!ref_source_o,
             !!ref_reason_o,
             !!unav_date_start_o,
             !!unav_date_end_o,
             !!unav_days_no_o,
             !!unav_reason_o,
             !!act_code_sent_date_o,
             !!diag_1_o,
             !!diag_2_o,
             !!diag_3_o,
             !!treat_1_o,
             !!treat_2_o,
             !!treat_3_o,
             !!treat_group_or_ind_1_o,
             !!treat_group_or_ind_2_o,
             !!treat_group_or_ind_3_o,
             !!treat_start_date_o,
             !!measure_1_o,
             !!measure_2_o,
             !!measure_3_o,
             !!case_closed_date_o) %>% 
      distinct() %>% 
      ungroup()  %>% 
     group_by(!!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
     mutate(total_rows_hb = n()) %>% 
     summarise_all(~(sum(!is.na(.)))) %>% 
     ungroup() %>% 
     group_by(!!sym(dataset_type_o)) %>% 
     bind_rows(summarise(.,
                         across(where(is.numeric), sum),
                         across(!!hb_name_o, ~"NHS Scotland"),
                         .groups = "drop")) %>% 
     ungroup() %>% 
     mutate(across(where(is.numeric), ~ round(./total_rows_hb*100,1), .names = "perc_{col}")) %>% 
     pivot_longer(starts_with('perc'), names_to = 'category', values_to = 'value') %>% 
     select(!!hb_name_o, !!dataset_type_o, category, value) 
     
     df_prep3 <- df_prep1 %>% 
       ungroup() %>% 
       filter(!is.na(!!sym(app_date_o))) %>% 
       select(all_of(data_keys), 
              !!app_date_o,
              !!app_purpose_o,
              !!att_status_o) %>% 
       group_by(!!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
       mutate(total_rows_hb = n()) %>% 
       summarise_all(~(sum(!is.na(.)))) %>% 
       ungroup() %>% 
       group_by(!!sym(dataset_type_o)) %>% 
       bind_rows(summarise(.,
                           across(where(is.numeric), sum),
                           across(!!hb_name_o, ~"NHS Scotland"),
                           .groups = "drop")) %>% 
       ungroup() %>% 
       mutate(across(where(is.numeric), ~ round(./total_rows_hb*100,1), .names = "perc_{col}")) %>% 
       pivot_longer(starts_with('perc'), names_to = 'category', values_to = 'value') %>% 
       select(!!hb_name_o, !!dataset_type_o, category, value) 
     
    
      df_prep_perc <- df_prep2 %>%
        bind_rows(df_prep3) %>% 
        mutate(traffic_light=case_when( value ==0 ~ "0%", 
                                        value > 0 & value <= 33 ~ "0-33%", 
                                        value > 33 & value <= 66 ~ "33-66%", 
                                        value > 66 & value <100 ~ "66-99%", 
                                        value == 100 ~ "100%"),
               value=as.character(value)) 
      
      df_all_boards = data.frame(hb_name = level_order) %>% 
        filter(!!sym(hb_name_o) != 'NHS24') %>% 
        cross_join(data.frame(dataset_type = c('CAMHS','PT'))) %>% 
        cross_join(data.frame(category=unique(df_prep_perc$category))) %>% 
        mutate(value=NA,
               traffic_light='Not available')
      
      df_boards_missing <- df_all_boards %>% anti_join(df_prep_perc, by=c(hb_name_o, dataset_type_o))
        
      df_prep_plot = df_prep_perc %>% bind_rows(df_boards_missing) %>% 
        mutate(value = case_when(!!sym(dataset_type_o)=='CAMHS' & category == 'perc_vet' ~ NA,
                                 !!sym(dataset_type_o)=='CAMHS' & category == 'perc_preg_perinatal' ~ NA,
                                 !!sym(dataset_type_o)=='PT' & category == 'perc_looked_after_c' ~ NA,
                                 !!sym(dataset_type_o)=='CAMHS' & category == 'perc_act_code_sent_date' ~ NA,
                                 TRUE ~ value),
               traffic_light = case_when(!!sym(dataset_type_o)=='CAMHS' & category == 'perc_vet' ~ "Not applicable",
                                         !!sym(dataset_type_o)=='CAMHS' & category == 'perc_preg_perinatal' ~ "Not applicable",
                                         !!sym(dataset_type_o)=='PT' & category == 'perc_looked_after_c' ~ "Not applicable",
                                         !!sym(dataset_type_o)=='CAMHS' & category == 'perc_act_code_sent_date' ~ "Not applicable",
                                         !!sym(dataset_type_o)=='PT' & category == 'perc_act_code_sent_date' ~ "Not expected",
                                         str_detect(category,"2|3") & !is.na(value) ~ "Not expected",
                                         TRUE ~ traffic_light),
               type=case_when(str_detect(category,"app_date|app_pupose|att_status") ~ 'appointment',
                              TRUE ~ 'general'))
  
     
     return(df_prep_plot)
  }
  
  
    
   
   plot_it <- function(df_prep_plot, recent_data) {
     
     traffic_light_colours <- c("0%" = "#D26146", # rust 80%
                                "0-33%" = "#e3b28c", # amber 50%
                                "33-66%" = "#e3dd8c", # yellow 50%
                                "66-99%" = "#C1DD93", # green 50%
                                "100%" = "#9CC951",# green 80%
                                "Not applicable" = "grey50",
                                "Not available" = 'grey75',
                                "Not expected" = "#F3F8E9") #green 10% 
     
     lev_cat=c('patient_id',
               'ucpn',
               'dob',
               'sex',
               'ethnicity',
               'postcode',
               'looked_after_c',
               'vet',
               'preg_perinatal',
               'ref_rec_date',
               'ref_source',
               'ref_reason',
               'unav_date_start',
               'unav_date_end',
               'unav_days_no',
               'unav_reason',
               'act_code_sent_date',
               'treat_start_date',
               'app_date',
               'app_purpose',
               'att_status',
               'diag_1',
               'diag_2',
               'diag_3',
               'treat_1',
               'treat_2',
               'treat_3',
               'treat_group_or_ind_1',
               'treat_group_or_ind_2',
               'treat_group_or_ind_3',
               'outcome_1',
               'outcome_2',
               'outcome_3',
               'case_closed_date')
     
     
     product3_plot_heatmap <- df_prep_plot %>% 
       filter(category != 'perc_total_rows_hb',) %>%
       mutate(category = gsub('perc_','',category)) %>% 
       ggplot(aes(y = fct_rev(factor(category, levels=c(lev_cat))), x = factor(!!sym(hb_name_o), levels=c(level_order)), fill = traffic_light)) + 
       geom_tile(width = 0.95, height = 0.9, linewidth = .25, color = "black")+ 
       geom_text(aes(label = value), size = 2)+
       scale_fill_manual(values = traffic_light_colours, 
                         name = '% of completeness ', 
                         drop = FALSE,
                         breaks = c("0%", # rust 80%
                                    "0-33%", # rust 50%
                                    "33-66%", # blue 30%
                                    "66-99%", # green 50%
                                    "100%",
                                    "Not expected",
                                    "Not available",
                                    "Not applicable"))+
       theme_minimal()+
       theme(
         legend.key = element_rect(fill = "white", colour = "black"),
         plot.caption = element_text(hjust = 0))+
       facet_wrap(~ dataset_type)+
       labs(title = paste0("CAPTND: Completeness of data in closed cases until ",recent_data),
            caption=paste0("This heatmap only includes patients who have been treated (attended at least one treatment appointment) and have been discharged.
  Source: CAPTND - Date: ", Sys.Date()),
            x = NULL,
            y = NULL)+
       theme(plot.title = element_text(hjust = 0.5))+
       theme(strip.background = element_rect(
         fill="white", size=1, linetype="solid"),
         plot.caption = element_text(hjust = 1, size = 6),
         panel.spacing= unit(1, "lines"),
         axis.text.y = element_text(size=6),
         strip.text = element_text(size=9),
         axis.text.x = element_text(size=6, angle = 90, vjust = 0.5, hjust=1),
         legend.text=element_text(size=6),
         legend.title=element_text(size=8),
         legend.key.size = unit(3, 'mm')
       )
       
     
     ggsave(paste0(product3_dir,
                   '/product3_until_',
                   recent_data,
                   '.png'),
            width=30,
            height=15,
            units='cm',
            dpi = 300,
            bg='white')
   }

   #Plots for the last 5 quarters
   
   date_2 = most_recent_month_in_data %m-% months(3)
   date_3 = most_recent_month_in_data %m-% months(6)
   date_4 = most_recent_month_in_data %m-% months(9)
   date_5 = most_recent_month_in_data %m-% years(1)
   
   df1 <- df %>% 
     prep_df_for_plot() %>% 
     plot_it(.,most_recent_month_in_data)
   
   
   df2 <- df %>% 
     filter(!!sym(header_date_o) <= date_2) %>% 
     prep_df_for_plot() %>% 
     plot_it(., date_2)
     
   
   df3 <- df %>% 
     filter(!!sym(header_date_o) <= date_3) %>% 
     prep_df_for_plot() %>% 
     plot_it(.,date_3)
   
   
   df4 <- df %>% 
     filter(!!sym(header_date_o) <= date_4) %>% 
     prep_df_for_plot() %>% 
     plot_it(.,date_4)
   
   df5 <- df %>% 
     filter(!!sym(header_date_o) <= date_5) %>% 
     prep_df_for_plot() %>% 
     plot_it(.,date_5)
   
   message(paste0("All plots are in ",product3_dir))

}
  



###############################################.
###  Percentage of unusable records report  ###
###############################################.

# 1 Load libraries and packages ---------------------------------------------
library(dplyr)
library(lubridate)
library(readr)
source('config/new_colnames.R')


# 2 Function ----------------------------------------------------------------

report_multiple_ethnicities <- function(df_with_ethnicities) {
  
  savingLocation <- paste0(ethniticies_dir,"/multiple_ethnicities_")
  
  
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
  
  #calculates number and percentages of multiple ethnicities recorded 

  ###Calculating detailed table----------------------------
  
  df_stats_detailed <- df_with_ethnicities  %>%
    filter(!(!!sym(ethnicity_evaluation_o)=='ok' & is.na(!!sym(ethnicity_edited_o)))) %>% 
    select(!!hb_name_o,!!dataset_type_o,!!patient_id_o,!!ethnicity_evaluation_o,!!ethnicity_edited_o) %>% 
    distinct() %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(ethnicity_evaluation_o),!!sym(ethnicity_edited_o)) %>% 
    summarise(n_ethn = n(),
              .groups = "drop") %>% 
    group_by(!!sym(dataset_type_o),!!sym(ethnicity_evaluation_o),!!sym(ethnicity_edited_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"NHS Scotland"),
                        .groups = "drop")) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(ethnicity_evaluation_o)) %>%
    mutate(n_ethn_eval=sum(n_ethn),
           .after=!!ethnicity_evaluation_o) %>% 
    ungroup()
    
  
  
  
  ###Saving detailed table to csv----------------------------------
  
  write_csv(df_stats_detailed, paste0(savingLocation,
                                      "detailed.csv"))
  
  
  
  ###Plotting detailed table-------------------------------------
  df_stats_detailed %>% 
    filter(!!sym(ethnicity_evaluation_o)=='multiple ethnicities') %>% 
    ggplot( aes(x=factor(hb_name, level = level_order), y=n_ethn, fill=ethnicity_edited)) +
    geom_bar(stat = "identity", position = "fill")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ylab('proportion of ethnicity')+
    xlab("")+
    labs(fill= "Ethnicities reported")+
    ggtitle("Porportion of ethnicities in individuals with multiple (2+) ethnicities reported")+
    facet_wrap(~ dataset_type)+
    theme(plot.margin = unit(c(3,0.5,0.5,0.5), "cm"))+
    labs(caption = "Some patients are present in multiple boards and have multiple ethnicities represented, although a single ethnicity is shown in a board.")+
    theme(plot.caption = element_text(hjust = 0))
  
  
  ggsave(paste0(savingLocation,
                'detailed_plot.png'),
         width = 26,
         height = 16,
         units = c("cm"),
         dpi = 300,
         bg='white')
  
  
  ###Calculate general table---------------------------------------
  df_stats_general <- df_stats_detailed %>%
    select(-c(!!ethnicity_edited_o,n_ethn)) %>% 
    distinct() %>% 
    ungroup() %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    mutate(total_ethn_eval=sum(n_ethn_eval),
           perc_multiple_ethn=(round((n_ethn_eval*100)/total_ethn_eval,2))) %>% 
    ungroup()
  
  
  ###Save detailed table to csv------------------------------------
  write_csv(df_stats_general, paste0(savingLocation,
                                     "general.csv"))
  
  
  ###Plot general table---------------------------------------------
  df_stats_general %>% 
    filter(!!sym(ethnicity_evaluation_o)=='multiple ethnicities') %>% 
    ggplot( aes(x=factor(hb_name, level = level_order), y=perc_multiple_ethn)) +
    geom_bar(stat = "identity", position = "stack")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ylab('% of patients with multiple ethnicities recorded')+
    xlab("")+
    facet_wrap(~ dataset_type)+
    theme(plot.margin = unit(c(3,0.5,0.5,0.5), "cm"))
  
  ggsave(paste0(savingLocation,
                'general_plot.png'),
         width = 20,
         height = 16,
         units = c("cm"),
         dpi = 300,
         bg='white')
  
 
  ###Write message------------------------------------------------
  
  message(paste0('Stats on multiple ethnicities saved in\n',
                 ethniticies_dir))

}

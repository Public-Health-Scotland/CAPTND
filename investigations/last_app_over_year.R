#Calculate the number of cases with last app > 1 year ago

# 1 Load libraries and packages ---------------------------------------------
library(dplyr)
library(lubridate)
library(readr)
library(arrow)
library(plotly)
library(ggplot2)
library(phsstyles)
source('config/new_colnames.R')


df=read_parquet('../../../output/df_glob_swift_completed_2023-08-25.parquet')

library(stringr)

#Get extract date
file_name <- 'df_glob_swift_completed_2023-08-25.parquet'
file_date <- ymd(str_extract(filename, '\\d+-\\d+-\\d+'))


#Extract apps with no case closed date
apps_df <- df %>% 
  mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>%  
  select(all_of(c(data_keys,vec_app_cols)),!!submission_date_o,
         !!case_closed_date_o ) %>% 
  distinct() %>% 
  filter(!is.na(!!sym(app_date_o)) & is.na(!!sym(case_closed_date_o)))

#Make df with these cases by hb_name and dataset_type 
open_cases_board <- apps_df %>% 
  group_by(!!sym(dataset_type_o),!!sym(hb_name_o)) %>% 
  summarise(open_cases = n(), .groups = 'drop')  


#Filter cases with last app > 1 year ago using extract date 
last_app_over_year <- apps_df %>%
  group_by(across(all_of(data_keys))) %>% 
  filter(app_date == last(!!sym(app_date_o))) %>% 
  mutate(extract_date = file_date) %>% 
  mutate(days_last_app = as.numeric(difftime(extract_date, app_date))) %>%
  filter(days_last_app > 365)

#Filter cases with last app > 2 years ago using extract date
last_app_over_2yrs <- apps_df %>%
  group_by(across(all_of(data_keys))) %>% 
  filter(app_date == last(!!sym(app_date_o))) %>% 
  mutate(extract_date = file_date) %>% 
  mutate(days_last_app = as.numeric(difftime(extract_date, app_date))) %>%
  filter(days_last_app > 730)




#Filter cases with last app > 1 year ago using today's date  
#last_app_over_year <- apps_df %>%
# group_by(across(all_of(data_keys))) %>% 
# filter(app_date == last(!!sym(app_date_o))) %>% 
# mutate(today = Sys.Date()) %>% 
# mutate(days_last_app = as.numeric(difftime(today, app_date))) %>%
# filter(days_last_app > 365)

#write_csv(last_app_over_year, paste0('../../../problems/last_app_over_year_detailed.csv')) 


#Count apps by hb_name and dataset_type
apps_over_year_by_board <- last_app_over_year %>% 
  group_by(!!sym(dataset_type_o),!!sym(hb_name_o)) %>% 
  summarise(apps_over_year = n(), .groups = 'drop')

apps_over_2yrs_by_board <- last_app_over_2yrs %>% 
  group_by(!!sym(dataset_type_o),!!sym(hb_name_o)) %>% 
  summarise(apps_over_year = n(), .groups = 'drop')

#table for chart - join 'open cases' to cases with last app>1 year
df_apps_over_year <- apps_over_year_by_board %>%
  left_join(open_cases_board, by=c(hb_name_o, dataset_type_o)) %>% 
  left_join(df_sub_system, by=c(hb_name_o, dataset_type_o)) %>% 
  group_by(!!sym(dataset_type_o)) %>% 
  mutate(perc_over_year=(round((apps_over_year*100)/open_cases,2))) %>% 
  ungroup()

df_sub_system=read_csv_arrow('../../../data/hb_sub_system.csv')

write_csv(df_apps_over_year, paste0('../../../problems/df_apps_over_year_summary.csv')) 

#Plots  

savingLocation <- paste0("../../../output/investigations/last_app_over_year_")


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

#Plot with numbers
df_apps_over_year %>% 
  ggplot( aes(x=factor(hb_name, level = level_order), y=apps_over_year, fill=hb_name)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('number of apps > 1 year')+
  xlab("")+
  labs(fill= "NHS Boards")+
  ggtitle("Number of last appointments more than 1 year ago")+
  facet_wrap(~ dataset_type)+
  theme(plot.margin = unit(c(3,0.5,0.5,0.5), "cm"))+
  theme(legend.position = "none")+
  coord_flip()

ggsave(paste0(savingLocation,
              'num_plot',
              ".png"),
       width = 26,
       height = 16,
       units = c("cm"),
       dpi = 300,
       bg='white')



#Plot with % of open cases last app over 1 year
p1 <- df_apps_over_year %>% 
  ggplot(aes(factor(hb_name, level = level_order), perc_over_year, 
             fill=hb_name,
             text = paste0(
               "Health Board: ", hb_name, "<br>",
               "% over 1 year: ", perc_over_year, "<br>",
               "n apps:", apps_over_year, "<br>",
               'PMS: ', sub_system))) +
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('% with last app over 1 year')+
  xlab("")+
  labs(fill= "NHS Boards")+
  ggtitle("Percentage of open cases with last app over 1 year ago")+
  facet_wrap(~ dataset_type)+
  theme(plot.margin = unit(c(3,0.5,0.5,0.5), "cm"))+
  theme(legend.position = "none") #+
#coord_flip()


#Plotly plot with PMS
fig1 <- ggplotly(p1,tooltip = "text")


pname=paste0('../../../output/investigations/app_over_year_plot',
             '.html')

htmlwidgets::saveWidget(
  widget = fig1, #the plotly object
  file = pname, #the path & file name
  selfcontained = TRUE #creates a single html file
)


#Plot with % of open cases last app over 2 years
p2 <- df_apps_over_2yrs %>% 
  ggplot(aes(factor(hb_name, level = level_order), perc_over_2yrs, 
             fill=hb_name,
             text = paste0(
               "Health Board: ", hb_name, "<br>",
               "% over 2 years: ", perc_over_2yrs, "<br>",
               "n apps:", apps_over_year, "<br>",
               'PMS: ', sub_system))) +
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('% with last app over 2 years')+
  xlab("")+
  labs(fill= "NHS Boards")+
  ggtitle("Percentage of open cases with last app over 2 years ago")+
  facet_wrap(~ dataset_type)+
  theme(plot.margin = unit(c(3,0.5,0.5,0.5), "cm"))+
  theme(legend.position = "none") #+
#coord_flip()


#Plotly plot last app > 2yrs with PMS
fig2 <- ggplotly(p2,tooltip = "text")


pname=paste0('../../../output/investigations/app_over_2yrs_plot',
             '.html')

htmlwidgets::saveWidget(
  widget = fig2, #the plotly object
  file = pname, #the path & file name
  selfcontained = TRUE #creates a single html file
)








  
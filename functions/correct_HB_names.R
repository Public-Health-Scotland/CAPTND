##########################.
###   correct_HB_names ###
##########################.

#Changes misspelled/weird heath board names to its correct name


# 1 Load libraries and col names ------------------------------------------

source('setup/new_colnames.R')
source("./functions/save_df_as_parquet.R")
library(dplyr)
library(stringr)



# 2 Function --------------------------------------------------------------

correct_hb_names <- function(df){
  
  #Change misspelled health board names to correct names
  df_hb_evaluated=df %>%
    mutate(!!hb_name_o := case_when(str_detect(!!sym(hb_name_o), regex('ayr|aa|a&a', ignore_case = TRUE)) ~ 'NHS Ayrshire and Arran',
                                  str_detect(!!sym(hb_name_o), regex('bor', ignore_case = TRUE)) ~ 'NHS Borders',
                                  str_detect(!!sym(hb_name_o), regex('dumf|d&g', ignore_case = TRUE)) ~ 'NHS Dumfries and Galloway',
                                  str_detect(!!sym(hb_name_o), regex('fife', ignore_case = TRUE)) ~ 'NHS Fife',
                                  str_detect(!!sym(hb_name_o), regex('forth|fv', ignore_case = TRUE)) ~ 'NHS Forth Valley',
                                  str_detect(!!sym(hb_name_o), regex('gram', ignore_case = TRUE)) ~ 'NHS Grampian',
                                  str_detect(!!sym(hb_name_o), regex('glas|gg', ignore_case = TRUE)) ~ 'NHS Greater Glasgow and Clyde',
                                  str_detect(!!sym(hb_name_o), regex('high', ignore_case = TRUE)) ~ 'NHS Highland',
                                  str_detect(!!sym(hb_name_o), regex('lanar', ignore_case = TRUE)) ~ 'NHS Lanarkshire',
                                  str_detect(!!sym(hb_name_o), regex('loth', ignore_case = TRUE)) ~ 'NHS Lothian',
                                  str_detect(!!sym(hb_name_o), regex('ork', ignore_case = TRUE)) ~ 'NHS Orkney',
                                  str_detect(!!sym(hb_name_o), regex('shet|z', ignore_case = TRUE)) ~ 'NHS Shetland',
                                  str_detect(!!sym(hb_name_o), regex('tay', ignore_case = TRUE)) ~ 'NHS Tayside',
                                  str_detect(!!sym(hb_name_o), regex('west|wi', ignore_case = TRUE)) ~ 'NHS Western Isles',
                                  str_detect(!!sym(hb_name_o), regex('2', ignore_case = TRUE)) ~ 'NHS24',
                                  str_detect(!!sym(hb_name_o), regex('scot', ignore_case = TRUE)) ~ 'NHS Scotland',
                                  TRUE ~ !!sym(hb_name_o))) %>% 
    mutate(hb_correct=case_when(!(!!sym(hb_name_o) %in% c('NHS Ayrshire and Arran',
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
                                            'NHS24')) ~ FALSE,
                                TRUE ~ TRUE),
           .after=!!hb_name_o)
     

  #Prepare df with only correct names
  df_hb_correct <- df_hb_evaluated %>% 
    filter(hb_correct) %>% 
    select(-hb_correct)
  
  #Get wrong board names and postcodes
  df_wrong_hb_names <- df_hb_evaluated %>% 
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
    select(!!hb_name_o,!!dataset_type_o,!!submission_date_o,hb_correct) %>%
    group_by(!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
    mutate(!!total_rows_o := n()) %>%
    filter(!hb_correct) %>% 
    group_by(!!sym(dataset_type_o),!!sym(submission_date_o),!!sym(hb_name_o), hb_correct) %>% 
    mutate(unident_hb_variation=n()) %>% 
    group_by(!!sym(dataset_type_o),!!sym(submission_date_o),hb_correct) %>% 
    mutate(unident_hb_total_number = sum(unident_hb_variation),
           unident_hb_total_perc= round(unident_hb_total_number/!!sym(total_rows_o), 4)) %>% 
    distinct() %>% 
    inner_join(df_hb_evaluated %>% select(!!hb_name_o,!!postcode_o), by=hb_name_o) %>% 
    ungroup()

    
  
  #Get number of alternative non identifiable spellings
  weird_hb_names <- df_wrong_hb_names %>% 
    select(!!hb_name_o) %>% 
    distinct()
  
  if(nrow(df_wrong_hb_names)==0){
    message(paste('All health board names have been corrected.'))
  }else{
    df_wrong_hb_names_location=paste0('../../../output/removed/',
                                      'unidentifiable_HB_names_',
                                      as.character(today()),
                                      '.csv')
    
    write_csv(df_wrong_hb_names, df_wrong_hb_names_location)
    
  
    
    message(paste0(length(weird_hb_names), 
                  ' unidentifiable board names were found:\n',
                  paste(weird_hb_names, collapse=', ' ),
                  '\nA table with unidentifiable names, postcodes and stats was saved at\n ',
                  df_wrong_hb_names_location,'\n'))
  }
  
  
  return(df_hb_correct)
}


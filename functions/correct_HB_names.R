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
  
  df_hb_correct=df %>%
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
                                  TRUE ~ !!sym(hb_name_o)
                                  ) 
           )
  
  df_wrong_hb_names=df_hb_correct %>% 
    filter(!(!!sym(hb_name_o) %in% c('NHS Ayrshire and Arran',
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
                              'NHS24')))

  if(nrow(df_wrong_hb_names)==0){
    message(paste('All health board names have been corrected.'))
  }else{
    df_wrong_hb_names_location=paste0('../../../output/removed/',
                                      'remove_unusable_records_',
                                      as.character(now()))
    
    save_as_parquet(df_wrong_hb_names, df_wrong_hb_names_location)
    df_wrong_hb_names_length=nrow(df_wrong_hb_names)
    
    message(paste0(df_wrong_hb_names_length, 
                  ' records have unindentifiable health board names.\n',
                  'They are saved in ',df_wrong_hb_names_location, ".parquet"))
  }
  
  
  return(df_hb_correct)
}


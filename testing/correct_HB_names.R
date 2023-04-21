##########################.
###   correct_HB_names ###
##########################.

#Changes misspelled/weird heath board names to its correct name
source('scripts/setup/new_column_names_swift.R')
library(dplyr)
library(stringr)

correct_HB_names <- function(df){
  
  df_HB_correct=df %>%
    mutate(!!hb_name_o := case_when(str_detect(!!sym(hb_name_o), regex('ayr', ignore_case = TRUE)) ~ 'NHS Ayrshire and Arran',
                                  str_detect(!!sym(hb_name_o), regex('bor', ignore_case = TRUE)) ~ 'NHS Borders',
                                  str_detect(!!sym(hb_name_o), regex('dumf', ignore_case = TRUE)) ~ 'NHS Dumfries and Galloway',
                                  str_detect(!!sym(hb_name_o), regex('fife', ignore_case = TRUE)) ~ 'NHS Fife',
                                  str_detect(!!sym(hb_name_o), regex('forth', ignore_case = TRUE)) ~ 'NHS Forth Valley',
                                  str_detect(!!sym(hb_name_o), regex('gram', ignore_case = TRUE)) ~ 'NHS Grampian',
                                  str_detect(!!sym(hb_name_o), regex('glas|ggc', ignore_case = TRUE)) ~ 'NHS Greater Glasgow and Clyde',
                                  str_detect(!!sym(hb_name_o), regex('high', ignore_case = TRUE)) ~ 'NHS Highland',
                                  str_detect(!!sym(hb_name_o), regex('lanar', ignore_case = TRUE)) ~ 'NHS Lanarkshire',
                                  str_detect(!!sym(hb_name_o), regex('loth', ignore_case = TRUE)) ~ 'NHS Lothian',
                                  str_detect(!!sym(hb_name_o), regex('ork', ignore_case = TRUE)) ~ 'NHS Orkney',
                                  str_detect(!!sym(hb_name_o), regex('shet', ignore_case = TRUE)) ~ 'NHS Shetland',
                                  str_detect(!!sym(hb_name_o), regex('tay', ignore_case = TRUE)) ~ 'NHS Tayside',
                                  str_detect(!!sym(hb_name_o), regex('west', ignore_case = TRUE)) ~ 'NHS Western Isles',
                                  str_detect(!!sym(hb_name_o), regex('2', ignore_case = TRUE)) ~ 'NHS24',
                                  str_detect(!!sym(hb_name_o), regex('scot', ignore_case = TRUE)) ~ 'NHS Scotland',
                                  TRUE ~ !!hb_name_o
                                  ) 
           )

  
  return(df_noNull)
}


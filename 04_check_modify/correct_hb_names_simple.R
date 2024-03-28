correct_hb_names_simple <- function(df){
  
  #Change misspelled health board names to correct names
  df_hb_evaluated=df %>%
    mutate(!!hb_name_o := case_when(str_detect(!!sym(hb_name_o), regex('ayr|aa|a&a|ayshire', ignore_case = TRUE)) ~ 'NHS Ayrshire and Arran',
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
           .after=!!hb_name_o) |> 
    select(-hb_correct)

  return(df_hb_evaluated)
}

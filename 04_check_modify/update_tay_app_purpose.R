###########################################
## NHS Tayside Treat Start Date Function ##
###########################################

#The purpose of this function is to try and optimise the data ready for RTT in Tayside.
#Currently (23/04/2026), NHS Tayside are unable to provide app_purpose in thier CAPTND return.
#They do however provide treat_start_date for some pathways.
#The idea behind this function was to update the app_purpose of an appointment record from 99 to 02 
#where a treat_start_date had been provided that matched an app_date in that pathway.

#Author: Luke Taylor
#Date: 23/04/2026

update_tayside_app_purpose <- function(df){

identify_treat_start_tay <- df |>
  group_by(!!!syms(data_keys)) |>
  mutate(treat_start_flag_tay = !!sym(hb_name_o) == "NHS Tayside" & any(!is.na(treat_start_date))) |>
  mutate(app_purpose = if_else(treat_start_flag_tay &
                                 !is.na(app_date) & 
                                 app_date %in% treat_start_date, 2L, app_purpose)) |>
  ungroup()

return(identify_treat_start_tay)

}
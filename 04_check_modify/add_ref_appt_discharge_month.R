#############################################.
### Add ref appt and discharge month cols ###
#############################################.

#author: JBS
#date: 01/12/23


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------

add_ref_appt_discharge_month <- function(df) {
  
  df_extra_cols <- df %>%
    mutate(
      !!header_month_o := floor_date(!!sym(header_date_o), 'month'),
      !!referral_month_o := floor_date(!!sym(ref_rec_date_opti_o), 'month'),
      !!app_month_o := floor_date(!!sym(app_date_o), 'month'),
      !!case_closed_month_o := floor_date(!!sym(case_closed_date_o), 'month')
    )
    
  return(df_extra_cols)

}
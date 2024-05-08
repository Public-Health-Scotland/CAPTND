
#############################.
### Add proportion groups ###
#############################.

# Author: Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2024-05-08

add_proportion_groups <- function(df){
  
  vec_prop_groupings <- c("0%", ">0-33%", ">33-66%", ">66-99%", "100%", 
                          "Not submitted", "Supplementary info*", "Not applicable")
  
  df_grouped <- df |> 
    ungroup() |>  
    mutate(prop_group = case_when(
      variable %in% c("unav_date_start", "unav_date_end", "unav_reason", "unav_days_no") ~ "Supplementary info*",
      dataset_type == "PT" & variable == "act_code_sent_date" ~ "Supplementary info*",
      proportion == 0 & submission_status %in% c("submitted", "aggregate") ~ "0%",
      proportion > 0 & proportion <= 33.3 & submission_status %in% c("submitted", "aggregate")  ~ ">0-33%",
      proportion > 33.3 & proportion <= 66.6 & submission_status %in% c("submitted", "aggregate")  ~ ">33-66%",
      proportion > 66.6 & proportion < 100 & submission_status %in% c("submitted", "aggregate")  ~ ">66-99%",
      proportion == 100 & submission_status %in% c("submitted", "aggregate")  ~ "100%",
      is.na(proportion) & submission_status %in% c("submitted", "no submission possible", "aggregate") ~ "Not applicable",
      submission_status == "not submitted" ~ "Not submitted",
      TRUE ~ "check"),
      prop_group = factor(prop_group, levels = vec_prop_groupings))
  
  return(df_grouped)
   
}


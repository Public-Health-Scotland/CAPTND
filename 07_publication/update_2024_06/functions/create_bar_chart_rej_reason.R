
####################################################.
### Create bar chart to display rejection reason ###
####################################################.

# Author: Charlie Smith
# Date: 2024-07-12
ds = "CAMHS"
create_bar_chart_rejection_reason <- function(ds){

  df_reason <- read_parquet(paste0(non_acc_reason_dir, "non_acceptance_reason_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending, na.rm = TRUE) &
           hb_name == "NHS Scotland" &
             dataset_type == ds) |> 
    arrange(-prop)
  
  vec_reasons <- setdiff(unique(df_reason$ref_rej_reason_desc), "Other")
  
  chart <- df_reason |>
    mutate(ref_rej_reason_desc = factor(
      ref_rej_reason_desc,
      levels = c(vec_reasons, 'Other'))) |> 
    ggplot(aes(x = fct_rev(ref_rej_reason_desc), y = prop))+
    geom_bar(stat = "identity", width = 0.2)+
    coord_flip()
  
  return(chart)
  
}
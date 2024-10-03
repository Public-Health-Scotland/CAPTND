#########################################.
### Product 2 - reasons breakdown tbl ###
#########################################.

#author: Bex Madden
#date: 16/09/24


get_prod2_reasons_table <- function(latest_date){
  
# read in data relating to ability to calculate RTT FOR THE PAST MONTH ONLY
  
p2_data <- read_parquet(paste0(product2_dir, "/product2_data_monthly_", latest_date, ".parquet")) |>
  filter(rtt_general == 'not possible',
         sub_month == max(sub_month)) |> # latest month only MONTHLY data
  group_by(!!!syms(c(hb_name_o, dataset_type_o)), sub_month) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(percentage = round(n/total * 100, 1), #make percentage of total subs where rtt was 'not possible'
         !!sym(rtt_eval_o) := str_replace(!!sym(rtt_eval_o), ".*-", "")) 



p2_reasons <- p2_data |>
  group_by(!!sym(hb_name_o), !!sym(dataset_type_o)) |>
  arrange(-percentage, .by_group = TRUE) |>
  mutate(percentage = paste0(percentage, "%")) |>
  summarise(`reasons RTT not possible (% of affected records)` = paste0(!!sym(rtt_eval_o), " - ", percentage, collapse = "; ")) |> #makes descriptive list
  ungroup() |>
  arrange(!!hb_name_o, !!dataset_type_o)

return(p2_reasons)
}
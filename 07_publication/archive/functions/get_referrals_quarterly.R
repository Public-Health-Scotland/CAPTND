
##################################.
### Quarterly referrals by HB  ###
##################################.

# Author: Charlie Smith
# Date: 2023-11-17


get_referrals_quarterly <- function(){
  
  ref_quarter_ending_o <- "ref_quarter_ending"
  
  df_hb <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
      #filter(!is.na(!!sym(ref_acc_o))) %>% 
      select(all_of(data_keys),!!ref_acc_o, ref_quarter_ending_o) %>% 
      distinct() |>  
      group_by(!!!syms(c(dataset_type_o, hb_name_o, ref_quarter_ending_o))) |>  
      summarise(referrals = n(), .groups = 'drop')
  
  df_sco <- df_hb |> 
    group_by(!!!syms(c(dataset_type_o, ref_quarter_ending_o))) |> 
    summarise(referrals = sum(referrals, na.rm = TRUE)) |> 
    mutate(hb_name = "NHS Scotland")
    
  df_all <- bind_rows(df_hb, df_sco) |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_quarterly_hb"))  
  
  # narrative code
  table_hb_q_diff <- df_all |> 
    group_by(dataset_type, hb_name) |> 
    mutate(max_q = max(ref_quarter_ending),
           min_q = min(ref_quarter_ending)) %>% 
    filter(ref_quarter_ending == max_q |
             ref_quarter_ending == min_q) %>% 
    mutate(n_row = row_number(),
           order = case_when(n_row == 1 ~ "first", 
                             TRUE ~ "last")) %>% 
    select(-c(max_q, min_q, n_row)) 
  
  df1 <- table_hb_q_diff %>% 
    filter(order == "first") %>% 
    rename(first_refs = referrals) %>% 
    select(-c(order, ref_quarter_ending))
  
  df2 <- table_hb_q_diff %>% 
    filter(order == "last")%>% 
    rename(last_refs = referrals) %>% 
    select(-c(order, ref_quarter_ending))
  
  annual_change_desc <- left_join(df1, df2, by = c("dataset_type","hb_name")) %>% 
    mutate(year_diff = last_refs - first_refs,
           year_diff2 = ifelse(year_diff <0, year_diff * -1, year_diff)) %>% 
    arrange(-year_diff) %>% 
    na.omit() %>% 
    mutate(diff_perc = round((last_refs - first_refs) / first_refs *100, 1),
           dir = case_when(
             year_diff > 0 ~ "increased by ",
             year_diff < 0 ~ "decreased by ",
             year_diff == 0 ~ "remained the same",
             TRUE ~ NA_character_),
           dir2 = case_when(
             year_diff > 0 ~ "an increase of ",
             year_diff < 0 ~ "a decrease of ",
             year_diff == 0 ~ "remained the same",
             TRUE ~ NA_character_),
           year_diff2 = format(year_diff2, big.mark = ","),
           label = paste0(hb_name, " ", dir, year_diff2, " (", diff_perc, "%)"),
           label2 = paste0(dir2, year_diff2, " (", diff_perc, "%)")) %>% 
    select(-c(dir, dir2)) |> 
    arrange(dataset_type, hb_name)
  
  df_pos <- annual_change_desc |> 
    group_by(dataset_type) |> 
    filter(diff_perc > 0) |> 
    arrange(dataset_type, -diff_perc)
  
  df_neg <- annual_change_desc |> 
    group_by(dataset_type) |> 
    filter(diff_perc < 0) |> 
    arrange(dataset_type, -diff_perc)
  
}







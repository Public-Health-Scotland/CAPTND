##################################.
### Product 2 - data wrangling ###
##################################.

#author: Charlie Smith & JBS
#date: 23/11/23

#modified to add rrt eval for the last 5 quarters and Scotland totals
#20/02/24
#JBS

# 1 Load libraries --------------------------------------------------------

source('05_data_quality/product2_plot_details.R')
source('05_data_quality/product2_plot_general.R')
source('05_data_quality/product2_plot_heatmap.R')
source('05_data_quality/product2_plot_heatmap_quarterly.R')
source('05_data_quality/product2_plot_heatmap_monthly.R')
source('05_data_quality/product2_plot_issues.R')
source('04_check_modify/add_rtt_eval.R')

# 2 Make product 2 --------------------------------------------------------

make_product_2 <- function(df_rtt, most_recent_month_in_data) {
  
  make_df_prep_plot <- function(df_rtt, max_date) {
    df_rtt_plot_prep <- df_rtt %>%
      filter(!!sym(ref_rec_date_opti_o) >= ymd(210801)) %>%
      select(all_of(data_keys), !!rtt_eval_o) %>%
      distinct() %>%
      group_by(!!!syms(c(hb_name_o, dataset_type_o, rtt_eval_o))) %>%
      summarise(n = n(),
                .groups = 'drop') %>%
      group_by(!!!syms(c(dataset_type_o, rtt_eval_o))) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(!!hb_name_o, ~"NHS Scotland"),
                          .groups = "drop")) %>%
      ungroup() %>% 
      group_by(!!!syms(c(hb_name_o, dataset_type_o))) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      mutate(
        perc = round(n / total * 100, 1),
        !!hb_name_o := factor(!!sym(hb_name_o), level = level_order),
        !!rtt_eval_o := factor(!!sym(rtt_eval_o),
          level = c(
            'seen - active',
            #1
            'seen - closed',
            #2
            'seen - online - active',
            #3
            'seen - online - closed',
            #
            'waiting - after assessment',
            'waiting - not attended',
            'waiting - no app',
            'referral pending',
            'referral not accepted',
            'case closed after assessment',
            'case closed with no app',
            'case closed due to non attendance',
            'case closed - referral pending',
            'rtt not possible - app with no referral acc',
            'rtt not possible - patient had appt and ref is pending',
            'rtt not possible - app date but no attendance status',
            'rtt not possible - attended app but no purpose',
            'rtt not possible - unknown'
          )
        )
      ) %>%
      mutate(
        rtt_general = case_when(
          str_detect(!!sym(rtt_eval_o), 'seen.*') ~ 'seen',
          str_detect(!!sym(rtt_eval_o), '.*waiting.*') ~ 'waiting',
          !!sym(rtt_eval_o) %in% c('referral pending', 'referral not accepted') ~ 'referral not accepted',
          str_detect(!!sym(rtt_eval_o), 'closed') ~ 'closed before seen',
          str_detect(!!sym(rtt_eval_o), 'not possible') ~ 'rtt not possible'
        )
      ) %>%
      mutate(rtt_general = factor(
        rtt_general,
        level = c(
          'seen',
          #1
          'waiting',
          'referral not accepted',
          'closed before seen',
          'rtt not possible'
        )
      ))
    
    save_as_parquet(df = df_rtt_plot_prep, path = paste0(product2_dir, "/product2_data"))
    
    return(df_rtt_plot_prep)
  }
  
  
  date_2 = most_recent_month_in_data %m-% months(3)
  date_3 = most_recent_month_in_data %m-% months(6)
  date_4 = most_recent_month_in_data %m-% months(9)
  date_5 = most_recent_month_in_data %m-% years(1)
  date_6 = most_recent_month_in_data %m-% years(2)
  
  plot_all <- function(df_rtt, date_max, date_min) {  #added min date to give 12 month frame
    
    df_rtt_again <- df_rtt %>% 
      filter(!!sym(header_date_o) <= date_max,
             !!sym(header_date_o) > date_min) %>%
      add_rtt_eval(., evalAllData = TRUE) # is this needed? rtt eval already in main dataset, why calc again
    
    df_rtt_plot_prep <- make_df_prep_plot(df_rtt_again, date_max)
    
    product2_plot_general(df_rtt_plot_prep, date_max) # uses df plot prep from above - so is all time
    product2_plot_details(df_rtt_plot_prep, date_max) # uses df plot prep from above - so is all time
    product2_plot_issues(df_rtt_plot_prep, date_max) # uses df plot prep from above - so is all time
    product2_plot_heatmap(df_rtt_again, date_max) #this will be showing for the past year based on the date filters for df_rtt_again
    product2_plot_heatmap_quarterly(df_rtt_again, date_max) # ditto, past year split into quarters, need months
    product2_plot_heatmap_monthly(df_rtt_again, date_max) # ditto, past year split into quarters, need months
    
  }

  
  plot_all(df_rtt, most_recent_month_in_data, date_5)
  # plot_all(df_rtt, date_2)
  # plot_all(df_rtt, date_3)
  # plot_all(df_rtt, date_4)
  plot_all(df_rtt, date_5, date_6)
  
  
  
  message(paste0("All plots are in ",product2_dir))


}
  



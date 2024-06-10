
##################################################.
### Basic CAPTND vs. Shorewise referral counts ###
##################################################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-02-27



# 0 - Load functions ------------------------------------------------------

source("./07_publication/investigations/fix_quarter_ending.R")
source('02_setup/save_df_as_parquet.R')

library(scales)


# 1 - Load data -----------------------------------------------------------

# 1.1 - Basic CAPTND ------------------------------------------------------

location_basic <- "../../../../R script/CAPTND Reporting/Output/Agg_Report/Trial/report_info/"

basic_camhs_quart_hb <- read_parquet(file = paste0(location_basic, "CAMHS_refs_quart_hb.parquet")) |> 
  mutate(dataset_type = "CAMHS", .before = everything())

# basic_camhs_age_sex <- read_parquet(file = paste0(location_basic, "CAMHS_refs_age_sex.parquet")) |>  mutate(dataset_type = "CAMHS", .before = everything())
# basic_camhs_simd <- read_parquet(file = paste0(location_basic, "CAMHS_refs_simd.parquet")) |>  mutate(dataset_type = "CAMHS", .before = everything())

basic_pt_quart_hb <- read_parquet(file = paste0(location_basic, "PT_refs_quart_hb.parquet")) |>  
  mutate(dataset_type = "PT", .before = everything())

# basic_pt_age_sex <- read_parquet(file = paste0(location_basic, "PT_refs_age_sex.parquet")) |>  mutate(dataset_type = "PT", .before = everything())
# basic_pt_simd <- read_parquet(file = paste0(location_basic, "PT_refs_simd.parquet")) |>  mutate(dataset_type = "PT", .before = everything())


basic_quart_hb <- rbind(basic_camhs_quart_hb, basic_pt_quart_hb) |> 
  fix_quarter_ending() 

# basic_age_sex <- rbind(basic_camhs_age_sex, basic_pt_age_sex)
# basic_simd <- rbind(basic_camhs_simd, basic_pt_simd)

rm(basic_camhs_quart_hb,
   basic_pt_quart_hb)
# basic_camhs_monthly_sco,
# basic_camhs_age_sex,
# basic_camhs_simd,
# basic_pt_monthly_sco,
# basic_pt_age_sex,
# basic_pt_simd)


# 1.2 - Shorewise ---------------------------------------------------------

location_shorewise <- "../../../output/publication/data/2024-03-01_release/"

shorewise_quart_hb <- read_parquet(file = paste0(location_shorewise, "refs_quarterly_hb.parquet")) |> 
  mutate(referrals = as.numeric(referrals))

# shorewise_monthly_sco <- read_parquet(file = paste0(location_shorewise, "refs_monthly_sco.parquet"))
# shorewise_age_sex <- read_parquet(file = paste0(location_shorewise, "refs_sex_age.parquet"))
# shorewise_simd <- read_parquet(file = paste0(location_shorewise, "refs_simd.parquet"))




# 2 - Join referral dfs for comparisons -----------------------------------

comp_quart_refs_hb <- full_join(basic_quart_hb, shorewise_quart_hb, 
                                by = c("dataset_type" , "hb_name", "ref_quarter_ending"),
                                suffix = c("_basic", "_shore")) |> 
  
  # add change measures
  mutate(difference = referrals_shore - referrals_basic, 
         #ratio = round( referrals_shore / referrals_basic, 2), 
         perc_change = round((referrals_shore - referrals_basic) / referrals_basic * 100, 1))
         
         




# 3 - Present in tables ---------------------------------------------

table_quart_refs_hb_diff <- comp_quart_refs_hb |> 
  select(1:3, 6) |> 
  pivot_wider(names_from = ref_quarter_ending, 
              values_from = difference)

table_quart_refs_hb_perc_diff <- comp_quart_refs_hb |> 
  select(1:3, 8) |> 
  pivot_wider(names_from = ref_quarter_ending, 
              values_from = perc_change)


# 4 - Visualise -----------------------------------------------------------

# factorise and relevel HB names and factorise quarter end date

comp_quart_refs_hb <- comp_quart_refs_hb |>  
  mutate(hb_name = factor(hb_name), 
         hb_name = fct_relevel(hb_name, c('NHS Scotland',
                                          'NHS Ayrshire and Arran',
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
                                          'NHS 24')), 
         ref_quarter_ending = factor(ref_quarter_ending)) |> 
  arrange(hb_name) 

# create heatmap

comp_heatmap <- comp_quart_refs_hb |> 
  ggplot(aes(y = fct_rev(hb_name), x = ref_quarter_ending, fill = perc_change)) + 
  geom_tile(color = "black",
            lwd = 0.2,
            linetype = 1)+ 
  geom_text(aes(label = perc_change), size = 3)+
  scale_fill_gradient2(low = "#B3D7F2", mid = "white", high = "#D26146", 
                       na.value = "grey90", midpoint = 0)+
  labs(x = "Referral Quarter Ending", y = "Health Board")+
  scale_x_discrete(guide = guide_axis(angle = 45), 
                   labels = c("Dec '22", "Mar '23", "Jun '23", "Sep '23", "Dec '23"))+
  theme_minimal()+
  facet_wrap(~ dataset_type)

comp_heatmap



# 5 - Substitution plot --------------------------------------------------

# Plot referrals with shorewise data, 
# but for boards with under threshold for percent change substitute basic data into plot instead

# set threshold

threshold_val <- 10

# make dummy column with T/F if row meets threshold ###REMOVE MULTIPLE MUTATE CALLS ####

substitute_refs <- comp_quart_refs_hb |>
  mutate(data_used = case_when(perc_change <= -threshold_val ~ "Basic",
                               perc_change >= -threshold_val ~ "Optimised",
                               is.na(referrals_shore) & is.na(referrals_basic) ~ "Not available", 
                               is.na(referrals_shore) & !is.na(referrals_basic) ~ "Basic")) |>
  mutate(data_used = factor(data_used)) |> 
  mutate(data_used = fct_relevel(data_used, c("Optimised", "Basic", "Not available"))) |> 
  mutate(referrals_subs = ifelse(data_used == "Optimised", referrals_shore, referrals_basic))


# use heatmap plot to create gridded table


subs_plot <- substitute_refs |> 
  ggplot(aes(y = fct_rev(hb_name), x = ref_quarter_ending, fill = data_used)) + 
  geom_tile(color = "black",
            lwd = 0.2,
            linetype = 1)+ 
  geom_text(aes(label = scales::comma(referrals_subs)), size = 3)+
  scale_fill_manual(values=c("#C1DD93", "#DB806A", "grey90"))+
  labs(title = "CAPTND: Referrals by Health Board", 
       caption = paste0("\n This heatmap uses optimised CAPTND data, substituting with basic data \n for boards where less than ", threshold_val, "% of referral data survived optimisation"),
       x = "\n Referral Quarter Ending", y = "Health Board", fill = " CAPTND \n data source")+
  scale_x_discrete(guide = guide_axis(angle = 45), 
                   labels = c("Dec '22", "Mar '23", "Jun '23", "Sep '23", "Dec '23"))+
  theme_minimal()+
  facet_wrap(~ dataset_type)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.caption = element_text(hjust = 0, size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.key.size = unit(5, 'mm')
  )

subs_plot


# save out

ggsave("basic_shorewise_subs.png", plot = subs_plot,
       path = referrals_dir,
       width = 30, height = 20, units = "cm")



# 6 - Make Report ---------------------------------------------------------

# WIP needs refining


# Load workbook template
wb <- loadWorkbook(paste0("../../../output/product_pack_working/template_basic_v_opti.xlsx"))

# Write the data to the sheets
print(comp_heatmap)
insertPlot(wb, "Basic vs. Optimised heatmap",  
            startRow = 10, startCol = 2, width = 20, height = 15, units = "cm")

writeData(wb, sheet = "Change tables", x = table_quart_refs_hb_diff,
          startRow = 10, startCol = 2)
writeData(wb, sheet = "Change tables", x = table_quart_refs_hb_perc_diff,
          startRow = 10, startCol = 11)

print(subs_plot)
insertPlot(wb, "Substitution plot",  
            startRow = 10, startCol = 2, width = 22, height = 17.5, units = "cm")


# Export the file
saveWorkbook(wb, paste0(external_reports_dir, "/basic_vs_opti_comparison.xlsx"), overwrite = TRUE)




# 7. Monthly referrals for Scotland - line chart ------------------------------


# 7.1 Load in monthly referrals data --------------------------------------

#for basic

basic_camhs_monthly_sco <- read_parquet(file = paste0(location_basic, "CAMHS_refs_monthly_sco.parquet")) |>  
  mutate(dataset_type = "CAMHS", .before = everything())

basic_pt_monthly_sco <- read_parquet(file = paste0(location_basic, "PT_refs_monthly_sco.parquet")) |>  
  mutate(dataset_type = "PT", .before = everything())

basic_monthly_sco <- rbind(basic_camhs_monthly_sco, basic_pt_monthly_sco) |>
  rename(referral_month = rec_month, referrals = n_refs) |>
  select(!label)

rm(basic_camhs_monthly_sco,
   basic_pt_monthly_sco)

#for shorewise

shorewise_monthly_sco <- read_parquet(file = paste0(location_shorewise, "refs_monthly_sco.parquet"))


# 7.2 Bind dfs and add change measures ------------------------------------

comp_month_refs_sco <- full_join(basic_monthly_sco, shorewise_monthly_sco, 
                                 by = c("dataset_type" , "referral_month"),
                                 suffix = c("_basic", "_shore")) |> 
  
  # add change measures
  mutate(difference = referrals_shore - referrals_basic, 
         ratio = round( referrals_shore / referrals_basic, 2), 
         perc_change = round((referrals_shore - referrals_basic) / referrals_basic * 100, 1))


# Present as change table

table_month_refs_sco_diff <- comp_month_refs_sco |> 
  select(1:3, 6) |> 
  pivot_wider(names_from = referral_month, 
              values_from = difference)

table_month_refs_sco_perc_diff <- comp_month_refs_sco |> 
  select(1:3, 8) |> 
  pivot_wider(names_from = referral_month, 
              values_from = perc_change)




# 7.3 - Visualise -----------------------------------------------------------

# factorise and relevel

comp_month_refs_sco <- comp_month_refs_sco |>  
  mutate(referral_month = as.Date(referral_month, format = "%Y-%m-%d"),
         dataset_type = factor(dataset_type))


# Substitute basic data if threshold or retention not met

# set threshold

threshold_val <- 15

# make dummy column with T/F if row meets threshold ###REMOVE MULTIPLE MUTATE CALLS ####

substitute_month_refs <- comp_month_refs_sco |>
  mutate(data_used = case_when(perc_change <= -threshold_val ~ "Basic",
                               perc_change >= -threshold_val ~ "Optimised", 
                               # is.na(referrals_shore) & is.na(referrals_basic) ~ "Not available", 
                               is.na(referrals_shore) & !is.na(referrals_basic) ~ "Basic")) |>
  mutate(data_used = factor(data_used)) |> 
  mutate(data_used = fct_relevel(data_used, c("Optimised", "Basic"))) |> #, "Not available"
  mutate(referrals_subs = ifelse(data_used == "Optimised", referrals_shore, referrals_basic))


# separate PT and CAMHS plots

CAMHS_substitute_month_refs <- filter(substitute_month_refs, dataset_type == "CAMHS")
PT_substitute_month_refs <- filter(substitute_month_refs, dataset_type == "PT")

# Create Scotland referrals chart for CAMHS

CAMHS_ref_sco <- CAMHS_substitute_month_refs %>% 
  ggplot(aes(x = referral_month, y = referrals_subs, color = data_used, group = 1))+
  geom_line(linewidth = 1.5)+
  geom_point(size = 2)+
  scale_color_manual(values=c("#17375E", "#DB806A"))+
  xlab("")+
  ylab("Referrals")+
  labs(title = paste0("CAPTND: Monthly CAMHS Referrals ", ", NHS Scotland"), #add date
       caption = paste0("Source: CAPTND, ", 
                        "\n This plot uses optimised CAPTND data, substituting with basic data \n for instances where less than ", threshold_val, "% of referral data survived optimisation"), #add date
       color = " CAPTND data source")+
  scale_x_date(date_breaks = "1 month", minor_breaks = "1 month", date_labels = "%b-%y",
               expand = c(0.02, 0.02))+
  scale_y_continuous(breaks =
                       seq(0, plyr::round_any(max(CAMHS_substitute_month_refs$referrals_subs, na.rm = T), 500, f = ceiling), by = 500),
                     minor_breaks = NULL,
                     labels = comma,
                     limits = c(0, plyr::round_any(max(CAMHS_substitute_month_refs$referrals_subs, na.rm = T), 500, f = ceiling)))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom", 
        plot.caption = element_text(hjust = 1))

CAMHS_ref_sco


# for PT
PT_ref_sco <- PT_substitute_month_refs %>% 
  ggplot(aes(x = referral_month, y = referrals_subs, color = data_used, group = 1))+
  geom_line(linewidth = 1.5)+
  geom_point(size = 2)+
  scale_color_manual(values=c("#17375E", "#DB806A"))+
  xlab("")+
  ylab("Referrals")+
  labs(title = paste0("CAPTND: Monthly Psychological Therapies Referrals ", ", NHS Scotland"), #add date
       caption = paste0("Source: CAPTND, ", 
                        "\n This plot uses optimised CAPTND data, substituting with basic data \n for instances where less than ", threshold_val, "% of referral data survived optimisation"), #add date
       color = " CAPTND data source")+
  scale_x_date(date_breaks = "1 month", minor_breaks = "1 month", date_labels = "%b-%y",
               expand = c(0.02, 0.02))+
  scale_y_continuous(breaks =
                       seq(0, plyr::round_any(max(PT_substitute_month_refs$referrals_subs, na.rm = T), 1000, f = ceiling), by = 1000),
                     minor_breaks = NULL,
                     labels = comma,
                     limits = c(0, plyr::round_any(max(PT_substitute_month_refs$referrals_subs, na.rm = T), 1000, f = ceiling)))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom", 
        plot.caption = element_text(hjust = 1))

PT_ref_sco




# 7.4 Comparison plots for basic and optimised ----------------------------

#for camhs
# reshape data
CAMHS_comp <- CAMHS_substitute_month_refs |>
  select(referral_month, referrals_basic, referrals_shore) |>
  gather(key = "data_source", value = "referrals", -referral_month) 


#plot 

CAMHS_comp_ref_sco <- CAMHS_comp %>% 
  ggplot()+
  geom_line(aes(x = referral_month, y = referrals, color = data_source), linewidth = 1.5)+
  geom_point(aes(x = referral_month, y = referrals, color = data_source), size = 2)+
  scale_color_manual(labels = c("Basic", "Optimised"), values = c("#DB806A", "#17375E"))+
  xlab("")+
  ylab("Referrals")+
  labs(title = paste0("CAPTND: Monthly CAMHS Referrals ", ", NHS Scotland"), #add date
       caption = paste0("Source: CAPTND, ", 
                        "\n This plot shows optimised CAPTND data alongside basic CAPTND data for comparison."),  #add date
       color = " CAPTND data source")+
  scale_x_date(date_breaks = "1 month", minor_breaks = "1 month", date_labels = "%b-%y",
               expand = c(0.02, 0.02))+
  scale_y_continuous(breaks =
                       seq(0, plyr::round_any(max(CAMHS_comp$referrals, na.rm = T), 500, f = ceiling), by = 500),
                     minor_breaks = NULL,
                     labels = comma,
                     limits = c(0, plyr::round_any(max(CAMHS_comp$referrals, na.rm = T), 500, f = ceiling)))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom", 
        plot.caption = element_text(hjust = 1))

CAMHS_comp_ref_sco

# for PT
# reshape data
PT_comp <- PT_substitute_month_refs |>
  select(referral_month, referrals_basic, referrals_shore) |>
  gather(key = "data_source", value = "referrals", -referral_month) 


#plot 

PT_comp_ref_sco <- PT_comp %>% 
  ggplot()+
  geom_line(aes(x = referral_month, y = referrals, color = data_source), linewidth = 1.5)+
  geom_point(aes(x = referral_month, y = referrals, color = data_source), size = 2)+
  scale_color_manual(labels = c("Basic", "Optimised"), values = c("#DB806A", "#17375E"))+
  xlab("")+
  ylab("Referrals")+
  labs(title = paste0("CAPTND: Monthly Psychological Therapies Referrals ", ", NHS Scotland"), #add date
       caption = paste0("Source: CAPTND, ", 
                        "\n This plot shows optimised CAPTND data alongside basic CAPTND data for comparison."), #add date
       color = " CAPTND data source")+
  scale_x_date(date_breaks = "1 month", minor_breaks = "1 month", date_labels = "%b-%y",
               expand = c(0.02, 0.02))+
  scale_y_continuous(breaks =
                       seq(0, plyr::round_any(max(PT_comp$referrals, na.rm = T), 1000, f = ceiling), by = 1000),
                     minor_breaks = NULL,
                     labels = comma,
                     limits = c(0, plyr::round_any(max(PT_comp$referrals, na.rm = T), 1000, f = ceiling)))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom", 
        plot.caption = element_text(hjust = 1))

PT_comp_ref_sco

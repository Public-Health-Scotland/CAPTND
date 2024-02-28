
##################################################.
### Basic CAPTND vs. Shorewise referral counts ###
##################################################.

# Author: Charlie Smith
# Date: 2024-02-27



# 0 - Load functions ------------------------------------------------------

source("./07_publication/investigations/fix_quarter_ending.R")


# 1 - Load data -----------------------------------------------------------

# 1.1 - Basic CAPTND ------------------------------------------------------

location_basic <- "../../../../R script/CAPTND Reporting/Output/Agg_Report/Trial/report_info/"

basic_camhs_quart_hb <- read_parquet(file = paste0(location_basic, "CAMHS_refs_quart_hb.parquet")) |>  mutate(dataset_type = "CAMHS", .before = everything())
basic_camhs_monthly_sco <- read_parquet(file = paste0(location_basic, "CAMHS_refs_monthly_sco.parquet")) |>  mutate(dataset_type = "CAMHS", .before = everything())
basic_camhs_age_sex <- read_parquet(file = paste0(location_basic, "CAMHS_refs_age_sex.parquet")) |>  mutate(dataset_type = "CAMHS", .before = everything())
basic_camhs_simd <- read_parquet(file = paste0(location_basic, "CAMHS_refs_simd.parquet")) |>  mutate(dataset_type = "CAMHS", .before = everything())

basic_pt_quart_hb <- read_parquet(file = paste0(location_basic, "PT_refs_quart_hb.parquet")) |>  mutate(dataset_type = "PT", .before = everything())
basic_pt_monthly_sco <- read_parquet(file = paste0(location_basic, "PT_refs_monthly_sco.parquet")) |>  mutate(dataset_type = "PT", .before = everything())
basic_pt_age_sex <- read_parquet(file = paste0(location_basic, "PT_refs_age_sex.parquet")) |>  mutate(dataset_type = "PT", .before = everything())
basic_pt_simd <- read_parquet(file = paste0(location_basic, "PT_refs_simd.parquet")) |>  mutate(dataset_type = "PT", .before = everything())


basic_quart_hb <- rbind(basic_camhs_quart_hb, basic_pt_quart_hb) |> fix_quarter_ending() 

basic_monthly_sco <- rbind(basic_camhs_monthly_sco, basic_pt_monthly_sco)
basic_age_sex <- rbind(basic_camhs_age_sex, basic_pt_age_sex)
basic_simd <- rbind(basic_camhs_simd, basic_pt_simd)

rm(basic_camhs_quart_hb,
   basic_camhs_monthly_sco,
   basic_camhs_age_sex,
   basic_camhs_simd,
   basic_pt_quart_hb,
   basic_pt_monthly_sco,
   basic_pt_age_sex,
   basic_pt_simd)

# 1.2 - Shorewise ---------------------------------------------------------

location_shorewise <- "../../../output/publication/data/2024-03-01_release/"

shorewise_quart_hb <- read_parquet(file = paste0(location_shorewise, "refs_quarterly_hb.parquet")) |> 
  mutate(referrals = as.numeric(referrals))

shorewise_monthly_sco <- read_parquet(file = paste0(location_shorewise, "refs_monthly_sco.parquet"))
shorewise_age_sex <- read_parquet(file = paste0(location_shorewise, "refs_sex_age.parquet"))
shorewise_simd <- read_parquet(file = paste0(location_shorewise, "refs_simd.parquet"))





# 2 - Join referral dfs for comparisons -----------------------------------

comp_quart_refs_hb <- full_join(basic_quart_hb, shorewise_quart_hb, 
                                by = c("dataset_type" , "hb_name", "ref_quarter_ending"),
                                suffix = c("_basic", "_shore")) |> 
  
  # add change measures
  mutate(difference = referrals_shore - referrals_basic, 
         ratio = round( referrals_shore / referrals_basic, 2), 
         perc_change = round((referrals_shore - referrals_basic) / referrals_basic * 100, 1))




# 3 - Present as change table ---------------------------------------------

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
  mutate(hb_name = factor(hb_name)) |> 
  mutate(hb_name = fct_relevel(hb_name, c('NHS Scotland',
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
                                       'NHS 24'))) |> 
  arrange(hb_name) |> 
  mutate(ref_quarter_ending = factor(ref_quarter_ending))


# create heatmap

comp_heatmap <- comp_quart_refs_hb |> 
  ggplot(aes(y = fct_rev(hb_name), x = ref_quarter_ending, fill = perc_change)) + 
  geom_tile(color = "black",
            lwd = 0.2,
            linetype = 1)+ 
 #coord_fixed()+ #makes cells square
  geom_text(aes(label = perc_change), size = 3)+
  scale_fill_gradient2(low = "#B3D7F2", mid = "white", high = "#D26146", 
                      na.value = "grey90", midpoint = 0)+
  labs(x = "Referral Quarter Ending", y = "Health Board")+
  scale_x_discrete(guide = guide_axis(angle = 45), 
                   labels = c("Dec '22", "Mar '23", "Jun '23", "Sep '23", "Dec '23"))+
  theme_minimal()+
  facet_wrap(~ dataset_type)

comp_heatmap


# 5 - Make Report ---------------------------------------------------------

# WIP needs refining


# Create a blank workbook
OUT <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(OUT, "Basic vs. Shorewise heatmap")
addWorksheet(OUT, "Difference table")
addWorksheet(OUT, "Percentage change table")

# Write the data to the sheets
insertPlot(OUT, 1, width = 30, height = 20, fileType = "png", units = "cm")
writeData(OUT, sheet = "Difference table", x = table_quart_refs_hb_diff)
writeData(OUT, sheet = "Percentage change table", x = table_quart_refs_hb_perc_diff)


# Export the file
saveWorkbook(OUT, "Basic vs. Shorewise comparison.xlsx")




# To do:
# - fix order of HBs (create reference vector) -- done
# - axis labels -- done
# - show all 5 quarters on y-axis -- done
# - align quarter axis ticks and numbers -- done
# - black tile outlines -- done
# - set colour for NAs -- charlie did
# - save tables from section 3 
# - save image






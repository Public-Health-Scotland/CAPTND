
###############################################.
### Generate figures for CAPTND publication ###
###############################################.

# Author: Charlie Smith
# Date: 2023-11-14

# Note: recreate publication figures and charts using 'new' CAPTND (shorewise) data


# Step 1: Enter last month of data to include in publication --------------

month_end <- "2024-03-01"


# Step 2 - Run these scripts in sequence ----------------------------------

source("./07_publication/update_2024_06/chapters/1_load_packages.R")
source("./07_publication/update_2024_06/chapters/2_load_functions.R")
source("./07_publication/update_2024_06/chapters/3_set_constants.R")


# 3 - Analyse Data --------------------------------------------------------

summarise_referrals()
summarise_referrals_by_ref_source()

summarise_non_acceptance()
summarise_non_acceptance_reason()
summarise_non_acceptance_action()

summarise_referrals_basic_opti()
summarise_appointments_att()


# 4 - Compile excel workbooks ---------------------------------------------

# part of measure scoping
compile_referrals_summary()
compile_referrals_by_ref_source()

compile_non_acceptance_summary()
compile_non_acceptance_reason_summary()
compile_non_acceptance_action_summary()

compile_appointments_summary()
compile_basic_opti_summary()



# 5 - Create tables/charts for publication --------------------------------

create_table_referrals_quarterly()

create_table_acceptance_rate()
# create_table_acceptance_reason_action() # no longer required

create_bar_chart_non_acceptance_reason(ds = "CAMHS") 
create_bar_chart_non_acceptance_reason(ds = "PT") 

create_bar_chart_non_acceptance_action(ds = "CAMHS") 
create_bar_chart_non_acceptance_action(ds = "PT") 

create_bar_chart_dna_simd("CAMHS")
create_bar_chart_dna_simd("PT")

create_trend_plot_dna_rate("CAMHS")
create_trend_plot_dna_rate("PT")



# 6 - Create report

# set inputs

dataset_choice <- "PT" # "CAMHS"
dataset_label <- "Psychological Therapies" # "Child and Adolescent Mental Health Services"

month_label <- format(as.Date(month_end, "%Y-%m-%d"), "%B %Y")

# Render markdown document

test_render <- function(){
rmarkdown::render(
  "./07_publication/update_2024_06/markdown/CAPTND_shorewise_pub.Rmd",
  output_format = phstemplates::phs_report_docx(
    reference_docx = "phs-offdev-report.docx",
    cover_page = "phs-offdev-cover.docx",
    cover_title = "Child, Adolescent, and Psychological Therapies National Dataset (CAPTND):",
    cover_subtitle = paste0(dataset_label, " (", dataset_choice, ")"),
    cover_date = "03 09 2024",
    toc_depth = 3
  ),
  output_file = paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", 
                       data_analysis_latest_date, "/shorewise_publication/report/CAPTND_shorewise_pub_",
                       dataset_choice, "_", month_end, ".docx") # change this to change output file name
 )
}
test_render()

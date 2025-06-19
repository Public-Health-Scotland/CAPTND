###################################################.
### CREATE CAPTND PUBLICATION SUMMARY DOCUMENTS ###.
###################################################.

# Author: Luke Taylor
# Date: 12/06/2025

#Script to create publication summaries for both PT and CAMHS

dataset_choice <- "PT"
dataset_label <- "Psychological Therapies"

source("./07_publication/script/functions/source_data_for_markdown.R", local = knitr::knit_global())

rmarkdown::render("./07_publication/script/markdown/CAPTND_shorewise_pub_summary.Rmd",
                  output_file = paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_",
                                       data_analysis_latest_date, "/shorewise_publication/report/CAPTND_publication_summary_",
                                       dataset_choice, "_", month_end, ".docx"))


dataset_label <- "Child and Adolescent Mental Health Services"
dataset_choice <- 'CAMHS'
                  
source("./07_publication/script/functions/source_data_for_markdown.R", local = knitr::knit_global())

rmarkdown::render(
  "./07_publication/script/markdown/CAPTND_shorewise_pub_summary.Rmd",
  output_file = paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_",
                       data_analysis_latest_date, "/shorewise_publication/report/CAPTND_publication_summary_",
                       dataset_choice, "_", month_end, ".docx"))
  

rm(dataset_choice)
rm(dataset_label)

##########################################.
### CREATE CAPTND PUBLICATION DOCUMENT ###.
##########################################.

# Author: Luke Taylor
# Date: 26/06/2026

create_dna_pub_word_doc <- function(dataset_choice = c("CAMHS", "PT")){
  
  if(dataset_choice == "PT"){
    dataset_label <- "Psychological Therapies"
  } else {
    if(dataset_choice == "CAMHS"){
      dataset_label <- "Child and Adolescent Mental Health Services"
    }}
  
  
  # Render markdown document
  if(dataset_choice == "PT"){ 
    rmarkdown::render(
      "./07_publication/script/markdown/dna_pub/CAPTND_shorewise_dna_pub_PT.Rmd",
      output_format = phstemplates::phs_report_docx(
        reference_docx = "phs-offdev-report.docx",
        cover_page = "phs-offdev-cover.docx",
        cover_title = "Child, Adolescent, and Psychological Therapies National Dataset (CAPTND):",
        cover_subtitle = "Psychological Therapies (PT) Appointment 'Did Not Attend' (DNA) Report (April 2025 - June 2026)", 
        cover_date = "01 09 2026",
        toc_depth = 3
      ),
      output_file = paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", 
                           data_analysis_latest_date, "/shorewise_publication/report/CAPTND_dna_publication_",
                           dataset_choice, "_", publication_month, ".docx")) 
    
    
  } else {
    
    if(dataset_choice == "CAMHS"){
      rmarkdown::render(
        "./07_publication/script/markdown/dna_pub/CAPTND_shorewise_dna_pub_CAMHS.Rmd",
        output_format = phstemplates::phs_report_docx(
          reference_docx = "phs-offdev-report.docx",
          cover_page = "phs-offdev-cover.docx",
          cover_title = "Child, Adolescent, and Psychological Therapies National Dataset (CAPTND):",
          cover_subtitle = "Child and Adolescent Mental Health Services (CAMHS) Appointment 'Did Not Attend' (DNA) Report (April 2025 - June 2026)", 
          cover_date = "01 09 2026",
          toc_depth = 3
        ),
        output_file = paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", 
                             data_analysis_latest_date, "/shorewise_publication/report/CAPTND_dna_publication_",
                             dataset_choice, "_", publication_month, ".docx"))
      
    }}
  
  
}


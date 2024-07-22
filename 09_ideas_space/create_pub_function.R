

dataset_choice <- "PT" # "CAMHS"


create_pub_word_doc <- function(dataset_choice){
  
  if(dataset_choice == "PT"){
    
    dataset_label <- "Psychological Therapies"
    
  } else {
    
    if(dataset_choice == "CAMHS"){
      
      dataset_label <- "Child and Adolescent Mental Health Services"
      
    }
    
  }
  
  #dataset_label <- "Psychological Therapies" # "Child and Adolescent Mental Health Services"
  
  month_label <- format(as.Date(month_end, "%Y-%m-%d"), "%B %Y")
  
  # Render markdown document
  
  # rmarkdown::render(
  #   "./07_publication/update_2024_06/markdown/CAPTND_shorewise_pub.Rmd",
  #   output_file = paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_2024-06-28/shorewise_publication/report/CAPTND_shorewise_pub_",
  #                        dataset_choice, "_", month_end, ".docx") # change this to change output file name
  # )
  
  rmarkdown::render(
    paste0(markdown_dir, "/CAPTND_shorewise_pub.Rmd"),
    output_file = paste0(shorewise_pub_report_dir, "/CAPTND_shorewise_pub_",
                         dataset_choice, "_", month_end, ".docx") # change this to change output file name
  )
  
}

create_pub_word_doc(dataset_choice = "PT")

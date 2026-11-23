library(dplyr)
library(purrr)
library(readr)

# written by Alan Coventry, November 2026

# classifies data frame structure, exports as a csv
# very useful for building an AI knowledge base 

export_structure <- function(df, file_name) {
  
  structure <- tibble(
    column = names(df),
    class = map_chr(df, ~ paste(class(.x), collapse = ", ")),
    typeof = map_chr(df, typeof),
    n_rows = nrow(df),
    n_missing = map_int(df, ~ sum(is.na(.x))),
    n_unique = map_int(df, ~ n_distinct(.x, na.rm = TRUE)),
    example_values = map_chr(
      df,
      ~ paste(head(unique(na.omit(as.character(.x))), 5), collapse = " | ")
    )
  )
  
  write_csv(structure, file_name)
}

# OUTPUT and useage - PLEASE CHANGE DATAFRAME, EXPORT DIRECTORY, AND NAME ACCORDINGLY
export_structure(
  df,
  "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/alan/df_swift_opti_data_structure.csv"
)
#CAPTND DUMMY DATA Dashboard - Test 002
#Alan Coventry
#11/06/2025

#### Packages ####
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

#### Load & Prep Data ####
# Load data
dummy_data_002 <- read_csv("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/Dashboard/DummyData_002.csv")

# Ensure ordered by quarter
dummy_data_002$Quarter <- factor(dummy_data_002$Quarter, levels = sort(unique(dummy_data_002$Quarter)))

# Unique variables for tab creation
variables <- unique(dummy_data_002$Variable)

#### UI ####
ui <- fluidPage(
  titlePanel("CAPTND DUMMY DATA Dashboard - Test 002"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataset", "Select Dataset:",
                   choices = unique(dummy_data_002$DataSet),
                   selected = unique(dummy_data_002$DataSet)[1]),
      
      selectInput("health_board", "Select Health Board:",
                  choices = unique(dummy_data_002$HealthBoard),
                  selected = unique(dummy_data_002$HealthBoard)[1]),
      
      radioButtons("measurement", "Select Measurement Type:",
                   choices = c("SeenWithin18Weeks (Total)" = "SeenWithin18Weeks",
                               "TotalSeen" = "TotalSeen",
                               "SeenWithin18Weeks (%)" = "Percentage"),
                   selected = "SeenWithin18Weeks")
    ),
    
    mainPanel(
      do.call(tabsetPanel, c(id = "variable_tabs", lapply(variables, function(var_name) {
        tabPanel(title = var_name,
                 plotlyOutput(outputId = paste0("plot_", gsub(" ", "_", var_name)))  # <- changed
        )
      })))
    )
  )
)

#### Server ####
server <- function(input, output) {
  
  for (var_name in variables) {
    local({
      var <- var_name
      output_id <- paste0("plot_", gsub(" ", "_", var))
      
      output[[output_id]] <- renderPlotly({
        filtered_data <- dummy_data_002 %>%
          filter(DataSet == input$dataset,
                 HealthBoard == input$health_board,
                 Variable == var) %>%
          mutate(Percentage = ifelse(TotalSeen > 0,
                                     SeenWithin18Weeks / TotalSeen * 100,
                                     NA))
        
        y_value <- switch(input$measurement,
                          "SeenWithin18Weeks" = round(filtered_data$SeenWithin18Weeks, 1),
                          "TotalSeen" = round(filtered_data$TotalSeen, 1),
                          "Percentage" = round(filtered_data$Percentage, 1))
        
        filtered_data$y_value <- y_value  # Add column for plotting
        filtered_data$tooltip <- paste0(
          "Quarter: ", filtered_data$Quarter, "<br>",
          "Value: ", y_value
        )
        
        if (nrow(filtered_data) == 0 || all(is.na(y_value))) {
          return(plotly_empty())
        }
        
        p <- ggplot(filtered_data, aes(x = Quarter, y = y_value, group = 1, text = tooltip)) +
          geom_line(color = "#3F3685", size = 1.2) +
          geom_point(size = 3, color = "#3F3685") +
          labs(title = paste(input$measurement, "for", var),
               x = "Quarter",
               y = input$measurement) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ylim(0, NA)  # <- Always start y-axis at zero
        
        ggplotly(p, tooltip = "text")
      })
    })
  }
}

#### Run the app ####
shinyApp(ui = ui, server = server)

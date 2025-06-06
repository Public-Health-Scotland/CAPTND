#CAPTND Dashboard Test Script

#utilises dummy data, creates 4 simple bar charts, with Quarter determining data fed into each bar chart
#created by Alan Coventry
#06/06/2025

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Load data
camhs_data <- read_csv("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/Dashboard/DummyData_001.csv")

# UI
ui <- fluidPage(
  titlePanel("CAMHS Waiting Times Dashboard (Dummy Data)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("quarter", "Select Quarter:",
                  choices = unique(camhs_data$Quarter),
                  selected = "Q1 2024")
    ),
    mainPanel(
      plotOutput("waiting_times_plot")
    )
  )
)

# Server
server <- function(input, output) {
  output$waiting_times_plot <- renderPlot({
    filtered_data <- camhs_data %>%
      filter(Quarter == input$quarter) %>%
      mutate(PercentageSeen = (SeenWithin18Weeks / TotalSeen) * 100)
    
    ggplot(filtered_data, aes(x = HealthBoard, y = PercentageSeen, fill = HealthBoard)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Percentage Seen Within 18 Weeks -", input$quarter),
           x = "Health Board",
           y = "Percentage (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

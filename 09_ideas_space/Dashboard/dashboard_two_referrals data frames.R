#### Packages ####
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)


#### UI ####
ui <- fluidPage(
  titlePanel("Referrals Dashboard"),
  
  tabsetPanel(
    id = "data_tab",
    tabPanel("Referral Acceptance Status",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("dataset_type_accept", "Select Dataset Type:",
                              choices = unique(ref_accept_df$dataset_type),
                              selected = unique(ref_accept_df$dataset_type)[1]),
                 selectInput("health_board_accept", "Select Health Board:",
                             choices = unique(ref_accept_df$hb_name),
                             selected = unique(ref_accept_df$hb_name)[1]),
                 radioButtons("measure_type_accept", "Select Measurement Type:",
                              choices = unique(ref_accept_df$measure_type),
                              selected = unique(ref_accept_df$measure_type)[1])
               ),
               mainPanel(
                 plotlyOutput("accept_plot")
               )
             )
    ),
    tabPanel("Referrals by Sex",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("dataset_type_master", "Select Dataset Type:",
                              choices = unique(ref_master_df$dataset_type),
                              selected = unique(ref_master_df$dataset_type)[1]),
                 selectInput("health_board_master", "Select Health Board:",
                             choices = unique(ref_master_df$hb_name),
                             selected = unique(ref_master_df$hb_name)[1]),
                 radioButtons("measure_type_master", "Select Measurement Type:",
                              choices = unique(ref_master_df$measure_type),
                              selected = unique(ref_master_df$measure_type)[1])
               ),
               mainPanel(
                 plotlyOutput("master_plot")
               )
             )
    )
  )
)

#### Server ####
server <- function(input, output, session) {
  
  # Update Health Board dropdown dynamically for ref_accept_df
  observe({
    hb_choices <- ref_accept_df %>%
      filter(dataset_type == input$dataset_type_accept) %>%
      pull(hb_name) %>%
      unique()
    updateSelectInput(session, "health_board_accept", choices = hb_choices, selected = hb_choices[1])
  })
  
  # Plot for ref_accept_df
  output$accept_plot <- renderPlotly({
    filtered_data <- ref_accept_df %>%
      filter(dataset_type == input$dataset_type_accept,
             hb_name == input$health_board_accept,
             measure_type == input$measure_type_accept)
    
    p <- ggplot(filtered_data, aes(x = quarter_ending, y = count, group = measure_breakdown, color = measure_breakdown)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = paste("Referrals Acceptance Status -", input$measure_type_accept),
           x = "Quarter Ending",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Update Health Board dropdown dynamically for ref_master_df
  observe({
    hb_choices <- ref_master_df %>%
      filter(dataset_type == input$dataset_type_master) %>%
      pull(hb_name) %>%
      unique()
    updateSelectInput(session, "health_board_master", choices = hb_choices, selected = hb_choices[1])
  })
  
  # Plot for ref_master_df
  output$master_plot <- renderPlotly({
    filtered_data <- ref_master_df %>%
      filter(dataset_type == input$dataset_type_master,
             hb_name == input$health_board_master,
             measure_type == input$measure_type_master)
    
    p <- ggplot(filtered_data, aes(x = quarter_ending, y = count, group = measure_breakdown, color = measure_breakdown)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = paste("Referrals by Sex -", input$measure_type_master),
           x = "Quarter Ending",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

#### Run App ####
shinyApp(ui = ui, server = server)

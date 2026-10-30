
library(shiny)
library(dplyr)
library(networkD3)
library(htmlwidgets)

ui <- fluidPage(
  
  titlePanel("Alluvial chart showing referrals for patients where the health board of residence 
             differs to the health board of treatment"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "dataset_type",
        label = "Dataset type",
        choices = sort(unique(hb_treat_res_df$dataset_type)),
        selected = sort(unique(hb_treat_res_df$dataset_type))[1]
      ),
      
      selectInput(
        inputId = "year",
        label = "Year",
        choices = sort(unique(hb_treat_res_df$year), decreasing = TRUE),
        selected = max(hb_treat_res_df$year, na.rm = TRUE)
      ),
      
      selectInput(
        inputId = "residence_hb",
        label = "Health Board of residence",
        choices = sort(unique(hb_treat_res_df$residence_hb)),
        selected = sort(unique(hb_treat_res_df$residence_hb))[1]
      )
      
    ),
    
    mainPanel(
      
      sankeyNetworkOutput(
        outputId = "sankey_chart",
        height = "700px"
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  # Update available years when dataset type changes
  observeEvent(input$dataset_type, {
    
    available_years <- hb_treat_res_df |>
      filter(dataset_type == input$dataset_type) |>
      distinct(year) |>
      arrange(desc(year)) |>
      pull(year)
    
    updateSelectInput(
      session = session,
      inputId = "year",
      choices = available_years,
      selected = available_years[1]
    )
    
  })
  
  
  # Update available residence HBs when dataset type/year changes
  observeEvent(
    list(input$dataset_type, input$year),
    {
      
      req(input$dataset_type, input$year)
      
      available_boards <- hb_treat_res_df |>
        filter(
          dataset_type == input$dataset_type,
          year == as.integer(input$year)
        ) |>
        distinct(residence_hb) |>
        arrange(residence_hb) |>
        pull(residence_hb)
      
      updateSelectInput(
        session = session,
        inputId = "residence_hb",
        choices = available_boards,
        selected = available_boards[1]
      )
      
    }
  )
  
  
  # Filter data according to selections
  filtered_data <- reactive({
    
    req(
      input$dataset_type,
      input$year,
      input$residence_hb
    )
    
    hb_treat_res_df |>
      filter(
        dataset_type == input$dataset_type,
        year == as.integer(input$year),
        residence_hb == input$residence_hb,
        !is.na(treatment_hb),
        !is.na(referral_status),
        !is.na(patient_count),
        patient_count > 0
      )
    
  })
  
  
  output$sankey_chart <- renderSankeyNetwork({
    
    sankey_data <- filtered_data()
    
    validate(
      need(
        nrow(sankey_data) > 0,
        "No patient flows are available for the selected filters."
      )
    )
    
    
    # -----------------------------------------
    # Stage 1: Residence HB -> Referral status
    # -----------------------------------------
    
    links_stage1 <- sankey_data |>
      group_by(
        residence_hb,
        referral_status
      ) |>
      summarise(
        value = sum(patient_count, na.rm = TRUE),
        .groups = "drop"
      ) |>
      transmute(
        source_key = paste0("Residence: ", residence_hb),
        target_key = paste0("Status: ", referral_status),
        value
      )
    
    
    # -----------------------------------------
    # Stage 2: Referral status -> Treatment HB
    # -----------------------------------------
    
    links_stage2 <- sankey_data |>
      group_by(
        referral_status,
        treatment_hb
      ) |>
      summarise(
        value = sum(patient_count, na.rm = TRUE),
        .groups = "drop"
      ) |>
      transmute(
        source_key = paste0("Status: ", referral_status),
        target_key = paste0("Treatment: ", treatment_hb),
        value
      )
    
    
    # Combine the two stages
    sankey_links <- bind_rows(
      links_stage1,
      links_stage2
    )
    
    
    # -----------------------------------------
    # Create nodes
    # -----------------------------------------
    
    nodes <- data.frame(
      node_key = unique(c(
        sankey_links$source_key,
        sankey_links$target_key
      ))
    ) |>
      mutate(
        
        name = case_when(
          grepl("^Residence: ", node_key) ~ sub("^Residence: ", "", node_key),
          grepl("^Status: ", node_key) ~ sub("^Status: ", "", node_key),
          grepl("^Treatment: ", node_key) ~ sub("^Treatment: ", "", node_key)
        ),
        
        node_group = case_when(
          grepl("^Residence: ", node_key) ~ "Residence",
          node_key == "Status: Accepted" ~ "Accepted",
          node_key == "Status: Rejected" ~ "Rejected",
          grepl("^Treatment: ", node_key) ~ "Treatment"
        )
        
      )
    
    
    # -----------------------------------------
    # Convert node names to networkD3 IDs
    # -----------------------------------------
    
    links <- sankey_links |>
      mutate(
        source = match(source_key, nodes$node_key) - 1,
        target = match(target_key, nodes$node_key) - 1
      ) |>
      select(
        source,
        target,
        value
      )
    
    
    # -----------------------------------------
    # Node colours
    # -----------------------------------------
    
    colour_scale <- htmlwidgets::JS(
      'd3.scaleOrdinal()
        .domain(["Residence", "Accepted", "Rejected", "Treatment"])
        .range(["#005EB8", "#28A745", "#DC3545", "#41A5EE"])'
    )
    
    
    # -----------------------------------------
    # Create Sankey chart
    # -----------------------------------------
    
    sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      NodeGroup = "node_group",
      colourScale = colour_scale,
      fontSize = 13,
      nodeWidth = 30,
      nodePadding = 15,
      sinksRight = TRUE
    )
    
  })
  
}


shinyApp(
  ui = ui,
  server = server
)


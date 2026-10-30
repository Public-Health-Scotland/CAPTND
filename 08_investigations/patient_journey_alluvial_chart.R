


#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

df_agg_steps <- df |>
  filter(is_case_closed == TRUE) |> #only use discharged cases for now
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(discharge_year = lubridate::year(case_closed_date), #year of discharge
         assessed = case_when(any(app_purpose %in% c(1,3) & att_status == 1) ~ 'Assessed',
                              TRUE ~ 'Not assessed'),
         treated = case_when(any(app_purpose %in% c(2,3,5) & att_status == 1) ~ 'Treated',
                             TRUE ~ 'Not treated'),
         app_n = sum(record_type_label == "Appointment", na.rm = TRUE),
         app_n_cat = case_when(app_n > 0 & app_n <= 2 ~ '1 to 2 apps',
                               app_n > 2 & app_n <= 5 ~ '3 to 5 apps',
                               app_n > 5 & app_n <= 10 ~ '6 to 10 apps',
                               app_n > 10 & app_n <= 15 ~ '11 to 15 apps',
                               app_n > 15 & app_n <= 20 ~ '16 to 20 apps',
                               app_n > 20 & app_n <= 25 ~ '21 to 25 apps',
                               app_n > 25 ~ 'More than 25 apps',
                               TRUE ~ 'No apps')) |> ungroup() |>
  select(dataset_type, hb_name, ucpn, chi, rtt_eval, assessed, treated, app_n_cat, discharge_year) |>
  distinct() |>
  mutate(treated = case_when(rtt_eval == 'case closed due to non attendance' ~ 'Not treated - non attendance',
                             rtt_eval == 'case closed after assessment' ~ 'Treatment not required',
                             rtt_eval == 'rtt not possible - attended app but no purpose' ~ 'Attended app but no purpose',
                             TRUE ~ treated),
         assessed = case_when(rtt_eval == 'case closed due to non attendance' &
                                assessed == 'Not assessed' ~ 'Not assessed - non attendance',
                              TRUE ~ assessed),
         acceptance = case_when(rtt_eval == 'referral not accepted' ~ 'Not accepted',
                                TRUE ~ 'Accepted'),
         discharge = 'Discharged') |>
  group_by(dataset_type, hb_name, discharge_year, acceptance, assessed, treated, app_n_cat, discharge) |>
  summarise(count = n())


library(shiny)
library(networkD3)
library(htmlwidgets)


ui <- fluidPage(
  
  titlePanel("Patient pathway"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "hb_name",
        label = "Health board:",
        choices = sort(unique(df_agg_steps$hb_name)),
        selected = sort(unique(df_agg_steps$hb_name))[1]
      ),
      
      selectInput(
        inputId = "dataset_type",
        label = "Dataset:",
        choices = sort(unique(df_agg_steps$dataset_type)),
        selected = sort(unique(df_agg_steps$dataset_type))[1]
      ),
      
      selectInput(
        inputId = "discharge_year",
        label = "Discharge year:",
        choices = sort(unique(df_agg_steps$discharge_year)),
        selected = max(df_agg_steps$discharge_year, na.rm = TRUE)
      )
    ),
    
    mainPanel(
      
      networkD3::sankeyNetworkOutput(
        outputId = "pathway_sankey",
        height = "700px"
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    req(
      input$hb_name,
      input$dataset_type,
      input$discharge_year
    )
    
    df_agg_steps %>%
      filter(
        hb_name == input$hb_name,
        dataset_type == input$dataset_type,
        discharge_year == as.integer(input$discharge_year)
      )
    
  })
  
  
  sankey_data <- reactive({
    
    df_sankey <- filtered_data()
    
    validate(
      need(
        nrow(df_sankey) > 0,
        "No data are available for the selected filters."
      )
    )
    
    # Acceptance -> Assessment
    links_1 <- df_sankey %>%
      filter(
        !is.na(acceptance),
        !is.na(assessed)
      ) %>%
      group_by(acceptance, assessed) %>%
      summarise(
        value = sum(count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      transmute(
        source = paste0("acceptance_", acceptance),
        target = paste0("assessed_", assessed),
        value
      )
    
    # Assessment -> Treatment
    links_2 <- df_sankey %>%
      filter(
        !is.na(assessed),
        !is.na(treated)
      ) %>%
      group_by(assessed, treated) %>%
      summarise(
        value = sum(count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      transmute(
        source = paste0("assessed_", assessed),
        target = paste0("treated_", treated),
        value
      )
    
    # Treatment -> Appointment count
    links_3 <- df_sankey %>%
      filter(
        !is.na(treated),
        !is.na(app_n_cat)
      ) %>%
      group_by(treated, app_n_cat) %>%
      summarise(
        value = sum(count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      transmute(
        source = paste0("treated_", treated),
        target = paste0("appointments_", app_n_cat),
        value
      )
    
    # Appointment count -> Discharge
    links_4 <- df_sankey %>%
      filter(
        !is.na(app_n_cat),
        !is.na(discharge)
      ) %>%
      group_by(app_n_cat, discharge) %>%
      summarise(
        value = sum(count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      transmute(
        source = paste0("appointments_", app_n_cat),
        target = paste0("discharge_", discharge),
        value
      )
    
    links <- bind_rows(
      links_1,
      links_2,
      links_3,
      links_4
    ) %>%
      filter(value > 0)
    
    validate(
      need(
        nrow(links) > 0,
        "No patient pathways are available for the selected filters."
      )
    )
    
    nodes <- tibble(
      id = unique(c(links$source, links$target))
    ) %>%
      mutate(
        name = sub("^[^_]+_", "", id)
      )
    
    links <- links %>%
      mutate(
        source_id = match(source, nodes$id) - 1,
        target_id = match(target, nodes$id) - 1
      )
    
    list(
      links = links,
      nodes = nodes
    )
    
  })
  
  
  output$pathway_sankey <- networkD3::renderSankeyNetwork({
    
    sankey <- sankey_data()
    
    p <- networkD3::sankeyNetwork(
      Links = sankey$links,
      Nodes = sankey$nodes,
      Source = "source_id",
      Target = "target_id",
      Value = "value",
      NodeID = "name",
      fontSize = 14,
      nodeWidth = 30,
      nodePadding = 15,
      sinksRight = TRUE)
    
    htmlwidgets::onRender(p,
                          '
  function(el, x) {

    d3.select(el)
      .selectAll(".node")
      .select("title")
      .text(function(d) {

        var value = d.value.toLocaleString();
        var label = d.name.toLowerCase();

        if (label === "accepted")
          return value + " accepted patients";

        if (label === "not accepted")
          return value + " patients not accepted";

        if (label === "assessed")
          return value + " assessed patients";

        if (label === "not assessed")
          return value + " patients not assessed";

        if (label === "not assessed - non attendance")
          return value + " patients not assessed - non-attendance";

        if (label === "treated")
          return value + " treated patients";

        if (label === "not treated")
          return value + " patients not treated";

        if (label === "not treated - non attendance")
          return value + " patients not treated - non-attendance";

        if (label === "treatment not required")
          return value + " patients not requiring treatment";

        if (label === "discharge")
          return value + " discharged patients";

        return value + " patients";
      });

  }
  '
    )
    
  })
  
}


shinyApp(ui = ui, 
         server = server)


#CAPTND DUMMY DATA Dashboard - Test 002
#Alan Coventry
#11/06/2025

#### 0) Packages ####
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

#### 1) Load & Prep Data ####
# Load data
# dummy_data_002 <- read_csv("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/Dashboard/DummyData_002.csv")
# 
# # Ensure ordered by quarter
# dummy_data_002$Quarter <- factor(dummy_data_002$Quarter, levels = sort(unique(dummy_data_002$Quarter)))
# 
# # Unique variables for tab creation
# variables <- unique(dummy_data_002$Variable)

#Script to create datatables for Shiny dashboard

     ##### 1.1 Referral demographics #####
ref_sex_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_sex.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by sex') |>
  rename(measure_breakdown = sex_reported) |>
  pivot_longer(cols = c('count', 'total', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_age_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_age.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by age') |>
  rename(measure_breakdown = agg_age_groups) |>
  pivot_longer(cols = c('count', 'total', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_simd_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_simd.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by SIMD') |>
  rename(measure_breakdown = simd2020_quintile) |>
  pivot_longer(cols = c('count', 'total', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_master_df <- rbind(ref_sex_df, ref_age_df, ref_simd_df) |>
  mutate(measure_type = recode(measure_type,
                              "Count" = "count",
                              "Total" = "total",
                              "Proportion" = "prop",
                              "Rate per 1,000 population" = "pop_rate_1000"))

     ##### 1.2 Referral acceptance #####
ref_accept_df <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance/non_acceptance_summary_quarter_hb.parquet")) |>
  mutate(measure_name = 'Referrals acceptance status') |>
  rename(measure_breakdown = ref_acc_desc) |>
  pivot_longer(cols = c('count', 'total', 'prop'),
               names_to = 'measure_type',
               values_to = 'count')


     ##### 1.3 Appointment attendance #####
appt_att_df <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |>
  mutate(measure_name = 'Appointment attendance status') |>
  rename(measure_breakdown = Attendance,
         Count = apps_att,
         Total = total_apps,
         Percentage = prop_apps_att) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

     ##### 1.4 First contact attendance #####
first_con_df <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb.parquet")) |>
  select(-total_apps) |>
  mutate(measure_name = 'First contact attendance status') |>
  rename(measure_breakdown = Attendance,
         Count = firstcon_att,
         Total = first_contact,
         Percentage = prop_firstcon_att) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

     ##### 1.5 Referral source #####
df_ref_source <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/ref_source_month_hb.parquet")) |>
  mutate(measure_name = 'Referral source') |>
  rename(measure_breakdown = ref_source_name,
         Count = count,
         Total = total,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

     ##### 1.6 Appointment care locations #####
df_care_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_mth_hb.parquet")) |>
  mutate(measure_name = 'Appointment care location') |>
  rename(measure_breakdown = loc_label,
         Count = count,
         Total = total_apps,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

     ##### 1.7 Care professionals #####
df_prof_group <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_mth_hb.parquet")) |>
  mutate(measure_name = 'Care professional') |>
  rename(measure_breakdown = prof_label,
         Count = count,
         Total = total_apps,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

     ##### 1.8 First contact #####

     ##### 1.9 Waiting list #####

     ##### 1.10 Patients seen #####

     ##### 1.11 Open cases #####
    
    
    
    
##### UI ####
ui <- fluidPage(
  titlePanel("CAPTND Test Dashboard - Test 003"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataset", "Select Dataset:",
                   choices = unique(ref_master_df$dataset_type),
                   selected = unique(ref_master_df$dataset_type)[1]),
      
      selectInput("health_board", "Select Health Board:",
                  choices = unique(ref_master_df$hb_name),
                  selected = unique(ref_master_df$hb_name)[1]),
      
      radioButtons("measurement", "Select Measurement Type:",
                   choices = c("Count" = "count",
                               "Total" = "total",
                               "Proportion" = "prop",
                               "Rate per 1,000 population" = "pop_rate_1000"),
                   selected = "count")
    ),
    
    mainPanel(
      do.call(tabsetPanel, c(id = "variable_tabs", lapply(variables, function(var_name) {
        tabPanel(title = var_name,
                 plotlyOutput(outputId = paste0("plot_", gsub(" ", "_", var_name))))
      })))
    )
  )
)


##### Server ####
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

##### Run the app ####
shinyApp(ui = ui, server = server)

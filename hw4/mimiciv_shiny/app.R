library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(gtsummary)

mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"

# BigQuery authentication using service account
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

transfer_info <- tbl(con_bq, "transfers")
labevents_info <- tbl(con_bq, "labevents")
procedures_info <- tbl(con_bq, "procedures_icd")
procedures_id <- tbl(con_bq, "d_icd_procedures")

diagnoses_info <- tbl(con_bq, "diagnoses_icd")
diagnoses_id <- tbl(con_bq, "d_icd_diagnoses")

chartevents_info <- tbl(con_bq, "chartevents")

# UI
ui <- fluidPage(
  titlePanel("MIMIC-IV ICU Cohort Explorer"),
  
  ### Table 1
  tabsetPanel(
    # Summary Tab
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(
                 # Variable group selection
                 selectInput("variable_group", "Variable Group", 
                             choices = c("Demographics" = "demo",
                                         "Lab Measurements" = "lab_measure",
                                         "Vitals" = "vital"),
                             selected = "demo"),
                 
                 # Specific variable selection (dynamically updated)
                 selectInput("variable", "Variable", choices = NULL),
                 
                 # Range slider for numerical variables (dynamically generated)
                 uiOutput("slider_ui")
               ),
               
               mainPanel(
                 plotOutput("plot1"),  # Display the histogram or bar plot
                 DTOutput("summaryTable")  # Display the summary table
               )
             )
    ),
    
    # Patient Info Tab
    tabPanel("Patient Info",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a patient"),  # Instruction text
                 selectizeInput("patient_id", "Patient ID", 
                                choices = NULL, options = list(maxItems = 1)),  # Patient ID selection
                 actionButton("search", "Submit"),  # Search button
                 
                 # Plot type selection (ADT timeline or ICU vitals)
                 selectInput("type", "Select a plot",
                             choices = c("ADT", "ICU"),
                             selected = "ADT")
               ),
               mainPanel(
                 plotOutput("plot2")  # Display the selected plot
               )
             ))
  )
)

#Server
server <- function(input, output, session) {
  # Define variable groups
  variable_choices <- list(
    demo = c("age_intime", "race", "gender", "language", "insurance", 
             "material_status"), 
    lab_measure = c("hematocrit", "bicarbonate", "glucose", "chloride", "wbc", 
                    "potassium", "creatinine", "sodium"),
    vital = c("heart_rate", "temperature_fahrenheit",
              "non-invasive_blood_pressure_systolic",
              "non-invasive_blood_pressure_diastolic",
              "respiratory_rate")
  )
  
  # Observe changes in variable_group and update variable options dynamically
  observe({
    req(input$variable_group)
    updateSelectInput(session, "variable",
                      choices = variable_choices[[input$variable_group]],
                      selected = variable_choices[[input$variable_group]][1])
  })
  
  # Dynamically update the slider based on selected variable
  observe({
    req(input$variable)
    var_data <- mimic_icu_cohort[[input$variable]]
    
    if (is.numeric(var_data)) {
      min_val <- min(var_data, na.rm = TRUE)
      max_val <- max(var_data, na.rm = TRUE)
      
      output$slider_ui <- renderUI({
        sliderInput("xlim", "Select Range:", 
                    min = floor(min_val), 
                    max = ceiling(max_val), 
                    value = c(floor(min_val), ceiling(max_val)))
      })
    } else {
      output$slider_ui <- renderUI({ NULL })  # Hide slider for categorical variables
    }
  })
  
  # Reactive function to get the selected variable
  variable_reactive <- reactive({
    req(input$variable)
    req(input$variable %in% names(mimic_icu_cohort))  # Ensure the variable exists
    input$variable
  })
  
  # Generate plots based on the selected variable
  output$plot1 <- renderPlot({
    variable <- variable_reactive()
    req(variable)
    
    plot_data <- mimic_icu_cohort
    
    # Filter numeric variables based on selected range from the slider
    if (!is.null(input$xlim) && is.numeric(plot_data[[variable]])) {
      plot_data <- plot_data %>%
        filter(between(!!sym(variable), input$xlim[1], input$xlim[2]))
    }
    
    # **For numerical variables: Histogram**
    if (is.numeric(plot_data[[variable]])) {
      ggplot(plot_data, aes_string(x = variable)) +
        geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", variable),
             x = variable, y = "Count")
      
      # **For categorical variables: Bar Chart**
    } else {
      plot_data <- plot_data %>%
        count(!!sym(variable), name = "count")  # Count occurrences of each category
      
      ggplot(plot_data, aes_string(x = variable, y = "count")) +
        geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Bar Chart of", variable), x = variable, 
             y = "Count") +
        coord_flip()  # Flip X-axis for better readability
    }
  })

  
  output$summaryTable <- renderDT({
    req(input$variable)
    req(input$variable %in% names(mimic_icu_cohort))
    
    var_data <- mimic_icu_cohort[[input$variable]]
    
    if (is.numeric(var_data)) {
      summary_data <- data.frame(
        Statistic = c("Min", "1st Qu", "Median", "3rd Qu", "Max", "NA Count"),
        Value = c(
          min(var_data, na.rm = TRUE),
          quantile(var_data, 0.25, na.rm = TRUE),
          median(var_data, na.rm = TRUE),
          quantile(var_data, 0.75, na.rm = TRUE),
          max(var_data, na.rm = TRUE),
          sum(is.na(var_data))
        )
      )
    } else {
      summary_data <- as.data.frame(table(var_data, useNA = "ifany"))
      colnames(summary_data) <- c("Category", "Count")
    }
    
    datatable(summary_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
  ### Patient Information
  
  # Update patient ID selection dynamically
  observe({
    updateSelectizeInput(session, "patient_id", choices = unique(mimic_icu_cohort$subject_id), server = TRUE)
  })
  
  # Generate plot title with patient demographics
  plot_title_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    patient_info <- mimic_icu_cohort[mimic_icu_cohort$subject_id == sub_id, ]
    
    gender <- patient_info$gender[1]
    age <- patient_info$age_intime[1]
    race <- patient_info$race[1]
    
    paste("Patient", sub_id, "-", gender, ",", age, "years old,", tolower(race))
  })
  
  # Retrieve patient ADT (Admission, Discharge, Transfer) records
  transfers_info_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    transfer_info |>
      filter(subject_id == sub_id & eventtype != "discharge") |>
      collect()
  })
  
  # Retrieve patient Lab Events
  labevents_info_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    labevents_info |>
      filter(subject_id == sub_id) |>
      select(charttime) |>
      collect()
  })
  
  # Retrieve patient Procedures
  procedures_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    procedures_info |>
      filter(subject_id == sub_id) |>
      select(chartdate, icd_code, icd_version) |>
      left_join(procedures_id, by = c("icd_code" = "icd_code", 
                                      "icd_version" = "icd_version")) |>
      select(chartdate, long_title) |>
      collect()
  })
  
  # Retrieve patient Diagnoses (ensuring unique and most relevant ones)
  diagnoses_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    diagnoses_info |>
      filter(subject_id == sub_id) |>
      select(seq_num, icd_code, icd_version) |>
      # Ensure unique (icd_code, icd_version) combinations before joining
      distinct(icd_code, icd_version) |> 
      # Left join to get long_title after filtering unique values
      left_join(diagnoses_id, by = c("icd_code" = "icd_code", 
                                     "icd_version" = "icd_version")) |>
      select(long_title) |>
      collect() |>
      # Select the top 3 unique diagnoses
      slice(1:3) 
  })
  
  # Retrieve patient ICU Vitals
  chartevents_info_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    chartevents_info |>
      filter(subject_id == sub_id & itemid %in% c(220045, 220179, 
                                                  220180, 220210, 223761)) |>
      select(stay_id, charttime, itemid, value) |>
      collect() |>
      mutate(
        itemid = case_when(
          itemid == 220045 ~ "HR",
          itemid == 220180 ~ "NBPd",
          itemid == 220179 ~ "NBPs",
          itemid == 220210 ~ "RR",
          itemid == 223761 ~ "Temperature F"
        ),
        charttime = as.POSIXct(charttime),
        value = as.numeric(value)
      )
  })
  
  # Generate ADT / ICU Visualization
  output$plot2 <- renderPlot({
    req(input$patient_id, input$type)
    
    if (input$type == "ADT") {
      # Retrieve required data
      transfers_info <- transfers_info_reactive()
      labevents_info <- labevents_info_reactive()
      procedures <- procedures_reactive()
      diagnoses <- diagnoses_reactive()
      plot_title <- plot_title_reactive()
      
      # Extract lab event timestamps
      labtime <- labevents_info %>% pull(charttime)
      procedures$chartdate <- as.POSIXct(as.character(procedures$chartdate), format = "%Y-%m-%d")
      
      # Generate subtitle with top 3 diagnoses
      plot_subtitle <- paste(diagnoses$long_title, collapse = "\n")
      
      # **Adjust segment size: ICU/CCU lines thicker**
      transfers_info <- transfers_info %>%
        mutate(line_size = ifelse(str_detect(careunit, "ICU|CCU"), 5, 2))
      
      ggplot() +
        # **ADT Timeline (colored by Care Unit)**
        geom_segment(
          data = transfers_info, 
          aes(x = intime, xend = outtime, y = "ADT", yend = "ADT", 
              color = careunit, linewidth = line_size), 
          alpha = 0.8
        ) +
        
        # **Lab Events (Black markers with transparency)**
        geom_point(data = data.frame(charttime = labtime, y = "Lab"),
                   aes(x = charttime, y = y), shape = 3, color = "black", alpha = 0.6) +
        
        # **Procedures (Different shapes & colors)**
        geom_point(data = procedures, 
                   aes(x = chartdate, y = "Procedure", 
                       shape = long_title, color = long_title),
                   size = 5, alpha = 0.8) +
        
        scale_shape_manual(values = c(16, 17, 18, 15, 3, 8), 
                           labels = unique(procedures$long_title)) +
        
        # **Title and Labels**
        labs(
          title = plot_title,
          subtitle = plot_subtitle,
          x = "Calendar Time",
          y = ""
        ) +
        
        # **Adjust Y-axis order**
        scale_y_discrete(limits = c("Procedure", "Lab", "ADT")) +
        
        # **Minimalistic Theme with Increased Readability**
        theme_minimal(base_size = 20) + 
        theme(
          legend.position = "bottom",
          legend.box = "vertical",
          legend.key = element_blank(),
          legend.key.width = unit(1, "cm"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 14)
        )
    }
    
    # **ICU Vitals Visualization**
    else {
      plot_title <- paste("Patient", input$patient_id, "- ICU Vitals Over Time")
      chartevents_info <- chartevents_info_reactive()
      
      ggplot(chartevents_info) +
        geom_line(aes(x = charttime, y = value, group = itemid, color = itemid), linewidth = 1.2) + 
        geom_point(aes(x = charttime, y = value, color = itemid), size = 2.3, alpha = 0.65) +
        
        # **Facet by Item and Stay ID**
        facet_grid(itemid ~ stay_id, scales = "free") +
        
        # **Title and Labels**
        labs(title = plot_title, x = "", y = "") +
        
        # **Theme Adjustments**
        theme_minimal(base_size = 14) + 
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 15, hjust = 1, size = 12),
          strip.text.y = element_text(size = 14)
        )
      
    }
  })
}

shinyApp(ui, server)

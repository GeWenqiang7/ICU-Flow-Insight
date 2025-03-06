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
                 # 变量分类选择器
                 selectInput("variable_group", "Variable Group", 
                             choices = c("Demographics" = "demo",
                                         "Lab Measurements" = "lab_measure",
                                         "Vitals" = "vital"),
                             selected = "demo"),
                 
                 # 具体变量选择器（动态更新）
                 selectInput("variable", "Variable", choices = NULL),
                 
                 # 变量范围滑动条（动态）
                 uiOutput("slider_ui")
                 
               ),
               
               mainPanel(
                 plotOutput("plot1"),
                 DTOutput("summaryTable")
               )
             )
    ),
    
    # Patient Info Tab
    tabPanel("Patient Info",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a patient"),
                 selectizeInput("patient_id", "Patient ID", 
                                choices = NULL, options = list(maxItems = 1)),
                 actionButton("search", "Submit"),  
                 selectInput("type", "Select a plot",
                             choices = c("ADT", "ICU"),
                             selected = "ADT")
               ),
               mainPanel(
                 plotOutput("plot2")  
               )
             ))
  )
)

server <- function(input, output, session) {
  # 变量分组
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
  
  # 监听 variable_group 变化，动态更新 variable 选项
  observe({
    req(input$variable_group)
    updateSelectInput(session, "variable",
                      choices = variable_choices[[input$variable_group]],
                      selected = variable_choices[[input$variable_group]][1])
  })
  
  # 动态更新滑动条
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
      output$slider_ui <- renderUI({ NULL })  # 如果是分类变量，不显示滑动条
    }
  })
  
  # 选择变量
  variable_reactive <- reactive({
    req(input$variable)
    req(input$variable %in% names(mimic_icu_cohort))  # 确保变量存在
    input$variable
  })
  
  # 生成图表
  output$plot1 <- renderPlot({
    variable <- variable_reactive()
    req(variable)
    
    plot_data <- mimic_icu_cohort
    
    # 过滤数值型变量的 x 轴范围
    if (!is.null(input$xlim) && is.numeric(plot_data[[variable]])) {
      plot_data <- plot_data %>%
        filter(between(!!sym(variable), input$xlim[1], input$xlim[2]))
    }
    
    # **数值型变量：绘制直方图**
    if (is.numeric(plot_data[[variable]])) {
      ggplot(plot_data, aes_string(x = variable)) +
        geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", variable),
             x = variable, y = "Count")
      
      # **分类变量：绘制柱状图**
    } else {
      plot_data <- plot_data %>%
        count(!!sym(variable), name = "count")  # 计算类别频数
      
      ggplot(plot_data, aes_string(x = variable, y = "count")) +
        geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Bar Chart of", variable), x = variable, 
             y = "Count") +
        coord_flip()  # 翻转 X 轴，方便展示
    }
  })
  
  # 生成 Summary Table
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

  
  
  ### Table 2
  observe({
    updateSelectizeInput(session, "patient_id", choices = unique(mimic_icu_cohort$subject_id), server = TRUE)
  })
  
  # 获取患者信息
  plot_title_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    patient_info <- mimic_icu_cohort[mimic_icu_cohort$subject_id == sub_id, ]
    
    gender <- patient_info$gender[1]
    age <- patient_info$age_intime[1]
    race <- patient_info$race[1]
    
    paste("Patient", sub_id, "-", gender, ",", age, "years old,", tolower(race))
  })
  
  # 获取患者的 ADT (住院转移记录)
  transfers_info_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    transfer_info |>
      filter(subject_id == sub_id & 
               eventtype != "discharge") |>
      collect()
  })
  
  # 获取患者的 Lab Events
  labevents_info_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    labevents_info |>
      filter(subject_id == sub_id) |>
      select(charttime) |>
      collect()
  })
  
  # 获取患者的 Procedures
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
  
  # 获取患者的 Diagnoses
  diagnoses_reactive <- reactive({
    req(input$patient_id)
    sub_id <- as.integer(input$patient_id)
    
    diagnoses_info |>
      filter(subject_id == sub_id) |>
      select(seq_num, icd_code, icd_version) |>
      left_join(diagnoses_id, by = c("icd_code" = "icd_code", 
                                     "icd_version" = "icd_version")) |>
      select(seq_num, long_title) |>
      distinct(long_title, .keep_all = TRUE) |>  # 确保保留 seq_num
      arrange(seq_num) |>  # 按 seq_num 排序，保证重要性
      collect() |>
      slice(1:3) # 选取前 3 个不同的 long_title
  })
  
  
  # 获取患者的 Vitals
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
  
  # 生成 ADT / ICU 图
  output$plot2 <- renderPlot({
    req(input$patient_id, input$type)
    
    if (input$type == "ADT") {
      transfers_info <- transfers_info_reactive()
      labevents_info <- labevents_info_reactive()
      procedures <- procedures_reactive()
      diagnoses <- diagnoses_reactive()
      plot_title <- plot_title_reactive()
      
      labtime <- labevents_info %>% pull(charttime)
      procedures$chartdate <- as.POSIXct(as.character(procedures$chartdate), format = "%Y-%m-%d")
      
      plot_subtitle <- paste(diagnoses$long_title, collapse = "\n")
      
      # **调整线段大小 (ICU/CCU 更粗)**
      transfers_info <- transfers_info %>%
        mutate(line_size = ifelse(str_detect(careunit, "ICU|CCU"), 5, 2))
      
      ggplot() +
        # **ADT 轨迹 (使用颜色区分 Care Unit)**
        geom_segment(
          data = transfers_info, 
          aes(x = intime, xend = outtime, y = "ADT", yend = "ADT", 
              color = careunit, linewidth = line_size), 
          alpha = 0.8
        ) +
        
        # **实验室检查 (黑色 + 透明度)**
        geom_point(data = data.frame(charttime = labtime, y = "Lab"),
                   aes(x = charttime, y = y), shape = 3, color = "black", alpha = 0.6) +
        
        # **手术 (颜色 + 形状)**
        geom_point(data = procedures, 
                   aes(x = chartdate, y = "Procedure", 
                       shape = long_title, color = long_title),
                   size = 5, alpha = 0.8) +
        
        scale_shape_manual(values = c(16, 17, 18, 15, 3, 8), 
                           labels = unique(procedures$long_title)) +
        labs(
          title = plot_title,
          subtitle = plot_subtitle,
          x = "Calendar Time",
          y = ""
        ) +
        
        # 颜色 & 主题
        scale_y_discrete(limits = c("Procedure", "Lab", "ADT")) +

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
    
    # ICU Vitals
    else {
      plot_title <- paste("Patient", input$patient_id, "- ICU Vitals Over Time")
      chartevents_info <- chartevents_info_reactive()
    
      ggplot(chartevents_info) +
        geom_line(aes(x = charttime, y = value, group = itemid, color = itemid), linewidth = 1.2) + 
        geom_point(aes(x = charttime, y = value, color = itemid), size = 2.3, alpha = 0.65) +
        facet_grid(itemid ~ stay_id, scales = "free") +
        labs(title = plot_title, x = "", y = "") +
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

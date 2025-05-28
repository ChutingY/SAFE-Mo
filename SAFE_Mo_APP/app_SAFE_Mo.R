rm(list = ls())
required_packages <- c(
  "dplyr", "ggplot2", "tibble", "forcats",
  "VIM", "MASS", "mice", "pROC",
  "ROCR", "foreign", "lattice"
)
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE, 
                     repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
  }
}
install_missing_packages(required_packages)
lapply(required_packages, require, character.only = TRUE)

data <- read.csv("data.csv",header = T,row.names = 1)
load(file = "SAFE_Mo_model.Rdata")
data$OS_26DAY <- as.factor(data$OS_26DAY)
train <-  data[index,];train$OS_26DAY <- as.factor(make.names(train$OS_26DAY))
test <-   data[-index,];test$OS_26DAY <- as.factor(make.names(test$OS_26DAY))
######################################################################################################
# 1. SAFE_Mo Construction ############################################################################
######################################################################################################
model_preds_train <- data.frame(predict(SAFE_Mo, newdata = train, type = "prob"))
AUC_train <- as.data.frame(t(caTools::colAUC(model_preds_train$X1, train$OS_26DAY)))
pred_train <- model_preds_train
pred_train$ID <- rownames(pred_train)
data_ <- data
data_$ID <- rownames(data_)
merged_train <- left_join(pred_train, data_, by = "ID")
rownames(merged_train) <- rownames(pred_train)
cal_train <- cbind(OS_26DAY = train$OS_26DAY, model_preds_train)
cut_point <- as.numeric(cutoff::roc(cal_train$X1, cal_train$OS_26DAY)[3])
cal_train[["SAFE_Mo_group"]] <- ifelse(cal_train$X1 > cut_point, "X1", "X2")
colnames(cal_train) <- c('OS_26DAY','SAFE_Mo', 'X2', 'SAFE_Mo_group')

######################################################################################################
# 2. Shiny App ###########################################################################################
######################################################################################################
ui <- fluidPage(
  titlePanel("Mortality Risk Assessment System (SAFE-Mo Model)"),
  sidebarLayout(
    sidebarPanel(
      h4("Data Upload"),
      fileInput("file", "Upload Feature Data (CSV Format)", accept = ".csv"),
      helpText("File must contain all required feature columns (excluding SAFE_Mo score)"),
      hr(),
      h4("Risk Threshold Specification"),
      p(strong("Predicted Risk:"), "Probability of mortality risk (range: 0-1)"),
      p(strong("Risk Classification:"), 
        span(style="color:#D55535", "X1 - High Risk"), " | ",
        span(style="color:#0071C2", "X2 - Low Risk")),
      p(strong("Required File Format:"), br(),
        "Upload a CSV file with exactly these columns (order doesn't matter):"),
      tags$ul(
        tags$li("admission_type (1=Emergency, 2=Urgent, 3=Elective)"),
        tags$li("age (years)"),
        tags$li("sbp_min (mmHg)"),
        tags$li("sbp_max (mmHg)"),
        tags$li("mbp_min (mmHg)"),
        tags$li("temperature_max (°C)"),
        tags$li("spo2_min (%)"),
        tags$li("urineoutput (mL)"),
        tags$li("lactate_min (mmol/L)"),
        tags$li("lactate_max (mmol/L)"),
        tags$li("ph_min"),
        tags$li("po2_min (mmHg)"),
        tags$li("aado2_calc_min (mmHg)"),
        tags$li("aado2_calc_max (mmHg)"),
        tags$li("wbc_min (×10⁹/L)"),
        tags$li("aniongap_min (mEq/L)"),
        tags$li("bun_min (mg/dL)"),
        tags$li("abs_monocytes_max (×10⁹/L)"),
        tags$li("inr_max"),
        tags$li("pt_max (seconds)"),
        tags$li("ptt_min (seconds)"),
        tags$li("ptt_max (seconds)"),
        tags$li("ast_min (U/L)"),
        tags$li("potassium_min (mEq/L)"),
        tags$li("sodium_min (mEq/L)"),
        tags$li("myocardial_infarct (0/1)"),
        tags$li("cerebrovascular_disease (0/1)"),
        tags$li("mild_liver_disease (0/1)"),
        tags$li("diabetes_with_cc (0/1)"),
        tags$li("malignant_cancer (0/1)"),
        tags$li("severe_liver_disease (0/1)"),
        tags$li("metastatic_solid_tumor (0/1)")
      )),
    mainPanel(tabsetPanel(
      tabPanel("Risk Prediction", h4("Risk Assessment Results"), DT::dataTableOutput("results")),
      tabPanel("Risk Distribution", plotOutput("risk_plot"), verbatimTextOutput("cutoff_info"))))))

server <- function(input, output, session) {
  
  # Get model objects from global environment
  SAFE_Mo <- get("SAFE_Mo", envir = .GlobalEnv)
  cut_point <- get("cut_point", envir = .GlobalEnv)
  
  # Read uploaded data
  raw_data <- reactive({
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Validate required features
      required_features <- setdiff(names(SAFE_Mo$trainingData), ".outcome")
      if(!all(required_features %in% names(df))) {
        stop(paste("Missing required features:", 
                   paste(setdiff(required_features, names(df)), collapse = ", ")))
      }
      
      # Ensure factor variables match training data
      for(col in names(df)) {
        if(is.factor(SAFE_Mo$trainingData[[col]])) {
          df[[col]] <- factor(df[[col]], levels = levels(SAFE_Mo$trainingData[[col]]))
        }
      }
      
      df
    }, error = function(e) {
      showNotification(paste("Data loading error:", e$message), type = "error")
      NULL
    })
  })
  
  # Generate predictions
  predictions <- reactive({
    req(raw_data())
    tryCatch({
      pred_probs <- predict(SAFE_Mo, newdata = raw_data(), type = "prob")
      
      data.frame(
        ID = 1:nrow(raw_data()),
        Predicted_Risk = pred_probs$X1,
        Risk_Group = ifelse(pred_probs$X1 > cut_point, "X1 (High Risk)", "X2 (Low Risk)"),
        raw_data()
      )
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error")
      NULL
    })
  })
  
  # Display model info
  output$model_info <- renderPrint({
    cat("Model Information:\n")
    cat("Type:", class(SAFE_Mo)[1], "\n")
    cat("Features used:", length(setdiff(names(SAFE_Mo$trainingData), ".outcome")), "\n")
    cat("Risk cutoff:", round(cut_point, 3), "\n")
  })
  
  # Prediction results table
  output$results <- DT::renderDataTable({
    req(predictions())
    
    display_cols <- c("ID", "Predicted_Risk", "Risk_Group")
    if("OS_26DAY" %in% names(predictions())) {
      display_cols <- c(display_cols, "OS_26DAY")
    }
    
    DT::datatable(
      predictions()[, display_cols, drop = FALSE],
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>% 
      DT::formatRound("Predicted_Risk", digits = 3) %>%
      DT::formatStyle("Risk_Group",
                      backgroundColor = styleEqual(
                        c("X1 (High Risk)", "X2 (Low Risk)"), 
                        c("#FFB8A8", "#A6D4FF")
                      ),
                      fontWeight = "bold"
      )
  })
  
  # Risk distribution plot
  output$risk_plot <- renderPlot({
    req(predictions())
    
    ggplot(predictions(), aes(x = Predicted_Risk, fill = Risk_Group)) +
      geom_histogram(binwidth = 0.05, alpha = 0.7) +
      geom_vline(xintercept = cut_point, linetype = "dashed", color = "#D75615", size = 1) +
      labs(title = "Patient Risk Score Distribution",
           x = "Predicted Risk Score",
           y = "Number of Patients",
           fill = "Risk Group") +
      scale_fill_manual(values = c("#FFB8A8", "#A6D4FF")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
  
  # Cutoff information
  output$cutoff_info <- renderPrint({
    req(predictions())
    
    high_risk <- sum(predictions()$Predicted_Risk > cut_point, na.rm = TRUE)
    low_risk <- sum(predictions()$Predicted_Risk <= cut_point, na.rm = TRUE)
    
    cat("Risk Threshold Information:\n")
    cat("Current cutoff:", round(cut_point, 3), "\n")
    cat("High-risk patients:", high_risk, "\n")
    cat("Low-risk patients:", low_risk, "\n")
    cat("High-risk proportion:", round(high_risk / (high_risk + low_risk) * 100, 1), "%\n")
  })
}

shinyApp(ui, server)

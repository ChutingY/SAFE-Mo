### 0. Environment Setup -------------------------------------------------------
rm(list = ls())
# Load required packages
required_packages <- c(
  "lattice", "MASS", "nnet", "mice", "foreign",
  "ranger", "mlr3verse", "dplyr", "ggplot2",
  "forcats", "VIM", "caret", "pROC", "cutoff",
  "tibble", "tidyr", "caTools"
)
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}))

### 1. Data Preparation -------------------------------------------------------

# Load dataset
data <- read.csv("./nwicu/data.csv", header = TRUE, row.names = 1)
cat("Initial dataset dimensions:", dim(data), "\n")

# Filter cases with ≥80% complete data
complete_cases <- rowSums(!is.na(data)) >= ncol(data) * 0.8
MICE_PREPARE <- data[complete_cases, ]
cat("After completeness filtering:", dim(MICE_PREPARE), "\n")

### 2. Data Imputation --------------------------------------------------------

# Multiple Imputation using Random Forest
set.seed(123)
mice_model <- mice(
  MICE_PREPARE,
  method = "rf",      # Random forest imputation
  m = 5,              # Number of imputations
  maxit = 5,          # Number of iterations
  printFlag = FALSE   # Suppress printing
)

# Extract completed dataset
MICE <- complete(mice_model)
rownames(MICE) <- rownames(MICE_PREPARE) # Preserve original row names

# Handle remaining NAs by copying values from related columns
MICE$lactate_max <- MICE$lactate_min
MICE$ptt_max <- MICE$ptt_min

# Verify imputation
cat("\nRemaining missing values after imputation:\n")
print(colSums(is.na(MICE)))

### 3. Model Evaluation -------------------------------------------------------

# Load pre-trained models
load("./nwicu/ML结果_OS_26DAY.Rdata")

# Generate predictions
model_predictions <- lapply(models, predict, newdata = MICE, type = "prob")
model_scores <- lapply(model_predictions, function(x) x[, "X1"])

# Create prediction matrix
pred_matrix <- data.frame(model_scores)
rownames(pred_matrix) <- rownames(MICE)

# Calculate AUCs
auc_results <- as.data.frame(t(caTools::colAUC(pred_matrix, MICE$OS_26DAY)))
print("\nModel AUC Results:")
print(auc_results)

### 4. Model Performance Metrics ----------------------------------------------

# Extract best model (svmRadialSigma) predictions
best_predictions <- data.frame(
  ID = rownames(pred_matrix),
  SAFE_Mo = pred_matrix$svmRadialSigma,
  stringsAsFactors = FALSE
)

# Merge with outcomes
merged_data <- merge(best_predictions, 
                     MICE[, c("ID", "OS_26DAY")], 
                     by = "ID")

# Prepare final evaluation dataset
eval_data <- data.frame(
  OS_26DAY = factor(merged_data$OS_26DAY, 
                    levels = c(0, 1),
                    labels = c("Survived", "Deceased")),
  SAFE_Mo = merged_data$SAFE_Mo
)

### 5. Classification Performance ---------------------------------------------
# Determine optimal cutoff
optimal_cutoff <- cutoff::roc(eval_data$SAFE_Mo, eval_data$OS_26DAY)[3]
cat("\nOptimal cutoff point:", optimal_cutoff, "\n")

# Classify cases
eval_data$Predicted <- ifelse(
  eval_data$SAFE_Mo > optimal_cutoff,
  "Deceased", 
  "Survived"
)

# Generate confusion matrix
conf_matrix <- confusionMatrix(
  factor(eval_data$Predicted, levels = c("Deceased", "Survived")),
  eval_data$OS_26DAY
)

# Extract performance metrics
performance_metrics <- data.frame(
  Accuracy = conf_matrix$overall["Accuracy"],
  Sensitivity = conf_matrix$byClass["Sensitivity"],
  Specificity = conf_matrix$byClass["Specificity"],
  PPV = conf_matrix$byClass["Pos Pred Value"],
  NPV = conf_matrix$byClass["Neg Pred Value"],
  F1 = conf_matrix$byClass["F1"]
)

### 6. Visualization ----------------------------------------------------------
# 6.1 Confusion Matrix Visualization
conf_plot_data <- as.data.frame(conf_matrix$table)
conf_plot_data <- conf_plot_data %>%
  rename(Actual = Reference, Predicted = Prediction)

conf_plot <- ggplot(conf_plot_data, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste("n =", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "#2A77AC") +
  labs(title = "Confusion Matrix",
       x = "Actual Outcome",
       y = "Predicted Outcome") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("./nwicu/confusion_matrix.pdf", conf_plot, width = 6, height = 5)

# 6.2 Performance Metrics Bar Plot
metrics_long <- performance_metrics %>%
  gather(key = "Metric", value = "Value")

metrics_plot <- ggplot(metrics_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5) +
  scale_fill_manual(values = c("#2A77AC", "#78AB31", "#EDB11A", "#D55535", "#7E318A")) +
  labs(title = "Model Performance Metrics",
       y = "Score") +
  ylim(0, 1.1) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

ggsave("./nwicu/performance_metrics_plot.pdf", metrics_plot, width = 7, height = 5)

# 6.3 ROC Curve Visualization
roc_obj <- roc(
  response = as.numeric(eval_data$OS_26DAY == "Deceased"),
  predictor = eval_data$SAFE_Mo,
  levels = c(0, 1)
)

ci_auc <- ci.auc(roc_obj)
auc_label <- paste0("AUC = ", round(ci_auc[2], 3),
                    " (95% CI: ", round(ci_auc[1], 3), "-", 
                    round(ci_auc[3], 3), ")")

roc_plot <- ggroc(roc_obj, color = "#2A77AC", size = 1) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  labs(title = "Receiver Operating Characteristic Curve",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  annotate("text", x = 0.7, y = 0.3, label = auc_label) +
  theme_minimal()

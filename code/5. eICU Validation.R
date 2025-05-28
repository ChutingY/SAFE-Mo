### 0. Environment Configuration ----------------------------------------------
# Set working directory and clear environment
rm(list = ls())

# Load required packages with installation check
required_packages <- c(
  "lattice", "MASS", "nnet", "mice", "foreign",
  "ranger", "mlr3verse", "dplyr", "ggplot2",
  "forcats", "VIM", "ROCR", "pROC", "cutoff",
  "caret", "tidyr", "caTools"
)

invisible(sapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}))

### 1. Data Preparation -------------------------------------------------------
data <- read.csv("./eICU/data_cal.csv", header = TRUE, row.names = 1)
load("ML.Rdata")

# Filter cases with ≥80% complete data
MICE_PREPARE <- data[rowSums(!is.na(data)) >= ncol(data) * 0.8, ]

### 2. Data Preprocessing -----------------------------------------------------

# Outlier treatment using Tukey's method
data_3 <- MICE_PREPARE
for (i in 2:33) {
  boxplot_stats <- boxplot.stats(data_3[, i])
  data_3[data_3[, i] %in% boxplot_stats$out, i] <- NA
}

# Check for columns with single unique value
unique_counts <- apply(na.omit(data_3), 2, function(x) length(unique(x)))
if (any(unique_counts == 1)) {
  message("Removing columns with single unique value: ", 
          paste(names(which(unique_counts == 1)), collapse = ", "))
  data_3 <- data_3[, !unique_counts == 1]
}

### 3. Multiple Imputation Using MICE-RF --------------------------------------
set.seed(123)
mice_model <- mice(
  data_3,
  method = "rf",
  m = 50,
  maxit = 5,
  printFlag = FALSE
)
MICE_PREPARE <- complete(mice_model)
rownames(MICE_PREPARE) <- rownames(data_3) # Preserve original identifiers

### 4. Model Performance Evaluation ------------------------------------------
# Generate predictions
model_preds <- lapply(models, predict, newdata = MICE_PREPARE, type = "prob")
model_preds <- lapply(model_preds, function(x) x[, "X1"])
model_preds_df <- data.frame(model_preds)
rownames(model_preds_df) <- rownames(MICE_PREPARE)

# Calculate AUCs
auc_results <- as.data.frame(t(caTools::colAUC(model_preds_df, MICE_PREPARE$OS_26DAY)))

# Extract best model (SVM Radial) predictions
best_preds <- data.frame(
  ID = rownames(model_preds_df),
  SAFE_Mo = model_preds_df$svmRadialSigma,
  stringsAsFactors = FALSE
)

# Merge with outcomes
merged_data <- merge(best_preds, 
                     MICE_PREPARE[, c("ID", "OS_26DAY")], 
                     by = "ID")

cal_data <- data.frame(
  OS_26DAY = merged_data$OS_26DAY,
  SAFE_Mo = merged_data$SAFE_Mo
)

### 5. Classification Performance Metrics -------------------------------------
# Recode outcomes for consistency
cal_data$OS_26DAY <- ifelse(cal_data$OS_26DAY == 0, "Survived", "Deceased")

# Determine optimal cutoff
optimal_cutoff <- cutoff::roc(cal_data$SAFE_Mo, cal_data$OS_26DAY)[3]
cal_data$Predicted <- ifelse(cal_data$SAFE_Mo > optimal_cutoff, 
                             "Deceased", "Survived")

# Generate confusion matrix
conf_matrix <- confusionMatrix(
  factor(cal_data$Predicted, levels = c("Deceased", "Survived")),
  factor(cal_data$OS_26DAY, levels = c("Deceased", "Survived"))
)

# Extract performance metrics
metrics <- data.frame(
  Accuracy = conf_matrix$overall["Accuracy"],
  Sensitivity = conf_matrix$byClass["Sensitivity"],
  Specificity = conf_matrix$byClass["Specificity"],
  PPV = conf_matrix$byClass["Pos Pred Value"],
  NPV = conf_matrix$byClass["Neg Pred Value"],
  F1 = conf_matrix$byClass["F1"],
  AUC = auc_results["svmRadialSigma", ]
)

### 6. Visualization ---------------------------------------------------------

# 6.1 Confusion Matrix Heatmap
conf_df <- as.data.frame(conf_matrix$table)

conf_plot <- ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0("n = ", Freq)), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "#0072B2") +
  labs(title = "Confusion Matrix",
       x = "Actual Outcome", 
       y = "Predicted Outcome") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("./eICU/confusion_matrix.pdf", conf_plot, width = 6, height = 5)

# 6.2 Performance Metrics Barplot
plot_metrics <- metrics %>% 
  select(Sensitivity, Specificity, PPV, NPV) %>%
  gather(key = "Metric", value = "Value")

metric_plot <- ggplot(plot_metrics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Value)), 
            vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Model Performance Metrics",
       y = "Score") +
  ylim(0, 1.1) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none")

# 6.3 ROC Curve
roc_obj <- roc(response = as.numeric(cal_data$OS_26DAY == "Deceased"),
               predictor = cal_data$SAFE_Mo,
               levels = c(0, 1),
               direction = "<")
roc_df <- data.frame( FPR = 1 - roc_obj$specificities,TPR = roc_obj$sensitivities)

auc_label <- sprintf("AUC = %.3f (95%% CI: %.3f-%.3f)",auc(roc_obj),ci.auc(roc_obj)[1],ci.auc(roc_obj)[3])

roc_plot <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#D55E00", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Receiver Operating Characteristic Curve",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)",
       caption = auc_label) +
  annotate("text", x = 0.7, y = 0.2, label = auc_label, size = 5) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = 0.5))


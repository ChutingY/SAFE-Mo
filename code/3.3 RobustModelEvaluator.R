library(data.table)
library(caret)
library(boot)
methods <- c("vglmContRatio", "vglmCumulative", "bayesglm", "glmboost", "glm","xgbLinear", "xgbTree","glmnet","pda", "pda2", "rf", "RRF", "C5.0", "C5.0Tree","kernelpls", "pls", "simpls", "widekernelpls", "spls","nnet", "pcaNNet", "rpart", "ctree","svmLinear", "svmRadial", "svmRadialCost", "svmRadialSigma","LogitBoost","cforest", "fda","kknn", "knn", "lda", "lda2", "earth", "gcvEarth")

# Function to safely calculate metrics with error handling
safe_calculate_metrics <- function(data, indices, method_name) {
  tryCatch({
    sample_data <- data[indices, ]
    
    # Skip if method column is missing or has no variation
    if (!method_name %in% names(sample_data) || length(unique(sample_data[[method_name]])) < 2) {
      return(rep(NA, 8))
    }
    
    cut_point <- as.numeric(cutoff::roc(sample_data[[method_name]], sample_data$OS_26DAY)[3])
    
    sample_data[[paste0(method_name, "_group")]] <- ifelse(sample_data[[method_name]] > cut_point, "X1", "X2")
    
    # Check if predictions have variation
    if (length(unique(sample_data[[paste0(method_name, "_group")]])) < 2) {
      return(rep(NA, 8))
    }
    
    confusion_mn <- confusionMatrix(as.factor(sample_data[[paste0(method_name, "_group")]]), 
                                    as.factor(sample_data$OS_26DAY))
    
    return(c(
      Accuracy = confusion_mn$byClass["Balanced Accuracy"],
      Precision = confusion_mn$byClass["Precision"],
      Recall = confusion_mn$byClass["Recall"],
      F1_score = confusion_mn$byClass["F1"],
      Specificity = confusion_mn$byClass["Specificity"],
      Sensitivity = confusion_mn$byClass["Sensitivity"],
      NPV = confusion_mn$byClass["Neg Pred Value"],
      PPV = confusion_mn$byClass["Pos Pred Value"]
    ))
  }, error = function(e) {
    return(rep(NA, 8))
  })
}

# Main analysis with robust bootstrap
results <- data.table()
bootstrap_summary <- data.table()

for (method in methods) {
  cat("Processing method:", method, "\n")
  
  # Skip if method column doesn't exist
  if (!method %in% names(cal_test)) {
    cat("Skipping", method, "- column not found\n")
    next
  }
  
  # Calculate point estimates with error handling
  point_metrics <- tryCatch({
    cut_point <- as.numeric(cutoff::roc(cal_test[[method]], cal_test$OS_26DAY)[3])
    cal_test[[paste0(method, "_group")]] <- ifelse(cal_test[[method]] > cut_point, "X1", "X2")
    
    confusion_mn <- confusionMatrix(as.factor(cal_test[[paste0(method, "_group")]]), 
                                    as.factor(cal_test$OS_26DAY))
    
    data.frame(
      Method = method,
      Accuracy = confusion_mn$byClass["Balanced Accuracy"],
      Precision = confusion_mn$byClass["Precision"],
      Recall = confusion_mn$byClass["Recall"],
      F1_score = confusion_mn$byClass["F1"],
      Specificity = confusion_mn$byClass["Specificity"],
      Sensitivity = confusion_mn$byClass["Sensitivity"],
      NPV = confusion_mn$byClass["Neg Pred Value"],
      PPV = confusion_mn$byClass["Pos Pred Value"]
    )
  }, error = function(e) {
    data.frame(
      Method = method,
      Accuracy = NA,
      Precision = NA,
      Recall = NA,
      F1_score = NA,
      Specificity = NA,
      Sensitivity = NA,
      NPV = NA,
      PPV = NA
    )
  })
  
  # Perform bootstrap with error handling
  boot_results <- tryCatch({
    set.seed(123)
    boot(data = cal_test, 
         statistic = safe_calculate_metrics, 
         R = 100, 
         method_name = method)
  }, error = function(e) {
    NULL
  })
  
  # Process bootstrap results if available
  if (!is.null(boot_results) && !all(is.na(boot_results$t))) {
    boot_ci <- tryCatch({
      t(sapply(1:ncol(boot_results$t), function(i) {
        ci <- tryCatch({
          boot.ci(boot_results, index = i, type = "perc")$percent[4:5]
        }, error = function(e) {
          c(NA, NA)
        })
        if (is.null(ci)) c(NA, NA) else ci
      }))
    }, error = function(e) {
      matrix(NA, nrow = 8, ncol = 2)
    })
    
    colnames(boot_ci) <- c("CI_lower", "CI_upper")
    
    metric_names <- c("Accuracy", "Precision", "Recall", "F1_score", 
                      "Specificity", "Sensitivity", "NPV", "PPV")
    
    boot_summary <- data.frame(
      Method = method,
      Metric = metric_names,
      Point_Estimate = as.numeric(point_metrics[1, -1]),
      CI_lower = boot_ci[,1],
      CI_upper = boot_ci[,2]
    )
    
    bootstrap_summary <- rbind(bootstrap_summary, boot_summary)
  } else {
    cat("Bootstrap failed for method:", method, "\n")
    
    boot_summary <- data.frame(
      Method = method,
      Metric = c("Accuracy", "Precision", "Recall", "F1_score", "Specificity", "Sensitivity", "NPV", "PPV"),
      Point_Estimate = as.numeric(point_metrics[1, -1]),
      CI_lower = NA,
      CI_upper = NA
    )
    
    bootstrap_summary <- rbind(bootstrap_summary, boot_summary)
  }
  
  results <- rbind(results, point_metrics)
}


# Create formatted output
formatted_matrix <- dcast(bootstrap_summary, Method ~ Metric, value.var = c("Point_Estimate", "CI_lower", "CI_upper"))

# Combine point estimates with CIs for each metric
metrics <- c("Accuracy", "Precision", "Recall", "F1_score", "Specificity", "Sensitivity", "NPV", "PPV")

for(metric in metrics) {
  formatted_matrix[[metric]] <- sprintf("%.2f (%.2f-%.2f)", 
                                        formatted_matrix[[paste0("Point_Estimate_", metric)]],
                                        formatted_matrix[[paste0("CI_lower_", metric)]],
                                        formatted_matrix[[paste0("CI_upper_", metric)]])
}

# Select only the formatted columns
final_output <- formatted_matrix[, .SD, .SDcols = c("Method", metrics)]
final_output[, Accuracy_value := as.numeric(gsub("(.+) \\(.+\\)", "\\1", Accuracy))]
final_output <- final_output[order(-Accuracy_value), !"Accuracy_value"]
head(final_output)

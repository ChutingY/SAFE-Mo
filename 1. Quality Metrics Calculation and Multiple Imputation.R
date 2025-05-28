# Load required libraries
library(lattice)
library(MASS)
library(nnet)
library(mice)
library(foreign)
library(ranger)
library(mlr3verse)
library(dplyr)
library(ggplot2)
library(forcats)
library(VIM)
library(caret)
library(caretEnsemble)
library(caTools)
library(tidyr)

# Set working directory and load data
data <- read.csv("ready for ML.csv", header = TRUE, row.names = 1)


# Data preprocessing
data$OS_26DAY <- factor(data$OS_26DAY, levels = c(1, 2), labels = c("Death", "Alive"))
set.seed(123)

# Simple random sampling 
index <-  sort(sample(nrow(data), nrow(data)*.8))
train <-  data[index,];train$OS_26DAY <- as.factor(make.names(train$OS_26DAY))
test <-   data[-index,];test$OS_26DAY <- as.factor(make.names(test$OS_26DAY))

# Stratified sampling 
set.seed(123) 
trainIndex <- createDataPartition(data$OS_26DAY, p = 0.8, list = FALSE)  
train <- data[trainIndex, ]  
test <- data[-trainIndex, ]  

# Model training
control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,
  index = createResample(train$OS_26DAY, 10),
  sampling = "up",
  summaryFunction = twoClassSummary
)

model_list <- c(
  "vglmContRatio", "vglmCumulative", "bayesglm", "glmboost", "glm",
  "xgbLinear", "xgbTree", "glmnet", "pda", "pda2", "rf", "RRF",
  "C5.0", "C5.0Tree", "kernelpls", "pls", "simpls", "widekernelpls", "spls",
  "nnet", "pcaNNet", "rpart", "ctree",
  "svmLinear", "svmRadial", "svmRadialCost", "svmRadialSigma",
  "LogitBoost", "cforest", "fda", "kknn", "knn", "lda", "lda2", "earth", "gcvEarth"
)

models <- caretList(
  OS_26DAY ~ .,
  data = train,
  trControl = control,
  methodList = model_list
)

# Model evaluation
resamps <- resamples(models);summary(resamps)

# Prepare ROC data
long_format <- resamps$values %>%
  select(contains("ROC")) %>%
  gather(key = "Model", value = "ROC") %>%
  mutate(Model = gsub("~ROC", "", Model)) %>%
  arrange(ROC)

# Plot ROC performance
ggplot(long_format, aes(x = Model, y = ROC)) +
  geom_boxplot(aes(fill = Model)) +
  labs(title = "Model Performance Comparison",
       y = "AUC-ROC",
       x = "") +
  coord_flip() +
  scale_fill_manual(values = colorRampPalette(c("#2A77AC", "#0071C2", "#78AB31", "#EDB11A", "#D75615", "#D55535", "#7E318A"))(36)) +
  theme_bw() +
  theme(legend.position = "none")

# Variable importance for best model
best_model <- models[["svmRadialSigma"]]
feat_imp <- varImp(best_model, scale = FALSE)
feature_data <- data.frame(
  Feature = rownames(feat_imp$importance),
  Importance = feat_imp$importance$Overall
)

# Plot variable importance
ggplot(feature_data, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", aes(fill = Feature)) +
  coord_flip() +
  labs(title = "Feature Importance",
       x = "",
       y = "Importance Score") +
  scale_fill_manual(values = colorRampPalette(c("#2A77AC", "#0071C2", "#78AB31", "#EDB11A", "#D75615", "#D55535", "#7E318A"))(36)) +
  theme_bw() +
  theme(legend.position = "none")

# Model stacking
stack <- caretStack(
  models,
  method = "rf",
  metric = "ROC",
  trControl = trainControl(
    method = "cv",
    number = 10,
    savePredictions = "final",
    classProbs = TRUE,
    index = createResample(train$OS_26DAY, 10),
    summaryFunction = twoClassSummary
  )
)

# Performance metrics calculation
performance_metrics <- data.frame()
for (model in names(models)) {
  pred <- predict(models[[model]], newdata = test, type = "prob")
  cm <- confusionMatrix(pred$X1 > 0.5, test$OS_26DAY == "X1")
  
  metrics <- data.frame(
    Model = model,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    PPV = cm$byClass["Pos Pred Value"],
    NPV = cm$byClass["Neg Pred Value"],
    F1 = cm$byClass["F1"],
    AUC = roc(test$OS_26DAY, pred$X1)$auc
  )
  
  performance_metrics <- rbind(performance_metrics, metrics)
}

# ROC curve plotting
roc_data <- data.frame()
for (model in names(models)) {
  pred <- predict(models[[model]], newdata = test, type = "prob")
  roc_obj <- roc(test$OS_26DAY, pred$X1)
  
  temp_data <- data.frame(
    Model = model,
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    AUC = auc(roc_obj)
  )
  
  if (temp_data$AUC[1] >= 0.6) {
    roc_data <- rbind(roc_data, temp_data)
  }
}

ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "ROC Curves",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  scale_color_manual(values = colorRampPalette(c("#2A77AC", "#0071C2", "#78AB31", "#EDB11A", "#D75615", "#D55535", "#7E318A"))(36)) +
  theme_bw()






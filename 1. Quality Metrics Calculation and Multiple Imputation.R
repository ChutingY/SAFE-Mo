rm(list = ls())
library(lattice)
library(MASS)
library(nnet)
library(mice)
library(foreign)
library(ranger)
library(mlr3verse)
library(dplyr)
library(ggplot2)
library(dplyr)
library(forcats)
library(VIM)

data <- read.csv("data.csv",header = T,row.names = 1)

# 2.1 Selection based on missingness threshold
na_distribution <- colMeans(is.na(data))
selected_vars <- names(na_distribution[na_distribution <= 0.4])
data_filtered <- data[, selected_vars]
data_filtered <- data_filtered[rowSums(!is.na(data_filtered)) >= ncol(data_filtered)*0.2, ]

# 2.2 Mortality analysis
deceased <- subset(data_filtered, OS_initial == 1)
median_survival <- median(deceased$OS_time, na.rm = TRUE)
print(paste("Median survival time:", round(median_survival, 2), "hours"))

# Survival time distribution
ggplot(deceased, aes(x = OS_time)) + 
  geom_bar(fill = "#4d97cd") + 
  theme_minimal() + 
  labs(title = "Survival Time Distribution of Deceased Patients",
       x = "Time (hours)",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 200))

# 3.1 Outlier handling using Tukey's method
clin_vars <- data_filtered[, 6:83]
for (i in seq_along(clin_vars)) {
  outliers <- boxplot.stats(clin_vars[[i]])$out
  clin_vars[clin_vars[[i]] %in% outliers, i] <- NA
}

# 3.2 Categorical variable encoding
data_processed <- clin_vars %>%
  mutate(
    admission_type = recode(admission_type,  "EMER"=1, "SURGERY"=2, "URGENT"=3, "ELECTIVE"=4, "OBSERVATION"=5),
    insurance = recode(insurance,"Medicare"=1, "Private"=2, "Medicaid"=3, "Other"=4),
    marital_status = recode(marital_status, "MARRIED"=1, "SINGLE"=2, "DIVORCED"=3, "WIDOWED"=4),
    race = recode(race,"WHITE"=1, "BLACK"=2, "ASIAN"=3, "OTHER"=4),
    gender = recode(gender, "M"=1, "F"=2)
  )

# 3.3 Multiple imputation using Bayesian regression (norm)
set.seed(123)
imputation_model <- mice(data_processed,method = "rf",m = 50,maxit = 5,print = FALSE)

# Imputation diagnostic plot
densityplot(imputation_model)

# 3.4 Generate final imputed dataset
final_data <- complete(imputation_model) 

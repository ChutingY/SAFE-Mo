########################################################################################
# 0. chen_score #######################################################################
########################################################################################
data <- read.csv("data.csv",header = T,row.names = 1)

data$age_mean <- data$age 
data$map_mean <- (data$mbp_min + data$mbp_max)/2 
data$hb_mean <- (data$hemoglobin_min + data$hemoglobin_max)/2
data$ph_mean <- (data$ph_min + data$ph_max)/2
data$sodium_mean <- (data$sodium_min + data$sodium_max)/2 
data$spo2_mean <- (data$spo2_min + data$spo2_max)/2 
data$bicarbonate_mean <- (data$bicarbonate_min + data$bicarbonate_max)/2
data$pt_mean <- (data$pt_min + data$pt_max)/2

data$cerebrovascular_disease <- data$cerebrovascular_disease 
data$liver_disease <- ifelse(data$mild_liver_disease == 1 | data$severe_liver_disease == 1, 1, 0)
data$diabetes_mellitus <- ifelse(data$diabetes_without_cc == 1 | data$diabetes_with_cc == 1, 1, 0) 
data$malignant_tumors <- ifelse(data$malignant_cancer == 1 | data$metastatic_solid_tumor == 1, 1, 0) 

# CPM
data$CPM <- 0.025 * data$age_mean +
  0.343 * data$map_mean -
  0.0045 * data$hb_mean +
  0.149 * data$ph_mean +
  0.424 * data$sodium_mean -
  0.094 * data$spo2_mean -
  0.043 * data$bicarbonate_mean +
  0.018 * data$pt_mean +
  0.502 * data$cerebrovascular_disease +
  0.683 * data$liver_disease -
  0.333 * data$diabetes_mellitus +
  0.734 * data$malignant_tumors +
  7.113 
head(data$CPM)
write.csv(data,"CPM.csv")

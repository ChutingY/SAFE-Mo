rm(list = ls())
library(dplyr)  
library(tidyverse)  
library(forcats)  
library(foreign)  
library(ggplot2)  
library(lattice)  
library(ggsci)  
library(VIM)  
library(MASS)  
library(nnet)  
library(glmnet)  
library(e1071)  
library(ranger)  
library(mice)  
library(mlr3verse)  
library(FactoMineR)  
library(factoextra)  

data <- read.csv("DATA_FOR_ML.csv",header = T,row.names = 1)
# Univariate_analysis_wilcoxon_test ############################################
var_class <- c(
  'gender','admission_type','insurance','marital_status','race',
  "myocardial_infarct", "congestive_heart_failure", "peripheral_vascular_disease",
  "cerebrovascular_disease", "dementia", "chronic_pulmonary_disease",
  "rheumatic_disease", "peptic_ulcer_disease", "mild_liver_disease",
  "diabetes_without_cc", "diabetes_with_cc", "paraplegia", "renal_disease",
  "malignant_cancer", "severe_liver_disease", "metastatic_solid_tumor"
)

table <- CreateTableOne(vars = colnames(data)[1:103],
                        data = data,
                        factorVars = var_class,
                        strata = 'OS_26DAY',
                        addOverall = F)
result <- print(table, showAllLevels = T)

# Univariate_Multivariate_Regression ############################################
CreateTableOne(data = data)
dput(names(data)) 
myVars <- c('gender','admission_type','insurance','marital_status','race','age','heart_rate_min','heart_rate_max','heart_rate_mean','sbp_min','sbp_max','sbp_mean','dbp_min','dbp_max','dbp_mean','mbp_min','mbp_max','mbp_mean','resp_rate_min','resp_rate_max','resp_rate_mean','temperature_min','temperature_max','temperature_mean','spo2_min','spo2_max','spo2_mean','ph_min','ph_max','po2_min','po2_max','pco2_min','pco2_max','baseexcess_min','baseexcess_max','totalco2_min','totalco2_max','platelets_min','platelets_max','wbc_min','wbc_max','aniongap_min','aniongap_max','bun_min','bun_max','creatinine_min','creatinine_max','inr_min','inr_max','pt_min','pt_max','ptt_min','ptt_max','hematocrit_min','hematocrit_max','bicarbonate_min','bicarbonate_max','glucose_min','glucose_max','hemoglobin_min','hemoglobin_max','calcium_min','calcium_max','potassium_min','potassium_max','sodium_min','sodium_max','myocardial_infarct','congestive_heart_failure','peripheral_vascular_disease','cerebrovascular_disease','dementia','chronic_pulmonary_disease','rheumatic_disease','peptic_ulcer_disease','mild_liver_disease','diabetes_without_cc','diabetes_with_cc','paraplegia','renal_disease','malignant_cancer','severe_liver_disease','metastatic_solid_tumor','aids','GCS_motor','GCS_verbal','GCS_eyes','GCS_unable','Charlson','Apsiii','Sapsii','SOFA','GCS')
catVars <- c('gender','admission_type','insurance','marital_status','race',"myocardial_infarct", "congestive_heart_failure", "peripheral_vascular_disease", "cerebrovascular_disease", "dementia", "chronic_pulmonary_disease", "rheumatic_disease", "peptic_ulcer_disease", "mild_liver_disease", "diabetes_without_cc", "diabetes_with_cc", "paraplegia", "renal_disease", "malignant_cancer", "severe_liver_disease", "metastatic_solid_tumor", "aids")

tab4 <- CreateTableOne(vars = myVars, strata = "OS_26DAY" , data = data, factorVars = catVars, addOverall = TRUE)
tab4Mat <- print(tab4, nonnormal = "OS_26DAY" , exact = "extent", smd = TRUE, quote = FALSE,noSpaces = TRUE)

# 2.3 Multicollinearity ############################################
lm_model <- lm(OS_26DAY ~ gender+admission_type+insurance+marital_status+race+age+heart_rate_min+heart_rate_max+sbp_min+sbp_max+dbp_min+dbp_max+mbp_min+mbp_max+resp_rate_min+resp_rate_max+temperature_min+temperature_max+spo2_min+urineoutput+lactate_min+lactate_max+ph_min+ph_max+po2_min+po2_max+pco2_min+pco2_max+aado2_calc_min+aado2_calc_max+baseexcess_min+baseexcess_max+totalco2_min+totalco2_max+platelets_min+platelets_max+wbc_min+wbc_max+aniongap_min+aniongap_max+bun_min+bun_max+creatinine_min+creatinine_max+abs_basophils_min+abs_basophils_max+abs_eosinophils_min+abs_eosinophils_max+abs_lymphocytes_min+abs_lymphocytes_max+abs_monocytes_min+abs_monocytes_max+abs_neutrophils_min+abs_neutrophils_max+inr_min+inr_max+pt_min+pt_max+ptt_min+ptt_max+alt_min+alt_max+alp_min+alp_max+ast_min+ast_max+bilirubin_total_min+bilirubin_total_max+hematocrit_min+hematocrit_max+bicarbonate_min+bicarbonate_max+glucose_min+glucose_max+hemoglobin_min+hemoglobin_max+calcium_min+calcium_max+potassium_min+potassium_max+sodium_min+sodium_max+myocardial_infarct+congestive_heart_failure+peripheral_vascular_disease+cerebrovascular_disease+dementia+chronic_pulmonary_disease+rheumatic_disease+peptic_ulcer_disease+mild_liver_disease+diabetes_without_cc+diabetes_with_cc+paraplegia+renal_disease+malignant_cancer+severe_liver_disease+metastatic_solid_tumor+aids,
               data)
line <- as.data.frame(vif(lm_model))

data_for_XX <- data[,c('gender','admission_type','insurance','marital_status','race','age','heart_rate_min','heart_rate_max','sbp_min','sbp_max','dbp_min','dbp_max','mbp_min','mbp_max','resp_rate_min','resp_rate_max','temperature_min','temperature_max','spo2_min','urineoutput','lactate_min','lactate_max','ph_min','ph_max','po2_min','po2_max','pco2_min','pco2_max','aado2_calc_min','aado2_calc_max','baseexcess_min','baseexcess_max','totalco2_min','totalco2_max','platelets_min','platelets_max','wbc_min','wbc_max','aniongap_min','aniongap_max','bun_min','bun_max','creatinine_min','creatinine_max','abs_basophils_min','abs_basophils_max','abs_eosinophils_min','abs_eosinophils_max','abs_lymphocytes_min','abs_lymphocytes_max','abs_monocytes_min','abs_monocytes_max','abs_neutrophils_min','abs_neutrophils_max','inr_min','inr_max','pt_min','pt_max','ptt_min','ptt_max','alt_min','alt_max','alp_min','alp_max','ast_min','ast_max','bilirubin_total_min','bilirubin_total_max','hematocrit_min','hematocrit_max','bicarbonate_min','bicarbonate_max','glucose_min','glucose_max','hemoglobin_min','hemoglobin_max','calcium_min','calcium_max','potassium_min','potassium_max','sodium_min','sodium_max','myocardial_infarct','congestive_heart_failure','peripheral_vascular_disease','cerebrovascular_disease','dementia','chronic_pulmonary_disease','rheumatic_disease','peptic_ulcer_disease','mild_liver_disease','diabetes_without_cc','diabetes_with_cc','paraplegia','renal_disease','malignant_cancer','severe_liver_disease','metastatic_solid_tumor','aids')]
data_for_XX <- as.data.frame(lapply(data_for_XX, function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(as.numeric(as.factor(x)))
  }
}))
XX<-cor(data_for_XX)
kappa(XX,exact=TRUE)

# 2.4 LASSO Regression ############################################
x <- as.matrix(data[,1:98])
(y <- as.factor(ifelse(data$OS_26DAY == "2", 0,1)))
dim(x);class(x);class(y);head(y)
fit = glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)
x <- coef(fit) 
tmp <- as.data.frame(as.matrix(x)) 
tmp$coef <- row.names(tmp) 
tmp <- reshape::melt(tmp, id = "coef") 
tmp$variable <- as.numeric(gsub("s", "", tmp$variable)) 
tmp$coef <- gsub('_','-',tmp$coef) 
tmp$lambda <- fit$lambda[tmp$variable+1] 
tmp$norm <- apply(abs(x[-1,]), 2, sum)[tmp$variable+1] 

# Plot LASSO regression curve
ggplot(tmp,aes(log(lambda),value,color = coef)) + 
  geom_vline(xintercept = log(cvfit$lambda.min),size=0.8,color='grey60',alpha=0.8,linetype=2)+
  geom_line(size=1) + xlab("Lambda (log scale)") + ylab('Coefficients')+ 
  theme_bw(base_rect_size = 2)+ 
  scale_color_manual(values= colorRampPalette(c( "#2A77AC","#0071C2","#78AB31","#EDB11A","#D75615", "#D55535","#7E318A" ))(100))+ 
  scale_x_continuous(expand = c(0.01,0.01))+ scale_y_continuous(expand = c(0.01,0.01))+ 
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size=15,color='black'), 
        axis.text = element_text(size=12,color='black'), 
        legend.title = element_blank(), 
        legend.text = element_text(size=12,color='black'), 
        legend.position = 'none')+ 
  annotate('text',x = -4,y=12,label='Optimal Lambda = 0.0135',color='black')+ 
  guides(col=guide_legend(ncol = 2))

xx <- data.frame(lambda=cvfit[["lambda"]],cvm=cvfit[["cvm"]],cvsd=cvfit[["cvsd"]], cvup=cvfit[["cvup"]],cvlo=cvfit[["cvlo"]],nozezo=cvfit[["nzero"]]) 
xx$ll<- log(xx$lambda) 
xx$NZERO<- paste0(xx$nozezo,' vars')

# Generate 10-fold cross-validation plot for LASSO regression
ggplot(xx,aes(ll,cvm,color=NZERO))+ 
  geom_errorbar(aes(x=ll,ymin=cvlo,ymax=cvup), width=0.05,size=1)+ 
  geom_vline(xintercept = xx$ll[which.min(xx$cvm)],size=0.8,color='grey60',alpha=0.8,
             linetype=2)+ 
  geom_point(size=2)+ 
  xlab("Log Lambda")+ ylab('Partial Likelihood Deviance')+ 
  theme_bw(base_rect_size = 1.5)+ 
  scale_color_manual(values= colorRampPalette(c("#7E318A", "#2A77AC","#0071C2","#78AB31","#EDB11A","#D75615", "#D55535" ))(50))+ 
  scale_x_continuous(expand = c(0.02,0.02))+ 
  scale_y_continuous(expand = c(0.02,0.02))+ 
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size=15,color='black'), 
        axis.text = element_text(size=12,color='black'), 
        legend.title = element_blank(), 
        legend.text = element_text(size=12,color='black'), 
        legend.position = 'none')+ 
  annotate('text',x= -4,y=0.4,label='Optimal Lambda = 0.0135',color='black')+ 
  guides(col=guide_legend(ncol = 6))

# data save #######################################################
myCoefs <- coef(cvfit, s="lambda.min")
lasso_fea <- myCoefs@Dimnames[[1]][which(as.logical(myCoefs != 0))]
(lasso_fea <- lasso_fea[-1])

# Checking for Multicollinearity
data_lasso <- as.data.frame(data[,lasso_fea[2:33]])
data_numeric <- as.data.frame(lapply(data_lasso, function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(as.numeric(as.factor(x)))
  }
}))
XX<-cor(data_numeric)
kappa(XX,exact=TRUE)

data_ready <- cbind(OS_26DAY = data$OS_26DAY,data_numeric)

# 3. PCA ####################################
pca <- (data_ready[,2:33])
types1 <- factor(data_ready$OS_26DAY)
pca_plot <- PCA(pca, graph = FALSE)

fviz_pca_ind(pca_plot,
             geom.ind = "point",
             pointsize =3,
             pointshape = 21,
             fill.ind = types1,
             habillage= types1,
             star.plot = TRUE,
             mean.point = TRUE,
             palette = c("#4d97cd", "#db6968"),
             addEllipses = TRUE,
             ellipse.type = "convex",
             legend.title = "Groups",
             title="") +
  labs(title = "Scatter plot of the reduced dataset using PCA",
       x = "Principal Component 1", y = "Principal Component 2")+
  theme_bw() +
  theme(text=element_text(size=14,face="plain",color="black"),
        axis.title=element_text(size=16,face="plain",color="black"),
        axis.text = element_text(size=14,face="plain",color="black"),
        legend.title = element_text(size=16,face="plain",color="black"),
        legend.text = element_text(size=14,face="plain",color="black"),
        legend.background = element_blank(),
        legend.position=c(0.85,0.1)
  )









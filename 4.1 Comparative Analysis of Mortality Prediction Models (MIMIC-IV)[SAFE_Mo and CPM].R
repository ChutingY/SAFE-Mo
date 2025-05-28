getwd()
setwd("F:\\burnburnburn\\MIMIC\\8.0\\train")
rm(list = ls())
.libPaths("D://Rmua")
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
library(ROCR) 
library(pROC)
library(survival)
library(rms)
library(PredictABEL)
library(rmda)
library(ggDCA)
library(caret)

data <- read.csv("data_prepared.csv",header = T,row.names = 1)
table(data$OS_26DAY)
mydata<-na.omit(data)
# 1. Logistic analysis ####################################################################
mydata$OS_26DAY[mydata$OS_26DAY == 2] <- 0
fit<- glm(OS_26DAY~Charlson+Apsiii+Sapsii+SOFA+SAFE_Mo+CPM,family=binomial(link = "logit"),data = mydata)
autoReg(fit,uni=TRUE,threshold=0.05)
result<-autoReg(fit, uni=TRUE) %>% myft()
result
table2docx(result)

# 2. ROC ###############################################################################
models_name <- c("SAFE_Mo","CPM")
dfm <- data.frame()
for (model in models_name) {
  mydata$OS_26DAY <- as.factor(mydata$OS_26DAY)
  rocobj <- AUC::roc(as.numeric(mydata[[model]]), mydata$OS_26DAY) 
  x <- ci(mydata$OS_26DAY, mydata[[model]])
  aucx <- paste0("AUC=", round(x[2], 3), ",95%CI (", round(x[1], 3), " - ", round(x[3], 3), ")")
  
  pred <- prediction(as.numeric(mydata[[model]]), mydata$OS_26DAY)  
  perf <- performance(pred,"tpr","fpr") 
  m1 <- tibble(name = model, 
               TPR = unlist(perf@x.values), 
               FPR = unlist(perf@y.values), 
               AUC = aucx,
               label = paste0(model, ": ", aucx))
  if (round(x[2], 3) < 0) {next}
  dfm <- rbind(dfm, m1)
}

ggplot(dfm,aes(x = 1-FPR, y = TPR,color=label)) +
  geom_path(size =0.8) +
  labs(title= "ROC curve", x = "False Positive Rate (1-Specificity)", y = "True Positive Rate (Sensitivity)")+
  scale_color_manual(values = colorRampPalette(c("#0071C2", "#D75615", "#EDB11A", "#7E318A", "#78AB31","#2A77AC","#D55535"))(10))+                 
  geom_abline(lty = 2) +
  theme_minimal()+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5,  linetype="solid", colour ="black"))
ggsave(".\\4.comparation\\3. ROC_OS_26DAY.pdf",width = 10,height = 7)

# 3. DCA ###############################################################################
set.seed(123)
data <- read.csv(".\\4.comparation\\data_prepared.csv",header = T,row.names = 1)
head(data)
data$OS_26DAY[data$OS_26DAY == 2] <- 0
Charlson_curve <- decision_curve(OS_26DAY~SAFE_Mo,data = data,thresholds = seq(0, 0.4, by = 0.005), bootstraps = 10)

variables <- models_name
results <- list()
for (var in variables) {
  formula <- as.formula(paste("OS_26DAY ~", var))
  result <- decision_curve(formula, data = data,thresholds = seq(0, 1, by = 0.005), bootstraps = 10)
  results[[paste( var,"curve")]] <- result
}
plot_decision_curve(results[["SAFE_Mo curve"]],  curve.names = "SAFE_Mo curve",
                    cost.benefit.axis =FALSE,
                    #col= c('red','blue'),
                    confidence.intervals=FALSE,
                    standardize = FALSE)

plot_decision_curve(results, 
                    curve.names = c( "SAFE_Mo curve","CPM curve"), 
                    col = colorRampPalette(c("#0071C2", "#D75615", "#EDB11A", "#7E318A", "#78AB31","#2A77AC","#D55535"))(9), 
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE, #remove cost benefit axis
                    legend.position = "topright")

# 4. Calibration curve #################################################################
variables <- models_name
results <- list()
for (var in variables) {
  formula <- as.formula(paste("OS_26DAY ~", var))
  result <- lrm(formula=formula,data=data,x=TRUE,y=TRUE)
  cal <- calibrate(result,  method = "boot", B = 1000)# method设置抽样的方法为bootstrap，B设置bootstrap的次数为1000。
  results[[paste( var)]] <- cal
}

plot(1,type = "n",
     xlim = c(0,1),ylim = c(0,1),
     xaxs = "i",yaxs = "i",
     xlab = "Predicted Probability",ylab="Observed  Probability",
     legend =FALSE,subtitles = FALSE,
     cex=1.5,cex.axis=1.5,cex.lab=1.5)
abline(0,1,col="black",lty=2,lwd=2)
lines(results[["SAFE_Mo"]][,c("predy","calibrated.orig")],lty=1,lwd=2,col="#0071C2")
lines(results[["CPM"]][,c("predy","calibrated.orig")],lty=1,lwd=2,col="#D55535")
legend(0.01,0.98,c("SAFE_Mo's Calibration curve","CPM's Calibration curve"),
       col = colorRampPalette(c("#0071C2","#2A77AC" ,"#78AB31", "#7E318A", "#D75615", "#EDB11A","#D55535"))(2),
       lty = c(1,1,1,1,1,1,1,1,1),
       lwd = c(2,2,2,2,2,2,2,2,2),
       bty="n",cex=1)









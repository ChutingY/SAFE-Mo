invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}))

data <- read.csv("./4.comparation/data_prepared.csv", header = TRUE, row.names = 1)
mydata <- na.omit(data)

# Descriptive Analysis
allVars <- c("Charlson", "Apsiii", "Sapsii", "SOFA", "SAFE_Mo")
table1 <- scitb1(vars = allVars, strata = "OS_26DAY", data = mydata,atotest = TRUE, statistic = TRUE, Overall = TRUE)
write.csv(table1, "./4.comparation/2_group_comparisons.csv", row.names = FALSE)

# Logistic analysis
mydata$OS_26DAY[mydata$OS_26DAY == 2] <- 0
fit<- glm(OS_26DAY~Charlson+Apsiii+Sapsii+SOFA+ SAFE_Mo,
          family=binomial(link = "logit"),
          data = mydata)
library(autoReg)
autoReg(fit,uni=TRUE,threshold=0.05)
autoReg(fit)
result<-autoReg(fit, uni=TRUE) %>% myft()
result
library(rrtable)
table2docx(result) #Exported table as Report.docx

# ROC
models_name <- c("Charlson", "Apsiii", "Sapsii", "SOFA", "SAFE_Mo")
dfm <- data.frame()

for (model in models_name) {
  rocobj <- roc(response = mydata$OS_26DAY,
                predictor = mydata[[model]],
                levels = c(0, 1),
                direction = "<")
  
  ci_auc <- ci.auc(rocobj)
  aucx <- sprintf("AUC = %.2f (95%% CI: %.2f-%.2f)",ci_auc[2], ci_auc[1], ci_auc[3])
  
  # Skip models with AUC < 0.6
  if (ci_auc[2] < 0.6) next
  
  temp_df <- data.frame(
    name = model,
    FPR = 1 - rocobj$specificities,
    TPR = rocobj$sensitivities,
    AUC = aucx,
    label = paste0(model, ": ", aucx)
  )
  dfm <- rbind(dfm, temp_df)
}

# Publication-quality ROC plot
roc_plot <- ggplot(dfm, aes(x = FPR, y = TPR, color = label)) +
  geom_path(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(title = "Receiver Operating Characteristic Curves",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A","#984EA3", "#FF7F00", "#A65628")) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.7, 0.25),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "grey90")
  )

ggsave("./4.comparation/3_roc_curves.pdf", roc_plot,width = 8)

# Decision Curve Analysis
dca_results <- lapply(models_name, function(x) {
  decision_curve(as.formula(paste("OS_26DAY ~", x)),
                 data = mydata,
                 thresholds = seq(0, 0.4, by = 0.01),
                 bootstraps = 1000)
})
names(dca_results) <- models_name

# Publication DCA plot
pdf("./4.comparation/4_dca_analysis.pdf", width = 8, height = 6)
plot_decision_curve(dca_results,
                    curve.names = c("Charlson", "APACHE III", "SAPS II","SOFA", "SAFE-Mo"),
                    col = c("#E41A1C", "#377EB8", "#4DAF4A","#984EA3", "#FF7F00"),
                    confidence.intervals = FALSE,
                    cost.benefit.axis = FALSE,
                    legend.position = "topright",
                    standardize = FALSE)
dev.off()

# Calibration Analysis
# Fit calibration models
cal_models <- lapply(models_name, function(x) {
  model <- lrm(as.formula(paste("OS_26DAY ~", x)),
               data = mydata, x = TRUE, y = TRUE)
  calibrate(model, method = "boot", B = 1000)
})
names(cal_models) <- models_name

# Publication calibration plot
plot(0:1, 0:1, type = "n",
     xlab = "Predicted Probability", ylab = "Observed Probability",
     main = "Calibration Curves", cex.lab = 1.2)
abline(0, 1, col = "grey50", lty = 2)

colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
for (i in seq_along(cal_models)) {
  lines(cal_models[[i]][, "predy"], cal_models[[i]][, "calibrated.orig"],
        col = colors[i], lwd = 2)
}
legend("bottomright",legend = c("Charlson", "APACHE III", "SAPS II", "SOFA", "SAFE-Mo"),
       col = colors, lwd = 2, cex = 0.9, bty = "n")


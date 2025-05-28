# SAFE-Mo: Sepsis-associated ARDS Fatality Evaluation Model
## Overview
Sepsis-associated acute respiratory distress syndrome (ARDS) carries a high mortality rate. **SAFE-Mo** is a machine learning model developed to **predict early mortality in sepsis-associated ARDS patients**, enabling clinicians to identify high-risk cases for timely intervention and improved outcomes.

## Repository Contents
### SAFE-Mo Application (SAFE_Mo_APP/)

- Deployable web application for clinical use, featuring:
- Interactive patient risk visualization
- Real-time mortality probability calculation
- Model interpretability outputs (risk factors)

### Modeling Code

- Full pipeline for transparency and reproducibility:
- Data preprocessing scripts
- Feature selection (LASSO)
- Model training
- Validation (10-fold cross-validation, external cohort testing)

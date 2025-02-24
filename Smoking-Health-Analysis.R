library(dplyr)
library(MASS)
library(ggplot2)
library(pROC)
library(tidyr)
# 1000 samples have been selected from the dataset
data = read.csv('/Users/anzhenghao/Desktop/cleaned_train_dataset.csv')
# data cleaning 
data <- data %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE)))) # Replace missing value

# 1. the relationship between smoking status and cardiovascular health indicators
model_cardio <- glm(smoking ~ systolic + relaxation + Cholesterol + HDL + LDL,
                    data = data, family = binomial())
# Backward selection optimization model
model_cardio_opt <- stepAIC(model_cardio, direction = "backward")
# summary model
summary(model_cardio_opt)
# ANOVA table
anova(model_cardio_opt, test = "Chisq")
# Visualize the ROC curve and AUC values of the model 
predicted_probs_cardio <- predict(model_cardio_opt, type = "response")
roc_cardio <- roc(data$smoking, predicted_probs_cardio)
plot(roc_cardio, main = "ROC Curve for Cardiovascular Health Model")
auc_cardio <- auc(roc_cardio)
print(paste("AUC for Cardiovascular Health Model: ", auc_cardio))

# 2. the relationship between smoking status and metabolic health indicators
model_metabolic <- glm(smoking ~ fasting.blood.sugar + triglyceride,
                       data = data, family = binomial())
# Backward selection optimization model
model_metabolic_opt <- stepAIC(model_metabolic, direction = "backward")
# summary model
summary(model_metabolic_opt)
# ANOVA table
anova(model_metabolic_opt, test = "Chisq")
# Visualize the ROC curve and AUC values of the model 
predicted_probs_metabolic <- predict(model_metabolic_opt, type = "response")
roc_metabolic <- roc(data$smoking, predicted_probs_metabolic)
plot(roc_metabolic, main = "ROC Curve for Metabolic Health Model")
auc_metabolic <- auc(roc_metabolic)
print(paste("AUC for Metabolic Health Model: ", auc_metabolic))

# 3. the relationship between smoking status and blood  health indicators
model_hemoglobin <- glm(smoking ~ hemoglobin,
                        data = data, family = binomial())
# summary model
summary(model_hemoglobin)
# ANOVA table
anova(model_hemoglobin, test = "Chisq")
# Visualize the ROC curve and AUC values of the model 
predicted_probs_hemoglobin <- predict(model_hemoglobin, type = "response")
roc_hemoglobin <- roc(data$smoking, predicted_probs_hemoglobin)
plot(roc_hemoglobin, main = "ROC Curve for Boold Health Model")
auc_hemoglobin<- auc(roc_hemoglobin)
print(paste("AUC for Boold Health Model: ", auc_hemoglobin))





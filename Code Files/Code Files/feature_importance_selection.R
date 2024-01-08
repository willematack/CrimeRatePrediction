library(readr)
library(knitr)
library(dplyr)
library(caret)
library(glmnet)
library(randomForest)
library(glmnetUtils)
library(car)
install.packages("gplots")
library(gplots)
install.packages(c("ggplot2", "broom"))
library(ggplot2)
library(broom)
library(rlang)

set.seed(1333)


# Load/Clean Data ---------------------------------------------------------
filepath_X <- "C:/Users/wille/Documents/AppliedStats/Project/CLeanData/X.csv"
filepath_Y <- "C:/Users/wille/Documents/AppliedStats/Project/CLeanData/Y.csv"
X <- read.csv(filepath_X)
Y <- read.csv(filepath_Y)

# Standardize the data
X <- as.data.frame(scale(as.matrix(X)))

mean(target_violent)

# Code for Building Models and Producing Confidence Intervals ---------------------------------------------------------

target_violent <- Y$Total_Violent
target_n_violent <- Y$Non.Violent
model <- lm(target_violent ~ ., data=X)

coefficients <- coef(model)
ci <- confint(new_viol)
summary(model)


subset_coefficients <- c("TotalPctDiv", "PctSpeakEnglOnly", "PctNotHSGrad", "pctWSocSec", "PopDens")
custom_labels <- c("HS Grad Rate", "% Speak English Only", "% with Social Security", "Population Density", "Divorce Rate")
plot_data <- data.frame(
  variable = names(coefficients),
  coefficient = coefficients,
  lower_bound = ci[, 1],
  upper_bound = ci[, 2]
)
plot_data_subset <- plot_data[plot_data$variable %in% subset_coefficients, ]
# Plot the coefficients with confidence intervals as error bars
ggplot(plot_data_subset, aes(x = variable, y = coefficient)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.2,
    position = position_dodge(0.2),
    color = "blue"
  ) +
  labs(
    title = "Confidence Intervals of 5 Covariates with Highest t-statistics - Non-Violent Crime",
    x = "Variable",
    y = "Coefficient"
  ) +
  scale_x_discrete(labels = custom_labels) + 
  theme_minimal() +  # Set a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Inference - t-stats (Bootstraping) -----------------------------------------------------
# This code was used to generate t-values for different linear models
target_variables_list <- c("Non.Violent","Total_Violent")
  
# Initialize an empty list to store the results for each target variable
tstat_results_list <- list()

# Loop through each target variable
for (target_variable in target_variables_list) {
  # Subset the data for the current target variable
  target <- Y[[target_variable]]

  num_samples <- 1
  sample_size <- 2215  # Replace N with the desired sample size
  
  # Initialize a matrix to store feature importances
  importance_matrix <- matrix(NA, nrow = num_samples, ncol = ncol(X), 
                              dimnames = list(NULL, colnames(X)))
  
  # Perform sampling and model fitting
  for (i in 1:num_samples) {
    # Randomly sample indices
    sample_indices <- sample(1:nrow(X), size = sample_size, replace = TRUE)
    
    # Create the sample dataset
    sampled_X <- X[sample_indices, ,drop=FALSE]
    sampled_Y <- target[sample_indices]
    
    # Fit a linear model (replace with your modeling code)
    model <- lm(sampled_Y ~ ., data = sampled_X)
    
    # Extract variable importance (replace with the appropriate function for your model)
    feature_importance <- varImp(model)$Overall
    
    # Store the feature importances in the matrix
    importance_matrix[i, ] <- feature_importance
  }
  
  average_importance <- apply(importance_matrix, 2, mean, na.rm = TRUE)
  variance_importance <- apply(importance_matrix, 2, var, na.rm = TRUE)
  
  # Combine results into a data frame
  result_df <- data.frame(
    Mean_Importance = average_importance,
    Variance_Importance = variance_importance
  )

  # Sort the data frame by mean importance in descending order
  t_stat_results <- result_df[order(-result_df$Mean_Importance), ]
  tstat_results_list[[target_variable]] <- t_stat_results

}

summary(model)

coefs <- model$coefficients

# Inference - Random Forest (Bootstrapping) -----------------------------------------------
# This code was used to generate RF feature Importances for different models

rf_results_list <- list()

for (target_variable in target_variables_list) {
  
  cat(target_variable)
  target <- Y[[target_variable]]
  
  num_samples <- 100
  sample_size <- 2215  #
  
  # Initialize a matrix to store feature importances
  rf_importance_matrix <- matrix(NA, nrow = num_samples, ncol = ncol(X), 
                              dimnames = list(NULL, colnames(X)))
  
  # Perform sampling and model fitting
  for (i in 1:num_samples) {
    # Randomly sample indices
    sample_indices <- sample(1:nrow(X), size = sample_size, replace = TRUE)
    
    # Create the sample dataset
    sampled_X <- X[sample_indices, ,drop=FALSE]
    sampled_Y <- target[sample_indices]
    
    # Fit a linear model (replace with your modeling code)
    rf_model <- randomForest(sampled_Y ~ ., data = sampled_X, ntree = 100)
    
    
    # Extract variable importance (replace with the appropriate function for your model)
    rf_feature_importance <- varImp(rf_model)$Overall
    
    # Store the feature importances in the matrix
    rf_importance_matrix[i, ] <- rf_feature_importance
  }
  
  rf_average_importance <- apply(rf_importance_matrix, 2, mean, na.rm = TRUE)
  rf_variance_importance <- apply(rf_importance_matrix, 2, var, na.rm = TRUE)
  
  # Combine results into a data frame
  rf_result_df <- data.frame(
    Mean_Importance = rf_average_importance,
    Variance_Importance = rf_variance_importance
  )
  
  # Sort the data frame by mean importance in descending order
  rf_results <- rf_result_df[order(-rf_result_df$Mean_Importance), ]
  rf_results_list[[target_variable]] <- rf_results
}

saveRDS(rf_results_list, file = "C:/Users/wille/Documents/AppliedStats/Project/Vars/rf_results.rds")
x <- readRDS("C:/Users/wille/Documents/AppliedStats/Project/Vars/rf_results.rds")



# AIC - Violent Models ---------------------------------------------------------

AIC_model_violent <- step(model_baseline_violent, direction="backward")
summary(AIC_model_violent)

X_AIC <- model.frame(AIC_model)[,-1]

# Compare using F-Score to Baseline Model
anova(AIC_model, model_baseline_violent)

#The F-score is very low. Not losing any real information

# BIC - Violent Models---------------------------------------------------------
BIC_model <- step(model_baseline_violent, k=log(2215), direction="backward")
summary(BIC_model)
X_BIC <- model.frame(BIC_model)[,-1]

# Compare using F-Score to Baseline Model
anova(BIC_model, model_baseline_violent)

# Using BIC is a bit more aggressive and further reduces the number of parameters, though the p-value of the F-statistic still fails to reduce the null hypothesis
anova(BIC_model, AIC_model)

# a direct comparison between the BIC and AIC models shows we reject the null hypothesis


# LASSO - Violent Models -------------------------------------------------------
library(glmnet)
lasso_model <- cv.glmnet(as.matrix(X), target_violent, alpha = 1)

# Identify the optimal lambda
best_lambda <- lasso_model$lambda.min

# Refit LASSO model with the optimal lambda
lasso_model <- glmnet(as.matrix(X), target_violent, alpha = 1, lambda = best_lambda)

nonzero_coeffs <- coef(lasso_model)
nonzero_indices <- which(nonzero_coeffs != 0, arr.ind = TRUE)
selected_features_lasso <- rownames(nonzero_coeffs)[nonzero_indices[, "row"]][-1]
LASSO_variables <- as.data.frame(X[, selected_features_lasso, drop = FALSE])

# ANOVA between this model 
LASSO_VAR_model <- lm(target_violent~., LASSO_variables)
anova(LASSO_VAR_model, model_baseline_violent)
anova(LASSO_VAR_model, AIC_model)

# Conclusion - LASSO did not make this more clear.

# Co-Linearity Analysis - Violent Models ---------------------------------------------------
# continue with co-linearity - on AIC dataset

correlation_matrix <- cor(X_AIC)
print(correlation_matrix)

drop <- c("PctNotSpeakEnglWell")
X_no_corr <-  X_AIC[,!(names(X_AIC) %in% drop)]

new_model <- lm(target_violent~., data=X_no_corr)
anova(new_model, AIC_model)

library(corrplot)
corrplot(correlation_matrix, method = "color")

# Tested model when removing all features with corr over 0.8
# Removing any of these features results in a model which is significantly worse - excoet fir PctEnglNotWell

# IFeature Selection - Non-Violent -------------------------------------------------
# The above code is repeated for non-violent models
AIC_model_n_violent <- step(model_baseline_non_violent, direction="backward")
summary(AIC_model_n_violent)

X_AIC_n_violent <- model.frame(AIC_model_n_violent)[,-1]

# Compare using F-Score to Baseline Model
anova(AIC_model_n_violent, model_baseline_n_violent)

#The F-score is very low. Not losing any real information


BIC_model_n_violent <- step(model_baseline_n_violent, k=log(2215), direction="backward")
summary(BIC_model_n_violent)
X_BIC_n_violent <- model.frame(BIC_model_n_violent)[,-1]

# Compare using F-Score to Baseline Model
anova(BIC_model_n_violent, model_baseline_n_violent)

# Using BIC is a bit more aggressive and further reduces the number of parameters, though the p-value of the F-statistic still fails to reduce the null hypothesis
anova(BIC_model_n_violent, AIC_model_n_violent)

# a direct comparison between the BIC and AIC models shows we reject the null hypothesis




lasso_model_n_violent <- cv.glmnet(as.matrix(X), target_n_violent, alpha = 1)

# Identify the optimal lambda
best_lambda_n_violent <- lasso_model_n_violent$lambda.min

# Refit LASSO model with the optimal lambda
lasso_model_n_violent <- glmnet(as.matrix(X), target_n_violent, alpha = 1, lambda = best_lambda_n_violent)

nonzero_coeffs <- coef(lasso_model_n_violent)
nonzero_indices <- which(nonzero_coeffs != 0, arr.ind = TRUE)
selected_features_lasso_n_violent <- rownames(nonzero_coeffs)[nonzero_indices[, "row"]][-1]
LASSO_variables_n_violent <- as.data.frame(X[, selected_features_lasso_n_violent, drop = FALSE])

# ANOVA between this model 
LASSO_VAR_model_n_violent <- lm(target_n_violent~., LASSO_variables_n_violent)
anova(LASSO_VAR_model_n_violent, model_baseline_n_violent)
anova(AIC_model_n_violent, LASSO_VAR_model_n_violent)


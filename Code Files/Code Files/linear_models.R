## Prediction

library(readr)
library(knitr)
library(dplyr)
library(caret)
library(glmnet)
library(randomForest)
library(glmnetUtils)
library(car)
set.seed(1333)


# Load Data ---------------------------------------------------------
filepath_X <- "C:/Users/wille/Documents/AppliedStats/Project/CleanData/X.csv"
filepath_refined <- "C:/Users/wille/Documents/AppliedStats/Project/Vars/"
filepath_Y <- "C:/Users/wille/Documents/AppliedStats/Project/CleanData/Y.csv"
X_full <- read.csv(filepath_X)
X_full <- as.data.frame(scale(as.matrix(X_full)))
Y <- read.csv(filepath_Y)
X_violent <- read.csv(paste(filepath_refined, "X_refined_violent.csv", sep = ""), row.names=1)
X_n_violent <- read.csv(paste(filepath_refined, "X_refined_n_violent.csv", sep = ""), row.names=1)
target_violent <- Y$Total_Violent
target_n_violent <- Y$Non.Violent



# Testing Linear Regression -----------------------------------------------
# k-fold validation
k <- 10
folds <- sample(rep(1:k, length.out = 2215))

# Initialize a vector to store the cross-validation results
cv_results <- numeric(k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_full[train_indices, ]
  test_X <- X_full[test_indices, ]
  
  train_y <- target_violent[train_indices]
  test_y <- target_violent[test_indices]
  
  # Fit a linear regression model on the training data
  lm_model <- lm(train_y ~ ., data = train_X)
  
  # Predict on the test data
  predictions <- predict(lm_model, newdata = test_X)
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- sqrt(mse)
}
average_rmse_violent <- mean(cv_results)




# Now, with full data for non-violent

# Perform k-fold cross-validation
for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_full[train_indices, ]
  test_X <- X_full[test_indices, ]
  
  train_y <- target_n_violent[train_indices]
  test_y <- target_n_violent[test_indices]
  
  # Fit a linear regression model on the training data
  lm_model <- lm(train_y ~ ., data = train_X)
  
  # Predict on the test data
  predictions <- predict(lm_model, newdata = test_X)
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- sqrt(mse)
}

# Calculate the average performance across folds
average_rmse_n_violent <- mean(cv_results)





## Now, with refined sets

# Initialize a vector to store the cross-validation results
cv_results <- numeric(k)

# Perform k-fold cross-validation
for (i in 1:k) {
  print(i)
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_violent[train_indices, ]
  test_X <- X_violent[test_indices, ]
  
  train_y <- target_violent[train_indices]
  test_y <- target_violent[test_indices]
  
  # Fit a linear regression model on the training data
  lm_model <- lm(train_y ~ ., data = train_X)
  
  # Predict on the test data
  predictions <- predict(lm_model, newdata = test_X)
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- sqrt(mse)
}
average_rmse_violent_ref <- mean(cv_results)


# Perform k-fold cross-validation
for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_n_violent[train_indices, ]
  test_X <- X_n_violent[test_indices, ]
  
  train_y <- target_n_violent[train_indices]
  test_y <- target_n_violent[test_indices]
  
  # Fit a linear regression model on the training data
  lm_model <- lm(train_y ~ ., data = train_X)
  
  # Predict on the test data
  predictions <- predict(lm_model, newdata = test_X)
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- sqrt(mse)
}

# Calculate the average performance across folds
average_rmse_n_violent_ref <- mean(cv_results)






# LASSO -------------------------------------------------------------------

for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_full[train_indices, ]
  test_X <- X_full[test_indices, ]
  
  train_y <- target_violent[train_indices]
  test_y <- target_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = 1, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = 1)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
lasso_average_rmse_fv <- mean(sqrt(cv_results))



for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_full[train_indices, ]
  test_X <- X_full[test_indices, ]
  
  train_y <- target_n_violent[train_indices]
  test_y <- target_n_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = 1, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = 1)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
lasso_average_rmse_fnv <- mean(sqrt(cv_results))



## Cleaned data
for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_violent[train_indices, ]
  test_X <- X_violent[test_indices, ]
  
  train_y <- target_violent[train_indices]
  test_y <- target_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = 1, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = 1)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
lasso_average_rmse_rv <- mean(sqrt(cv_results))




for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_n_violent[train_indices, ]
  test_X <- X_n_violent[test_indices, ]
  
  train_y <- target_n_violent[train_indices]
  test_y <- target_n_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = 1, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = 1)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
lasso_average_rmse_fnv <- mean(sqrt(cv_results))





                           




# Ridge Regression --------------------------------------------------------
a<-0

for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_full[train_indices, ]
  test_X <- X_full[test_indices, ]
  
  train_y <- target_violent[train_indices]
  test_y <- target_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = a, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = a)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
rr_average_rmse_fv <- mean(sqrt(cv_results))



for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_full[train_indices, ]
  test_X <- X_full[test_indices, ]
  
  train_y <- target_n_violent[train_indices]
  test_y <- target_n_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = a, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = a)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
rr_average_rmse_fnv <- mean(sqrt(cv_results))



## Cleaned data
for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_violent[train_indices, ]
  test_X <- X_violent[test_indices, ]
  
  train_y <- target_violent[train_indices]
  test_y <- target_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = a, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = a)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
rr_average_rmse_rv <- mean(sqrt(cv_results))




for (i in 1:k) {
  # Split the data into training and testing sets
  train_indices <- folds != i
  test_indices <- folds == i
  
  train_X <- X_n_violent[train_indices, ]
  test_X <- X_n_violent[test_indices, ]
  
  train_y <- target_n_violent[train_indices]
  test_y <- target_n_violent[test_indices]
  
  # Fit a LASSO regression model on the training data
  cv_model <- cv.glmnet(x = as.matrix(train_X), y = train_y, alpha = a, nfolds=k)  # alpha = 1 for LASSO
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(x = as.matrix(train_X), y = train_y, lambda=best_lambda, alpha = a)  # alpha = 1 for LASSO
  
  predictions <- predict(lasso_model, newx = as.matrix(test_X))
  
  # Calculate the mean squared error (you can use other metrics as needed)
  mse <- mean((test_y - predictions)^2)
  
  # Store the result
  cv_results[i] <- mse
}

# Calculate the average performance across folds
rr_average_rmse_rnv <- mean(sqrt(cv_results))


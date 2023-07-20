# data preprocessing
source("src/data_preprocessing.R")
library(xgboost)
library(Metrics)

set.seed(2023)

# convert training data to DMatrix
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train$TARGET)

# cross-validation
params_xgb <- list(max_depth = 2, eta = 0.01, nthread = 1, 
                   objective = "reg:squarederror")
cv <- xgb.cv(params = params_xgb, data = dtrain, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 50)

# train model
model_ <- xgb.train(params = params_xgb, data = dtrain, nrounds = cv$best_iteration)

# predictions
y_hat_train <- predict(model_, as.matrix(X_train))
y_hat_test <- predict(model_, as.matrix(X_test))

# RMSE
rmse_train_FR <- rmse(y_train[mask_train_FR, "TARGET"], y_hat_train[mask_train_FR]) %>% round(., 3)
rmse_train_DE <- rmse(y_train[!mask_train_FR, "TARGET"], y_hat_train[!mask_train_FR]) %>% round(., 3)
rmse_train <- rmse(y_train$TARGET, y_hat_train) %>% round(., 3)
print(paste("RMSE on training set for FR: ", rmse_train_FR))
print(paste("RMSE on training set for DE: ", rmse_train_DE))
print(paste("RMSE on training set: ", rmse_train))

# Spearman correlation for the predictions on the training set
cor_train_FR <- cor(y_train[mask_train_FR, "TARGET"], y_hat_train[mask_train_FR], method = "spearman") %>% round(., 3)
cor_train_DE <- cor(y_train[!mask_train_FR, "TARGET"], y_hat_train[!mask_train_FR], method = "spearman") %>% round(., 3)
cor_train <- cor(y_train$TARGET, y_hat_train, method = "spearman") %>% round(., 3)
print(paste("Spearman corr on training set for FR: ", cor_train_FR))
print(paste("Spearman corr on training set for DE: ", cor_train_DE))
print(paste("Spearman corr on training set: ", cor_train))

# Feature importance
importance_matrix <- xgb.importance(model = model_)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# export predictions on the test set
to_export <- tibble(ID = X_test_raw$ID, TARGET = y_hat_test)
# set EOL to CRLF to avoid conflicts with git
write_csv(to_export, "./data/y_hat_test.csv", eol = "\r\n")

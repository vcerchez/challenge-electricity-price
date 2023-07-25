# data preprocessing
source("src/data_preprocessing.R")
library(xgboost)
library(Metrics)
library(ggplot2)
library(Ckmeans.1d.dp)

set.seed(2023)

# convert training data to DMatrix
dtrain_FR <- xgb.DMatrix(data = as.matrix(X_train[mask_train_FR, ]), label = y_train[mask_train_FR, "TARGET"])
dtrain_DE <- xgb.DMatrix(data = as.matrix(X_train[!mask_train_FR, ]), label = y_train[!mask_train_FR, "TARGET"])

# cross-validation
params_xgb <- list(max_depth = 2, eta = 0.01, nthread = 1, min_child_weight = 10,
                   objective = "reg:squarederror")
cv_FR <- xgb.cv(params = params_xgb, data = dtrain_FR, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 50)
cv_DE <- xgb.cv(params = params_xgb, data = dtrain_DE, nrounds = 1000, nfold = 5, print_every_n = 10, early_stopping_rounds = 50)

# train model
model_FR <- xgb.train(params = params_xgb, data = dtrain_FR, nrounds = cv_FR$best_iteration)
model_DE <- xgb.train(params = params_xgb, data = dtrain_DE, nrounds = cv_DE$best_iteration)

# predictions
y_hat_train <- vector(mode="numeric", length=nrow(X_train_raw))
y_hat_test <- vector(mode="numeric", length=nrow(X_test_raw))

y_hat_train[mask_train_FR] <- predict(model_FR, as.matrix(X_train[mask_train_FR, ]))
y_hat_train[!mask_train_FR] <- predict(model_DE, as.matrix(X_train[!mask_train_FR, ]))

y_hat_test[mask_test_FR] <- predict(model_FR, as.matrix(X_test[mask_test_FR, ]))
y_hat_test[!mask_test_FR] <- predict(model_DE, as.matrix(X_test[!mask_test_FR, ]))

# y_hat_train <- predict(model_, as.matrix(X_train))
# y_hat_test <- predict(model_, as.matrix(X_test))

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
print_importance_matrix <- function(model_obj, plot_title){
  importance_matrix <- xgb.importance(model = model_obj)
  importance_matrix$gain_freq <- importance_matrix$Gain * importance_matrix$Frequency
  importance_matrix$gain_freq <- 100 * importance_matrix$gain_freq / sum(importance_matrix$gain_freq)
  importance_matrix$cum_gain_freq <- cumsum(importance_matrix$gain_freq)
  print(importance_matrix)
  plot_ <- xgb.ggplot.importance(
    importance_matrix = importance_matrix, 
    measure = "gain_freq", 
    rel_to_first = FALSE, 
    n_clusters = c(1, 3))
  plot_ + labs(y = "GAIN * FREQUENCY, normalized to 100", title = plot_title)
}

print_importance_matrix(model_FR, "France")
print_importance_matrix(model_DE, "Germany")

# TODO: feature interactions analysis with EIX(+++) or xgbfi

# export predictions on the test set
to_export <- tibble(ID = X_test_raw$ID, TARGET = y_hat_test)
# set EOL to CRLF to avoid conflicts with git
write_csv(to_export, "./data/y_hat_test.csv", eol = "\r\n")
